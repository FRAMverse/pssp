#' @title Downloads test fishing data from pssp
#' @description Queries RMIS for release infomation, defaults to tagged releases
#' @param catch_area Valid catch area code
#' @param start_date Start date of search
#' @param end_date End date of search
#' @param fresh Boolean. Ensures data is updated
#' @param check_fresh Boolean. Checks whether data is current
#' @param dsn DSN for the database connection. Default 'pssp_prod64'
#' @family Boat Effort Surveys
#' @examples
#' \dontrun{bs_data <- bs_pull('10', '2021-11-01', '2021-12-04')}
#' @export
bs_pull <-
  function(catch_area,
           start_date,
           end_date,
           fresh = FALSE,
           check_fresh = TRUE,
           dsn = "pssp_prod64") {
    catch_area <-
      purrr::map_chr(catch_area,
                     \(x) rlang::arg_match0(
                       x,
                       values = c(
                         '05','06','61',
                         '16','04','4B',
                         '62','07', '81',
                         '30', '15', '17',
                         '82', '09', '10',
                         '26', '28', '31',
                         '11', '12', '13'
                       ),
                       arg_nm = 'catch_area'
                     ))


    catch_area <- paste0(catch_area, collapse = '\',\'')

    if (fresh) {
      pssp_refresh_data()
      check_fresh <- F
    }

    if (check_fresh) {
      check_freshness()
    }

    con <- withr::local_db_connection(DBI::dbConnect(odbc::odbc(),
                                                     dsn,
                                                     timezone = "UTC",
                                                     bigint = "numeric"
    ))
    query <- DBI::dbGetQuery(
      con,
      glue::glue(
        "select
              *
             from mvw_boat_survey
             where
                  survey_datetime between '{start_date}' and '{end_date}' and
                  catch_area_code in ('{catch_area}');"
      )
    )

    df <- query |>
      tibble::as_tibble()

    if ("length_measurement_centimeter" %in% colnames(df) &&
        "length_type_description" %in% colnames(df)) {
      df <- df |>
        tidyr::pivot_wider(
          names_from = .data$length_type_description,
          values_from = .data$length_measurement_centimeter
        ) |>
        dplyr::select(-.data$individual_fish_id)
    }

    df |>
      janitor::clean_names()
  }


#' @title Calculates in-sample site weights
#' @description Calculates in-sample site weights
#' @param .data Dataframe
#' @family Boat Effort Surveys
#' @examples
#' \dontrun{bs_data |> bs_site_weights_in()}
#' @export
bs_site_weights_in <- function(.data){
  .data |>
    dplyr::mutate(
      day_type = dplyr::if_else(lubridate::wday(.data$survey_datetime) %in% c(6,7,1), 'WE', 'WD'),
      month = lubridate::month(.data$survey_datetime)
    ) |>
    identify_in_sample(location_code_column = .data$exit_location_code) |>
    dplyr::filter(
      .data$completion_status_description == 'Completed survey',
      .data$in_sample == TRUE
    ) |>
    dplyr::group_by(.data$month, .data$day_type, .data$exit_location_code,  .data$exit_location_name) |>
    dplyr::summarize(total_anglers = sum(.data$angler_count), .groups='drop') |>
    dplyr::group_by(.data$month, .data$day_type) |>
    dplyr::mutate(site_weight = .data$total_anglers / sum(.data$total_anglers)) |>
    dplyr::ungroup() |>
    dplyr::select(-.data$total_anglers) |>
    tidyr::pivot_wider(names_from = .data$day_type, values_from = .data$site_weight)


}



#' @title Calculates out-of-sample site weights
#' @description Calculates out-of-sample site weights
#' @param .data Dataframe
#' @family Boat Effort Surveys
#' @examples
#' \dontrun{bs_data |> bs_site_weights_out()}
#' @export
bs_site_weights_out <- function(.data){

  .data |>
    dplyr::mutate(
      day_type = dplyr::if_else(lubridate::wday(.data$survey_datetime) %in% c(6,7,1), 'WE', 'WD'),
      month = lubridate::month(.data$survey_datetime)
    ) |>
    identify_in_sample(location_code_column = .data$exit_location_code, named = TRUE) |>
    dplyr::filter(
      .data$completion_status_description == 'Completed survey'
    ) |>
    dplyr::group_by(.data$month, .data$day_type, .data$in_sample) |>
    dplyr::summarize(total_anglers = sum(.data$angler_count), .groups='drop') |>
    dplyr::group_by(.data$month, .data$day_type) |>
    tidyr::pivot_wider(names_from = .data$in_sample, values_from = .data$total_anglers) |>
    dplyr::mutate(
      in_sample_proportion = .data$in_sample / (.data$in_sample + .data$out_of_sample)
    )


}

#' @title Calculates site weights
#' @description Calculates site weights
#' @param .data Dataframe
#' @param sample_type Return in sample site weights or out of sample site weights
#' @family Boat Effort Surveys
#' @examples
#' \dontrun{bs_data |> bs_site_weights(sample_type = 'in_sample')}
#' \dontrun{bs_data |> bs_site_weights(sample_type = 'out_sample')}
#' @export
bs_site_weights <- function(.data, sample_type = c('in_sample', 'out_sample')){
  sample_type <- rlang::arg_match(sample_type)

  switch(
    sample_type,
    out_sample = bs_site_weights_out(.data),
    in_sample = bs_site_weights_in(.data)
  )

}


#' @title Effort map
#' @description Provides a heat map of angling effort
#' @param .data Dataframe
#' @param blur Inherited argument from leaflet
#' @param max Inherited argument from leaflet
#' @param radius Inherited argument from leaflet
#' @family Boat Effort Surveys
#' @examples
#' \dontrun{bs_data |> bs_map()}
#' @export
bs_map <- function(.data, blur = 25, max = .05, radius = 10) {
  .data |>
    leaflet::leaflet() |>
    leaflet::addTiles() |>
    leaflet.extras::addHeatmap(lat = ~.data$latitude_decimal_degrees_wgs84,
                               lng = ~.data$longitude_decimal_degrees_wgs84,
                               intensity = ~.data$angler_count,
                               blur = blur, max = max, radius = radius)
}
