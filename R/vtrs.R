#' @title Downloads VTR data from pssp
#' @description Queries RMIS for release infomation, defaults to tagged releases
#' @param catch_area Valid catch area code
#' @param start_date Start date of search
#' @param end_date End date of search
#' @param pretty Returns an easily readable dataframe
#' @param fresh Boolean. Ensures data is updated
#' @param check_fresh Boolean. Checks whether data is current
#' @return Dataframe
#' @family Volantary Trip Reports
#' @examples
#' \dontrun{vtr_data <- vtr_pull('10', '2021-11-01', '2021-12-04')}
#' @export
vtr_pull <- function(catch_area, start_date, end_date,
                     pretty = TRUE, fresh = FALSE, check_fresh = TRUE){


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

  if(fresh){
    pssp_refresh_data()
    check_fresh = FALSE
  }

  if (check_fresh){
    check_freshness()
  }


  query <- pssp_query(
    query =
      glue::glue(
        "select
              *
             from mvw_vtrs
             where
                  survey_datetime between '{start_date}' and '{end_date}' and
                  catch_area_code in ('{catch_area}')"
      )
  )


  df <- query |>
    tibble::as_tibble()


  if (pretty){
    df <- df |>
      dplyr::filter(.data$adipose_clip_status_code %in% c('AD', 'UM')) |>
      dplyr::group_by(.data$catch_area_code,
                      .data$fishing_method_short_description,
                      .data$legal_size_status_description,
                      .data$adipose_clip_status_code) |>
      dplyr::summarize(
        total_vtrs = dplyr::n_distinct(.data$survey_id),
        fish = sum(.data$fish_count),
        .groups = 'drop'
      ) |>
      dplyr::group_by(.data$catch_area_code, .data$fishing_method_short_description) |>
      dplyr::mutate(
        total_vtrs = sum(.data$total_vtrs)
      ) |>
      dplyr::ungroup() |>
      tidyr::unite('legal_status', .data$legal_size_status_description, .data$adipose_clip_status_code) |>
      tidyr::pivot_wider(names_from = .data$legal_status, values_from = .data$fish, values_fill = 0)
  }


  df |>
    janitor::clean_names()

}




