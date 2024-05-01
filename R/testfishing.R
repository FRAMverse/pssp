#' @title Downloads test fishing data from pssp
#' @description Queries pssp for test fishing encounters
#' @param catch_area Valid catch area code
#' @param start_date Start date of search
#' @param end_date End date of search
#' @param fresh Boolean. Ensures data is updated
#' @param check_fresh Boolean. Checks whether data is current
#' @param dsn DSN for the database connection. Default 'pssp_prod64'
#' @family Test Fishing
#' @examples
#' \dontrun{tf_data <- tf_pull('10', '2021-11-01', '2021-12-04')}
#' @export
tf_pull <-
  function(catch_area,
           start_date,
           end_date,
           fresh = F,
           check_fresh = T,
           dsn = 'pssp_prod64') {
    catch_area <- rlang::arg_match(catch_area,
                                   c(
                                     '05',
                                     '06',
                                     '61',
                                     '62',
                                     '07',
                                     '81',
                                     '82',
                                     '09',
                                     '10',
                                     '11',
                                     '12',
                                     '13'
                                   ))

    if (fresh) {
      pssp_refresh_data()
      check_fresh = F
    }

    if (check_fresh) {
      check_freshness()
    }

    con <- withr::local_db_connection(DBI::dbConnect(odbc::odbc(),
                                                     dsn,
                                                     timezone = "UTC",
                                                     bigint = "numeric"))
    query <- DBI::dbGetQuery(
      con,
      glue::glue(
        "select
              *
             from mvw_testfishing
             where
                  survey_datetime between '{start_date}' and '{end_date}' and
                  catch_area_code = '{catch_area}'"
      )
    )

    df <- query |>
      tibble::as_tibble()

    if ('length_measurement_centimeter' %in% colnames(df) &&
        'length_type_description' %in% colnames(df))  {
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


#' @title Summarize by Legal Mark category
#' @description Creates a summary data with counts by legal mark category
#' @param .data Dataframe
#' @param size_limit Size limit in inches to be used to find legality. Default 22 inchees
#' @family Test Fishing
#' @examples
#' \dontrun{tf_data |> tf_lm_summary()}
#'
#' @export
tf_lm_summary <- function(.data, size_limit = 22) {

  if(!is.numeric(size_limit)){cli::cli_abort('Size limit must be numeric')}

  size_limit <- size_limit * 2.54

  .data |>
    dplyr::filter(
      .data$common_name == 'Chinook Salmon',
      .data$catch_result_type_code %in% c('R', 'K'),
      .data$adipose_clip_status_code %in% c('AD', 'UM'),
      !is.na(.data$total_length)
    ) |>
    dplyr::mutate(
      legal = dplyr::if_else(.data$total_length >= size_limit, 'Legal', 'Sublegal')
    ) |>
    tidyr::unite('legal_mark', .data$legal, .data$adipose_clip_status_code) |>
    dplyr::count(.data$legal_mark) |>
    tidyr::pivot_wider(names_from = .data$legal_mark, values_from = .data$n)


}


