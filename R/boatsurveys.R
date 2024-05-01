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
