#' @title Pulls non-salmon related data
#' @description Queries dockside information from pssp_dev
#' @param catch_area Valid catch area code
#' @param start_date Start date of search
#' @param end_date End date of search
#' @param int_dates Returns year, month, day integer columns instead of a date field
#' @param fill_zero Fill NA's with zeros.
#' @param fresh Boolean. Ensures data is updated
#' @param check_fresh Boolean. Checks whether data is current
#' @family Dockside Creel Surveys
#' @examples
#' \dontrun{marine_pull <- marine_pull('10', '2021-11-01', '2021-12-04')}
#' \dontrun{marine_pull <- marine_pull(c('10', '09'), '2021-11-01', '2021-12-04')}
#'
#' @export
marine_pull <- function(catch_area, start_date, end_date, int_dates = FALSE,
                        fill_zero = FALSE, fresh = FALSE, check_fresh = TRUE){


  catch_area <- purrr::map_chr(catch_area, ~rlang::arg_match0(.x,
                       values = c('05', '06', '61',
                                      '62', '07', '81',
                                      '82', '09', '10', '26',
                                      '11', '12', '13'), arg_nm = 'catch_area'))


  catch_area <- paste0(catch_area, collapse = '\',\'')


  if(fresh){
    pssp_refresh_data()
    check_fresh = F
  }

  if (check_fresh){
    check_freshness()
  }

  query <- pssp_query(
    query =
      glue::glue(
        "select
              *
             from mvw_dockside_marine
             where
                  survey_datetime between '{start_date}' and '{end_date}' and
                  catch_area_code in('{catch_area}')"
      )
  )

  if (int_dates){
    query <- query |>
      dplyr::mutate(
        year = lubridate::year(.data$survey_datetime),
        month = lubridate::month(.data$survey_datetime),
        day = lubridate::day(.data$survey_datetime),
        .before = .data$survey_datetime
      ) |>
      dplyr::select(-.data$survey_datetime)
  }

  if (fill_zero){
    query[is.na(query)] <- 0
  }

  query |>
    tibble::as_tibble()


}
