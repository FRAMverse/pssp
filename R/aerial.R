#' @title Aerial data pull
#' @description Pulls the aerial data
#' @param catch_area Valid catch area code
#' @param start_date Start date of search
#' @param end_date End date of search
#' @param fresh Boolean. Ensures data is updated
#' @param check_fresh Boolean. Checks whether data is current
#' @family Aerial Effort Surveys
#' @examples
#' \dontrun{df <- aerial_pull('07', '2021-07-01', '2021-07-07')}
#' @export
aerial_pull <- function(catch_area, start_date, end_date, fresh = FALSE, check_fresh = TRUE){


  catch_area <- purrr::map_chr(catch_area, ~rlang::arg_match0(.x,
                     values = c('05', '06', '61',
                                    '62', '07', '81',
                                    '82', '09', '10',
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
             from mvw_aerial
             where
                  survey_datetime between '{start_date}' and '{end_date}' and
                  catch_area_code in('{catch_area}')"
      )
  )


  query |>
    tibble::as_tibble()
}


#' @title Aerial Estimator Table
#' @description Finds in-sample boats during the flight and calculates various numbers from it
#' @param catch_area Valid catch area code
#' @param start_date Start date of search
#' @param end_date End date of search
#' @family Aerial Effort Surveys
#' @examples
#' \dontrun{df <- aerial_estimator('07', '2021-07-01', '2021-07-07')}
#' @export
aerial_estimator <- function(catch_area, start_date, end_date){
  catch_area <- purrr::map_chr(catch_area, ~rlang::arg_match0(.x,
                       values = c('05', '06', '61',
                                      '62', '07', '81',
                                      '82', '09', '10',
                                      '11', '12', '13'), arg_nm = 'catch_area'))


  catch_area <- paste0(catch_area, collapse = '\',\'')

  query <- pssp_query(query =
                           glue::glue("select
              *
             from mvw_aerial_dockside_encounters
             where
                  survey_datetime between '{start_date}' and '{end_date}' and
                  catch_area_code in('{catch_area}')")
  )


  query |>
    tibble::as_tibble() |>
    dplyr::mutate(
      sample_fraction = .data$active_boats / .data$total_boats_in_flight,
      total_boats = .data$all_boats / .data$sample_fraction
    )

}


#' @title Aerial Estimator Table
#' @description Calculaties the adjusted proportion and variance of the aerial estimator table
#' @param .data Dataframe
#' @param bad_flights Throws out flights where there were more boats sampled at the dock than see from the air
#' @family Aerial Effort Surveys
#' @examples
#' \dontrun{df <- aerial_estimator('07', '2021-07-01', '2021-07-07')
#' df |>
#'       aerial_estimator_stats()
#'       }
#' @export
aerial_estimator_stats <- function(.data, bad_flights = FALSE){



  if(bad_flights){
    cli::cli_alert_danger('Flights where there were more boats seen dockside than in the air are included in this calculation')
    #calcuate the rounded sample fraction/ adjusted proportion
    adjusted_proportion <- round(mean(.data$sample_fraction),4)

    #calcuate the rounded variance
    adjusted_variance <- round(sum((.data$sample_fraction - adjusted_proportion) ^ 2) /
                                 nrow(aerial_estimator),6)

  } else {

    bad_flights <- .data |>
      dplyr::filter(.data$sample_fraction > 1) |> nrow()
    .data <- .data |>
      dplyr::filter(.data$sample_fraction <= 1)

    if (bad_flights > 0) {
      cli::cli_alert_warning('There are {bad_flights} flight{?s} not being used in this calculation')
    }


    #calcuate the rounded sample fraction/ adjusted proportion
    adjusted_proportion <- round(mean(.data$sample_fraction),4)

    #calcuate the rounded variance
    adjusted_variance <- round(sum((.data$sample_fraction - adjusted_proportion) ^ 2) /
                                 nrow(.data),6)

  }

  cli::cli_text(cat(cli::col_blue("Adjusted Proportion: "),cli::col_red(adjusted_proportion)))
  cli::cli_text(cat(cli::col_yellow("Adjusted Variance: "),cli::col_red(adjusted_variance)))

}
