#' @title Checks if data is current
#' @description Checks if data is current, if it is then updates, if not passes through
#' @param days The number of days to check with the last update of the materialized views
#' @examples
#' \dontrun{
#' check_freshness()
#' }
#' @export
check_freshness <- function(days = 3) {
  last_update <- pssp_last_update()

  intvl <- lubridate::interval(last_update, Sys.time())

  days_since_update <- intvl %/%
    lubridate::days()


  if (days_since_update >= days) {
    input <- readline(prompt = paste0(
      "It has been ", days_since_update, " days since this ",
      "data as been updated. Do you want to refresh? (y/n): "
    ))
    if (input == "y") {
      pssp_refresh_data()
    }
  }
}


#' @title Total length calculation for fork length
#' @description Adds StatWeek to Dataframe
#' @param .data Dataframe
#' @param size_class Adds size class column (sublegal and legal)
#' @param minimum_size_limit_inches Size limit (total length) of fishery in inches
#' @param fork_length_column Column name of fork lengths provided
#' @examples
#' \dontrun{
#' df |> calculate_total_length()
#' }
#'
#' @export
calculate_total_length <- function(.data, fork_length_column = "fork_length", size_class = TRUE, minimum_size_limit_inches = 22) {
  if (minimum_size_limit_inches != 22) {
    cli::cli_alert_info("Using a minimum size limit ({minimum_size_limit_inches}) that is not 22 inches")
  }

  fork_length <- rlang::enquo(fork_length_column)

  data <- .data |>
    dplyr::mutate(
      total_length = dplyr::case_when(
        !!fork_length < 68 ~ 1.023 + (1.045 * !!fork_length),
        !!fork_length >= 68 ~ 1.488 + (1.032 * !!fork_length)
      )
    )

  if (size_class) {
    data <- data |>
      dplyr::mutate(
        size_class = dplyr::case_when(
          .data$total_length >= (minimum_size_limit_inches * 2.54) ~ "Legal",
          total_length < (minimum_size_limit_inches * 2.54) ~ "Sublegal"
        )
      )
  }

  data
}

#' @title Add timestep columns to a dataframe
#' @description Adds a time step column to a dataframe
#' @param .data dataframe
#' @param species Species, either chinook or coho
#' @param date_column Column of the datetime
#' @examples
#' \dontrun{
#' df |> add_time_steps()
#' }
#'
#' @export
#'
add_time_steps <- function(.data, date_column = "survey_datetime", species = c("chinook", "coho")) {
  date_column <- rlang::enquo(date_column)

  species <- rlang::arg_match(species)

  if (species == "chinook") {
    .data |>
      dplyr::mutate(
        time_step = dplyr::case_when(
          lubridate::month(!!date_column) %in% c(10, 11, 12, 1, 2, 3, 4) ~ "1/4",
          lubridate::month(!!date_column) %in% c(5, 6) ~ "2",
          lubridate::month(!!date_column) %in% c(7, 8, 9) ~ "3"
        )
      ) |>
      dplyr::select(.data$time_step, dplyr::everything())
  } else {
    .data |>
      dplyr::mutate(
        time_step = dplyr::case_when(
          lubridate::month(!!date_column) %in% c(1, 2, 3, 4, 5, 6) ~ "1",
          lubridate::month(!!date_column) == 7 ~ "2",
          lubridate::month(!!date_column) == 8 ~ "3",
          lubridate::month(!!date_column) == 9 ~ "4",
          lubridate::month(!!date_column) %in% c(10, 11, 12) ~ "4",
        )
      ) |>
      dplyr::select(.data$time_step, dplyr::everything())
  }
}
