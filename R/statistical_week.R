#' Vectorized approach to calculating the statistical week, returns
#' an integer
#'
#' Statistical weeks start on mondays, so the first statistical week of the year starts
#' on the first monday of the year. (Contrast with management weeks which start on Sundays).
#'
#' @param date A vector of dates
#' @export
#' @examples
#' \dontrun{
#' data_frame |>
#'   mutate(stat_week = statistical_week(date_field))
#' }
#'
statistical_week <- function(date) {
  UseMethod("statistical_week")
}

#' @export
statistical_week.Date <- function(date) {
  statistical_week_(date)
}

#' @export
statistical_week.POSIXct <- function(date) {
  # convert posixct to date
  date <- as.Date(date)

  statistical_week_(date)
}

#' @export
statistical_week.character <- function(date) {
  # try a few common formats
  if (!anyNA(as.Date(date, "%Y-%m-%d"))) {
    date <- as.Date(date, "%Y-%m-%d")
  } else if (!anyNA(as.Date(date, "%m/%d/%Y"))) {
    date <- as.Date(date, "%m/%d/%Y")
  } else {
    cli::cli_abort("Date is in an ambiguous format")
  }

  statistical_week_(date)
}


statistical_week_ <- function(date) {
  dplyr::if_else(
    lubridate::wday(lubridate::floor_date(date, "year")) == 2 ## If the first day of the year is a monday
    ,
    as.integer(strftime(date, "%W")),
    as.integer(strftime(date, "%W")) + 1
  )
}
