#' @title Refreseshs datasources
#' @description Refreshes all materialized views
#' @param dsn DSN connection default "pssp_prod64"
#' @examples
#' \dontrun{
#' pssp_refresh_data()
#' }
#' @export
pssp_refresh_data <- function(dsn = "pssp_prod64") {
  cli::cli_alert_info("Refreshing data, this can take a few minutes...")
  con <- withr::local_db_connection(DBI::dbConnect(odbc::odbc(),
    dsn,
    timezone = "UTC",
    bigint = "numeric"
  ))
  query <- DBI::dbGetQuery(
    con,
    "select refresh_mvw();"
  )
  cli::cli_alert_success("Data updated!")
}

#' @title Gets the last time materialized views we updated
#' @description Gets the last time materialized views were updated
#' @param dsn DSN connection default "pssp_prod64"
#' @examples
#' \dontrun{
#' pssp_last_update()
#' }
#' @export
pssp_last_update <- function(dsn = "pssp_prod64") {
  con <- withr::local_db_connection(DBI::dbConnect(odbc::odbc(),
    dsn,
    timezone = "UTC",
    bigint = "numeric"
  ))
  query <- DBI::dbGetQuery(
    con,
    "select max(last_update) from mvw_dockside_salmon"
  )

  lubridate::with_tz(query[[1]], "US/Pacific")
}


#' @title Queries the pssp database
#' @description Queries the pssp database and returns a tibble
#' @param query SQL Query
#' @param dsn DSN connection default "pssp_prod64"
#' @examples
#' \dontrun{
#' data <- pssp_query("SELECT * FROM survey LIMIT 10;")
#' }
#' @export
pssp_query <- function(query, dsn = "pssp_prod64") {
  if (!is.character(query)) {
    cli::cli_abort("Query must be a character string")
  }

  # dont want mutating commands coming through here
  if (any(stringr::str_detect(tolower(query), c("update", "delete", "insert")))) {
    cli::cli_abort("Mutating queries not permitted e.g. INSERT, UPDATE, DELETE")
  }


  con <- withr::local_db_connection(DBI::dbConnect(odbc::odbc(),
    dsn,
    timezone = "UTC",
    bigint = "numeric"
  ))
  DBI::dbGetQuery(
    con,
    query
  ) |>
    tibble::as_tibble()
}
