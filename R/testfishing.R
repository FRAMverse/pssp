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
#' \dontrun{
#' tf_data <- tf_pull("10", "2021-11-01", "2021-12-04")
#' }
#' @export
tf_pull <-
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
             from mvw_testfishing
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


#' @title Summarize by Legal Mark category
#' @description Creates a summary data with counts by legal mark category
#' @param .data Dataframe
#' @param size_limit Size limit in inches to be used to find legality. Default 22 inchees
#' @family Test Fishing
#' @examples
#' \dontrun{
#' tf_data |> tf_lm_summary()
#' }
#'
#' @export
tf_lm_summary <- function(.data, size_limit = 22) {
  if (!is.numeric(size_limit)) {
    cli::cli_abort("Size limit must be numeric")
  }

  size_limit <- size_limit * 2.54

  .data |>
    dplyr::filter(
      .data$common_name == "Chinook Salmon",
      .data$catch_result_type_code %in% c("R", "K"),
      .data$adipose_clip_status_code %in% c("AD", "UM"),
      !is.na(.data$total_length)
    ) |>
    dplyr::mutate(
      legal = dplyr::if_else(.data$total_length >= size_limit, "Legal", "Sublegal")
    ) |>
    tidyr::unite("legal_mark", .data$legal, .data$adipose_clip_status_code) |>
    dplyr::count(.data$legal_mark) |>
    tidyr::pivot_wider(names_from = .data$legal_mark, values_from = .data$n)
}


#' @title Creates a ggplot scatter plot of test fishing lengths by adipose clip
#' @description Queries RMIS for release infomation, defaults to tagged releases
#' @param .data Dataframe
#' @param x_axis Variable for x axis of plot
#' @param y_axis Variable for y axis of plot
#' @param x_label Label of x axis
#' @param y_label Label of y axis
#' @param title Title of plot
#' @family Test Fishing
#'
#' @examples
#' \dontrun{
#' tf_data |> tf_scatter(survey_datetime, total_length)
#' }
#'
#' @export
tf_scatter <- function(.data, x_axis, y_axis, x_label = "Survey Date", y_label = "Total Length (CM)", title = "Chinook Test Fishing Lengths") {
  x_axis <- rlang::enquo(x_axis)
  y_axis <- rlang::enquo(y_axis)

  .data |>
    dplyr::filter(
      .data$common_name == "Chinook Salmon",
      .data$catch_result_type_code %in% c("R", "K"),
      .data$adipose_clip_status_code %in% c("AD", "UM"),
      !is.na(.data$total_length)
    ) |>
    ggplot2::ggplot(ggplot2::aes(!!x_axis, !!y_axis, color = .data$adipose_clip_status_code)) +
    ggplot2::labs(subtitle = title, x = x_label, y = y_label) +
    ggplot2::geom_point()
}


#' @title Test fishing length histogram
#' @description Identifies agency based on sampler name
#' @param .data Dataframe
#' @param x_axis Length column. total_length or fork_length
#' @param x_label Label of x axis
#' @param y_label Label of y axis
#' @param title Title of plot
#' @family Test Fishing
#' @examples
#' \dontrun{
#' tf_data |> tf_hist(total_length)
#' }
#'
#' @export
tf_hist <- function(.data,
                    x_axis,
                    x_label = "Total Length (CM)",
                    y_label = "Count",
                    title = "Chinook Test Fishing Lengths") {
  if (is.null(x_axis)) {
    cli::cli_abort("Missing x axis arguement")
  }

  x_axis <- rlang::enquo(x_axis)


  .data |>
    dplyr::filter(
      .data$common_name == "Chinook Salmon",
      .data$catch_result_type_code %in% c("R", "K"),
      .data$adipose_clip_status_code %in% c("AD", "UM"),
      !is.na(.data$total_length)
    ) |>
    ggplot2::ggplot(ggplot2::aes(!!x_axis, fill = .data$adipose_clip_status_code)) +
    ggplot2::labs(subtitle = title, x = x_label, y = y_label) +
    ggplot2::geom_histogram(binwidth = 2, alpha = .6)
}


#' @title Descriptive statistics of lengths
#' @description Mean, sum, and count of lengths by adipose clip
#' @param .data Dataframe
#' @param length_type total_length or fork_length
#' @family Test Fishing
#' @examples
#' \dontrun{
#' tf_data |> tf_descriptive_length(total_length)
#' }
#'
#' @export
tf_descriptive_length <- function(.data, length_type = "total_length") {
  length_type <- rlang::enquo(length_type)

  .data |>
    dplyr::filter(
      .data$common_name == "Chinook Salmon",
      .data$catch_result_type_code %in% c("R", "K"),
      .data$adipose_clip_status_code %in% c("AD", "UM"), !is.na(!!length_type)
    ) |>
    dplyr::group_by(
      .data$catch_area_code,
      .data$common_name,
      .data$adipose_clip_status_code
    ) |>
    dplyr::summarize(dplyr::across(
      !!length_type,
      list(
        mean = mean,
        sd = stats::sd,
        count = ~ sum(!is.na(.x))
      )
    ))
}

#' @title CV of Legal Marked Category
#' @description Coefficient of legal mark category
#' @param .data Dataframe
#' @param category Legal mark category (LM Default)
#' @family Test Fishing
#' @examples
#' \dontrun{
#' tf_data |> tf_cv(Legal_AD)
#' }
#'
#' @export
tf_cv <- function(.data, category = "Legal_AD") {
  category <- rlang::enquo(category)
  summary <- tf_lm_summary(.data)

  total <- summary |>
    sum()

  legal_ad <- summary |>
    dplyr::pull(!!category)

  proportion <- legal_ad / total

  cv <- sqrt(((1 - (proportion)) * (proportion)) / (total - 1)) / (proportion)
  tibble::tibble(
    cv = cv,
    proportion = proportion,
    legal_ad = legal_ad,
    sample_size = total,
    sample_size_needed = ceiling(((25 * (1 - proportion)) / proportion) + 1),
    sample_size_obtained = .data$sample_size / .data$sample_size_needed
  )
}

#' @title Iteragency summary
#' @description Summary of effort and encounters by agency
#' @param .data Dataframe
#' @family Test Fishing
#' @examples
#' \dontrun{
#' tf_data |> tf_interagency_summary()
#' }
#'
#' @export
tf_interagency_summary <- function(.data) {
  hr <- .data |>
    dplyr::select(.data$survey_datetime, .data$start_datetime, .data$end_datetime, .data$agency) |>
    unique() |>
    dplyr::mutate(
      hours_sampled = lubridate::time_length(lubridate::interval(.data$start_datetime, .data$end_datetime), "hours")
    ) |>
    dplyr::group_by(.data$agency) |>
    dplyr::summarise(
      total_trips = dplyr::n(),
      mean_hours = mean(.data$hours_sampled, na.rm = T),
      total_hours = sum(.data$hours_sampled, na.rm = T)
    )



  bio <- .data |>
    dplyr::filter(
      .data$common_name == "Chinook Salmon",
      .data$catch_result_type_code %in% c("R", "K")
    ) |>
    dplyr::group_by(.data$agency) |>
    dplyr::summarize(
      chinook_encounters = dplyr::n(),
      total_length_cm_mean = mean(.data$total_length, na.rm = T),
      total_length_cm_sd = stats::sd(.data$total_length, na.rm = T)
    )

  hr |>
    dplyr::left_join(bio, by = "agency") |>
    dplyr::mutate(
      chinook_per_hour = .data$chinook_encounters / .data$total_hours
    )
}

#' @title Mortality ratio
#' @description Produces a graph
#' @param .data Dataframe
#' @family Test Fishing
#' @examples
#' \dontrun{
#' tf_data |> tf_interagency_lm_summary()
#' }
#'
#' @export
tf_mortalility <- function(.data) {
  cli::cli_alert_info("If the point is on the LEFT of the line, the fishery is\
                      incurring more mortalies through release than retention")
  summary <- tf_lm_summary(.data)


  graph <- tibble::tibble(
    lm = seq(.01, 1, .001),
    sublegal = ((.data$lm * .87) - .1) / .1
  ) |>
    dplyr::filter(.data$sublegal >= 0, .data$sublegal <= 1)

  lem <- summary$Legal_AD / sum(summary, na.rm = TRUE)
  sul <- sum(summary$Sublegal_AD, summary$Sublegal_UM, na.rm = TRUE) / sum(summary, na.rm = TRUE)

  tibble::tibble(
    sl = sul,
    lm = lem
  ) |>
    ggplot2::ggplot(ggplot2::aes(.data$lm, .data$sl)) +
    ggplot2::geom_point() +
    ggplot2::geom_line(data = graph, ggplot2::aes(.data$lm, .data$sublegal)) +
    ggplot2::ylim(0, 1) +
    ggplot2::labs(y = "Sublegal Percentage", x = "Legal-Mark Percentage")
}


#' @title Iteragency legal mark summary
#' @description Summary of encounters by agency and legal mark category
#' @param .data Dataframe
#' @param percent Boolean. Results will be displayed in terms of percentage
#' @family Test Fishing
#' @examples
#' \dontrun{tf_data |> tf_interagency_lm_summary()}
#'
#' @export
tf_interagency_lm_summary <- function(.data, percent = FALSE) {

  summary <- .data |>
    dplyr::filter(
      .data$common_name == 'Chinook Salmon',
      .data$catch_result_type_code %in% c('R', 'K'),
      .data$adipose_clip_status_code %in% c('AD', 'UM'),
      !is.na(.data$total_length)
    ) |>
    dplyr::select(
      .data$agency,
      .data$catch_result_type_code,
      .data$common_name,
      .data$adipose_clip_status_code,
      .data$total_length
    ) |>
    dplyr::group_by(.data$agency) |>
    tidyr::nest() |>
    dplyr::mutate(lm = purrr::map(.data$data, tf_lm_summary)) |>
    dplyr::select(-.data$data) |>
    dplyr::ungroup() |>
    tidyr::unnest(.data$lm) |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), \(x) tidyr::replace_na(x, 0))
      )

  if(percent) {
    summary |>
      dplyr::mutate(
        dplyr::across(
          dplyr::where(is.numeric),
          \(x) x / (.data$Legal_AD + .data$Legal_UM +
                      .data$Sublegal_AD + .data$Sublegal_UM)
          )
        )
  } else {
    summary
  }

}


#' @title Interagency Fisher / Chi Square Test
#' @description Fisher / Chi Square test between agencies and legal mark counts
#' @param .data Dataframe
#' @param significance Significance threshold default .05
#' @family Test Fishing
#' @examples
#' \dontrun{tf_data |> tf_interagency_chisq()}
#'
#' @export
tf_interagency_chisq <- function(.data, significance = 0.05) {
  summary <- tf_interagency_lm_summary(.data) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) tidyr::replace_na(x, 0)))


  chi <- broom::tidy(stats::chisq.test(summary[, -1])) |>
    dplyr::mutate(significant_difference = dplyr::if_else(.data$p.value >= .env$significance, FALSE, TRUE))

  fisher <- broom::tidy(stats::fisher.test(summary[, -1])) |>
    dplyr::mutate(significant_difference = dplyr::if_else(.data$p.value >= .env$significance, FALSE, TRUE))

  dplyr::bind_rows(chi, fisher)
}


#' @title Histogram comparison between agencies by length
#' @description Histogram
#' @param .data Dataframe
#' @param x_axis Length column. total_length or fork_length
#' @param x_label Label of x axis
#' @param y_label Label of y axis
#' @param title Title of plot
#' @family Test Fishing
#' @examples
#' \dontrun{tf_data |> tf_interagency_histogram(total_length, agency)}
#'
#' @export
tf_interagency_histogram <- function(.data, x_axis, x_label = 'Total Length (CM)', y_label = 'Count', title = 'Interagency Lengths'){
  x_axis <- rlang::enquo(x_axis)
  .data |>
    dplyr::filter(
      .data$common_name == 'Chinook Salmon',
      .data$catch_result_type_code %in% c('R', 'K'),
      .data$adipose_clip_status_code %in% c('AD', 'UM'),
      !is.na(!!x_axis)
    ) |>
    ggplot2::ggplot(ggplot2::aes(!!x_axis, fill=.data$agency)) +
    ggplot2::geom_histogram(binwidth = 2, alpha=.6, position = 'identity') +
    ggplot2::labs(subtitle = title, x = x_label, y = y_label) +
    ggplot2::geom_vline(xintercept = 55.88)

}

#' @title Line plot of p-values from Chi-square
#' @description Line plot of cummulative p-values yieled from cummulative totals of test fishing encounters
#' @param .data Dataframe
#' @param alpha Significance metric to evaluate whether statistically sigificant or not
#' @family Test Fishing
#' @examples
#' \dontrun{tf_data |> tf_pvalue_history()}
#'
#' @export

tf_pvalue_history <- function(.data, alpha=.05) {
  .data |>
    dplyr::filter(
      .data$common_name == 'Chinook Salmon',
      .data$catch_result_type_code %in% c('R', 'K'),
      .data$adipose_clip_status_code %in% c('AD', 'UM'),
      !is.na(.data$total_length)
    ) |>
    dplyr::mutate(
      legal = dplyr::if_else( .data$total_length >= 55.88, 'Legal', 'Sublegal')
    ) |>
    tidyr::unite('legal_mark',  .data$legal,  .data$adipose_clip_status_code) |>
    dplyr::select(.data$survey_datetime,  .data$agency,  .data$legal_mark) |>
    dplyr::count( .data$survey_datetime,  .data$agency,  .data$legal_mark) |>
    tidyr::pivot_wider(names_from =  .data$legal_mark, values_from =  .data$n, values_fill = 0) |>
    dplyr::group_by( .data$agency) |>
    dplyr::mutate(
      LM = cumsum( .data$Legal_AD),
      LU = cumsum( .data$Legal_UM),
      SM = cumsum( .data$Sublegal_AD),
      SU = cumsum( .data$Sublegal_UM)
    ) |>
    dplyr::select(-.data$Legal_AD, -.data$Legal_UM, -.data$Sublegal_AD, -.data$Sublegal_UM) |>
    dplyr::group_by(.data$survey_datetime) |>
    tidyr::nest() |>

    dplyr::mutate(
      p_value = purrr::map_dbl(.data$data, \(x) suppressWarnings(stats::chisq.test(x[, -1])$p.value)),
      nrow = purrr::map_int(.data$data, nrow)
    ) |>
    dplyr::filter(nrow == 2) |>
    ggplot2::ggplot(ggplot2::aes(.data$survey_datetime, .data$p_value)) +
    ggplot2::labs(x='Date', y='p-value (Chi-square)') +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = alpha, color='red')
}

#' @title Map of test fishing encouners
#' @description This produces a leaflet map showing test fishing encounters
#' @param .data Dataframe
#' @param ... add_effort = TRUE, Adds a heat map of boat surveys. start_date, end_date and catch_area must also be supplied
#' @family Test Fishing
#' @examples
#' \dontrun{tf_data |> tf_map()}
#'
#' @export
tf_map <- function(.data, ...){

  args <- list(...)

  agencies <-  .data |>
    dplyr::select(.data$agency)

  pal <- leaflet::colorFactor(
    palette = 'Dark2',
    domain = agencies$agency
  )

  map <- .data |>
    leaflet::leaflet() |>
    leaflet::addTiles() |>
    leaflet::addCircleMarkers(~.data$longitude_decimal_degrees_wgs84,
                              ~.data$latitude_decimal_degrees_wgs84,
                              color=~pal(.data$agency),
                              label = ~.data$agency)


  if ('add_effort' %in% names(args)) {
    if (
      (args$add_effort == TRUE)
      & ('start_date' %in% names(args))
      & ('end_date' %in% names(args))
      & ('catch_area' %in% names(args))
    ) {

      bs <- bs_pull(args$catch_area, args$start_date, args$end_date)

      map <- map |>
        leaflet.extras::addHeatmap(lat = ~bs$latitude_decimal_degrees_wgs84,
                                   lng = ~bs$longitude_decimal_degrees_wgs84,
                                   intensity = ~bs$angler_count,
                                   blur = 25, max = 0.05, radius = 10)
    } else {
      cli::cli_abort('Missing start_date, end_date, or catch_area for boat survey data.')
    }
  }

  map
}


#' @title Test Fishing Weekly encounters
#' @description Produces a table of weekly encounters by legal mark category
#' @param .data Dataframe
#' @param date_column Column to calculate stat week, usually survey_datetime
#' @family Test Fishing
#' @examples
#' \dontrun{tf_data |> tf_lm_summary_week()}
#'
#' @export
tf_lm_summary_week <- function(.data, date_column ) {

  date_column <- rlang::enquo(date_column)
  .data |>
    dplyr::group_by(stat_week = statistical_week(!!date_column)) |>
    tidyr::nest() |>
    dplyr::mutate(
      lm = purrr::map(.data$data, tf_lm_summary)
    ) |>
    tidyr::unnest(.data$lm) |>
    dplyr::select(-.data$data) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.numeric), \(x) tidyr::replace_na(x, 0)
      )
    )
}

#' @title Test Fishing Weekly Effort
#' @description Produces a table of weekly effort in surveys and hours
#' @param .data Dataframe
#' @param date_column Column to calculate stat week, usually survey_datetime
#' @family Test Fishing
#' @examples
#'  \dontrun{tf_data |> tf_effort_summary_week() }
#'
#' @export
tf_effort_summary_week <- function(.data, date_column) {

  date_column <- rlang::enquo(date_column)

  .data |>
    dplyr::mutate(stat_week = statistical_week(!!date_column)) |>
    dplyr::select(.data$survey_datetime,
                  .data$stat_week,
                  .data$start_datetime,
                  .data$end_datetime) |>
    unique() |>
    dplyr::mutate(hours = lubridate::time_length(
      lubridate::interval(.data$start_datetime, .data$end_datetime),
      'hours'
    )) |>
    dplyr::group_by(.data$stat_week) |>
    dplyr::summarize(
      n = dplyr::n_distinct(.data$start_datetime),
      hours = sum(.data$hours),
      .groups = 'drop'
    )
}


#' @title Test Fishing Weekly Summary
#' @description Produces a table summarizing test fishing effort and encounters
#' @param .data Dataframe
#' @param date_column Column to calculate stat week, usually survey_datetime
#' @family Test Fishing
#' @examples
#' \dontrun{ tf_data |> tf_summary()}
#'
#' @export
tf_summary <- function(.data, date_column) {

  date_column <- rlang::enquo(date_column)

  encounters <- .data |> tf_lm_summary_week(!!date_column)

  effort <- .data |> tf_effort_summary_week(!!date_column)

  effort |>
    dplyr::left_join(encounters, by = "stat_week") |>
    dplyr::select(
      .data$stat_week,
      surveys = .data$n,
      .data$hours,
      .data$Legal_AD,
      .data$Legal_UM,
      .data$Sublegal_AD,
      .data$Sublegal_UM
    ) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), \(x) tidyr::replace_na(x, 0)))
}

