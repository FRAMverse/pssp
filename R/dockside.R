#' @title Downloads test fishing data from pssp
#' @description Queries RMIS for release infomation, defaults to tagged releases
#' @param catch_area Valid catch area code
#' @param start_date Start date of search
#' @param end_date End date of search
#' @param int_dates Returns year, month, day integer columns instead of a date field
#' @param fill_zero Fill NA's with zeros.
#' @param fresh Boolean. Ensures data is updated
#' @param check_fresh Boolean. Checks whether data is current
#' @param dsn DSN for the database connection. Default 'pssp_prod64'
#' @family Dockside Creel Surveys
#' @examples
#' \dontrun{ds_data <- ds_salmon_summary('10', '2021-11-01', '2021-12-04')}
#' \dontrun{ds_data <- ds_salmon_summary(c('11','10'), '2021-11-01', '2021-12-04')}
#' @export
ds_salmon_summary <- function(catch_area,
                              start_date,
                              end_date,
                              int_dates = FALSE,
                              fill_zero = FALSE,
                              fresh = FALSE,
                              check_fresh = TRUE,
                              dsn = "pssp_prod64"){
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
      check_fresh <- FALSE
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
             from mvw_dockside_salmon
             where
                  survey_datetime between '{start_date}' and '{end_date}' and
                  catch_area_code in ('{catch_area}');"
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
      tibble::as_tibble() |>
      janitor::clean_names() |>
      dplyr::select(-.data$last_update)
}




#' @title Makes a chart comparing current year CPUE vs previous years
#' @description Makes a chart comparing current year CPUE vs previous years
#' @param catch_area Valid catch area code
#' @param start_date Start date of search
#' @param end_date End date of search
#' @param smoothing_method Default loess. Takes method arguments from `geom_smooth`
#' @param species Species to look for - chinook, pink, coho
#' @param clipped_only Will use only ad clipped fish
#' @family Dockside Creel Surveys
#' @examples
#' \dontrun{ds_historical_cpue_chart('10', '2021-11-01', '2021-12-04')}
#' \dontrun{ds_historical_cpue_chart('10', '2021-11-01', '2021-12-04') + ggtitle('New Title')}
#' @export
ds_historical_cpue_chart <- function(catch_area,
                                     start_date,
                                     end_date,
                                     species = c('chinook', 'coho', 'pink'),
                                     smoothing_method = "loess",
                                     clipped_only = T) {

    # this function is whacky and needs to be rewritten
    catch_area <- rlang::arg_match(
      catch_area,
      c('05', '06', '61', '62', '07',
        '81', '82', '09', '16', '30',
        '31', '10', '11', '12', '13',
        '26'
      )
    )

    species <- rlang::arg_match(species)

    if (!is.na(as.Date(start_date, '%F')) &
        !is.na(as.Date(end_date, '%F'))) {
      start_date <- as.Date(start_date, '%F')
      end_date <- as.Date(end_date, '%F')
    } else{
      cli::cli_abort("Invalid date format '{end_date}' , try YYYY-MM-DD")
    }

    start_date <- as.Date(start_date, '%F')
    end_date <- as.Date(end_date, '%F')

    start_month <- lubridate::month(start_date)
    end_month <- lubridate::month(end_date)
    start_day <- lubridate::day(start_date)
    end_day <- lubridate::day(end_date)

    if (catch_area == '06') {
      catch_area <- paste0(c('06', '61', '62'), collapse = '\',\'')
    }

    row_value_cons <-
      paste0(
        "(extract(month from survey_datetime), extract(day from survey_datetime)) ",
        "BETWEEN(",
        start_month,
        ", ",
        start_day,
        ") AND (",
        end_month,
        ", ",
        end_day,
        ")"
      )

    query <- pssp_query(
      query =
      glue::glue(
        "select
            *
           from mvw_dockside_salmon
           where
                catch_area_code in('{catch_area}') and
                                    {row_value_cons}"
      )
    )


    df <- query |>
      tibble::as_tibble() |>
      dplyr::select(.data$survey_datetime,
                    .data$anglers,
                    dplyr::matches(paste0('^', species, '.*k$'))
                    )

    if (clipped_only & (tolower(species) %in% c('chinook', 'coho'))) {
      df <-
        df |> dplyr::select(.data$survey_datetime,
                            .data$anglers,
                            total_fish = dplyr::contains('_ad_'))
    } else{
      df <- df |>
        dplyr::rowwise() |>
        dplyr::mutate(total_fish = sum(dplyr::c_across(dplyr::starts_with(species)), na.rm = TRUE)) |>
        dplyr::select(.data$survey_datetime, .data$anglers, .data$total_fish)
    }

    # aggregate and caluclate cpue

    agg <- df |>
      dplyr::group_by(
        year = lubridate::year(.data$survey_datetime),
        month = lubridate::month(.data$survey_datetime),
        day = lubridate::day(.data$survey_datetime),
        .data$survey_datetime
      ) |>
      dplyr::summarize(dplyr::across(c(.data$anglers, .data$total_fish),
                       sum,
                       na.rm = TRUE), .groups = 'drop') |>
      dplyr::mutate(CPUE = .data$total_fish / .data$anglers)
    # odd years for pinks
    if (tolower(species) == 'pink') {
      cli::cli_alert_info('Analysis for Pink salmon will be limited to odd years only')
      agg <- agg |>
        dplyr::filter(.data$year %% 2 == 1)
    }


    ggplot2::ggplot(agg |>
             dplyr::filter(.data$year != lubridate::year(end_date)),
             ggplot2::aes(lubridate::mdy(paste0(
             .data$month, "-", .data$day, '-', 2021
           )),
           .data$CPUE)) +
      ggplot2::geom_smooth(method = smoothing_method,
                  mapping = ggplot2::aes(color = 'Historical CPUE')) +
      ggplot2::geom_line(
        data = agg |>
          dplyr::filter(.data$year == lubridate::year(end_date)),
        mapping = ggplot2::aes(
          x = lubridate::mdy(paste0(.data$month, "-", .data$day, '-', 2021)),
          y = .data$CPUE,
          color = 'Current CPUE'
        )
      ) +
      ggplot2::labs(color = 'Legend',
           x = 'Date',
           subtitle = stringr::str_to_title(
        paste0(species, ' salmon in Area ', catch_area, ' CPUE Comparison')
      )) +
      ggplot2::ylim(0, NA)



}




#' @title Get sampling effort data
#' @description Gets effort data for our dockside sampling
#' @param marine_area Valid marine area code
#' @param start_date Start date of search
#' @param end_date End date of search
#' @family Dockside Creel Surveys
#' @examples
#' \dontrun{df <- ds_pull_sampling_effort('10', '2021-01-01', '2021-12-31')}
#' @export
ds_pull_sampling_effort <- function(marine_area, start_date, end_date){
  marine_area <- purrr::map_chr(marine_area, ~rlang::arg_match0(.x,
                              values = c('05', '06', '61',
                                      '62', '07', '81',
                                      '82', '09', '10', '26',
                                      '11', '12', '13'), arg_nm = 'marine_area'))


  marine_area <- paste0(marine_area, collapse = '\',\'')


  query <- pssp_query(
                           glue::glue("select
              *
             from mvw_dockside_sampling_effort
             where
                  survey_datetime between '{start_date}' and '{end_date}' and
                  marine_area_code in('{marine_area}')")
  )


  query |>
    tibble::as_tibble()
}

#' @title Sampling effort summary
#' @description Gets effort data and summarizes by survey trips and survey hours
#' @param marine_area Valid marine area code
#' @param start_date Start date of search
#' @param end_date End date of search
#' @family Dockside Creel Surveys
#' @examples
#' \dontrun{df <- ds_sampling_effort_summary('10', '2021-01-01', '2021-12-31')}
#' @export
ds_sampling_effort_summary <- function(marine_area, start_date, end_date){

  data <- ds_pull_sampling_effort(marine_area, start_date, end_date)

  data |>
    dplyr::mutate(
      hours = lubridate::time_length(lubridate::interval(.data$start_datetime, .data$end_datetime) ,'hours')
    ) |>
    dplyr::group_by(.data$marine_area_code, .data$location_code, .data$location_name) |>
    dplyr::summarize(
      survey_count = dplyr::n_distinct(.data$survey_id),
      survey_hours = sum(.data$hours),
      mean_survey_hours = mean(.data$hours),
      .groups = 'drop'
    ) |>
    dplyr::mutate(
      count_percent = .data$survey_count / sum(.data$survey_count),
      hours_percent = .data$survey_hours / sum(.data$survey_hours)
    ) |>
    dplyr::arrange(-.data$survey_count)


}


#' @title Sampling effort summary with added sampler
#' @description Gets effort data and summarizes by survey trips and survey hours by sampler
#' @param marine_area Valid marine area code
#' @param start_date Start date of search
#' @param end_date End date of search
#' @family Dockside Creel Surveys
#' @examples
#' \dontrun{df <- ds_sampling_effort_sampler_summary('10', '2021-01-01', '2021-12-31')}
#' @export
ds_sampling_effort_sampler_summary <- function(marine_area, start_date, end_date){

  data <- ds_pull_sampling_effort(marine_area, start_date, end_date)

  data |>
    dplyr::mutate(
      hours = lubridate::time_length(lubridate::interval(.data$start_datetime, .data$end_datetime) ,'hours')
    ) |>
    dplyr::group_by(.data$marine_area_code, .data$sampler_first,
                    .data$sampler_last, .data$location_code, .data$location_name) |>
    dplyr::summarize(
      survey_count = dplyr::n_distinct(.data$survey_id),
      survey_hours = sum(.data$hours),
      mean_survey_hours = mean(.data$hours),
      .groups = 'drop'
    ) |>
    dplyr::mutate(
      count_percent = .data$survey_count / sum(.data$survey_count),
      hours_percent = .data$survey_hours / sum(.data$survey_hours)
    ) |>
    dplyr::arrange(-.data$survey_count)

}


#' @title Pulls a biological infomartion
#' @description Pulls lengths and cwts from dockside information
#' @param catch_area Valid catch area code
#' @param start_date Start date of search
#' @param end_date End date of
#' @param int_dates Returns year, month, day integer columns instead of a date field
#' @param fill_zero Fills NA's with zeros
#' @param fresh Boolean. Ensures data is updated
#' @param check_fresh Boolean. Checks whether data is current
#' @family Dockside Creel Surveys
#' @examples
#' \dontrun{ds_bio_data <- ds_bio_pull('10', '2021-11-01', '2021-12-04')}
#' \dontrun{ds_bio_data <- ds_bio_pull(c('10', '09'), '2021-11-01', '2021-12-04')}
#' @export
ds_bio_pull <- function(catch_area, start_date, end_date,  int_dates = FALSE,
                        fill_zero = FALSE, check_fresh = TRUE, fresh = FALSE){


  catch_area <- purrr::map_chr(catch_area,
                               ~rlang::arg_match0(.x,
                  values = c('05', '06', '61', '16', '04', '4B',
                                      '62', '07', '81', '30', '15', '17',
                                      '82', '09', '10', '26', '28', '31',
                                      '11', '12', '13'), arg_nm = 'catch_area'))


  catch_area <- paste0(catch_area, collapse = '\',\'')


  if(fresh){
    pssp_refresh_data()
    check_fresh = F
  }

  if (check_fresh){
    check_freshness()
  }

  query <- pssp_query(query=
    glue::glue(
      "select
              *
             from mvw_dockside_biological
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
    tibble::as_tibble() |>
    dplyr::mutate(cwt_snout_sample_number = dplyr::na_if(.data$cwt_snout_sample_number, '')) |>
    tidyr::pivot_wider(
      names_from = .data$length_type_description,
      values_from = .data$length_measurement_centimeter
    ) |>
    janitor::clean_names()


}




#' @title Pulls release information for chinook and coho with legal size status
#' @description Pulls release information for chinook and coho with legal size status
#' @param catch_area Valid catch area code
#' @param start_date Start date of search
#' @param end_date End date of search
#' @param int_dates Returns year, month, day integer columns instead of a date field
#' @param fresh Boolean. Ensures data is updated
#' @param check_fresh Boolean. Checks whether data is current
#' @param fill_zero Boolean. Fills NA's with zeros.
#' @family Dockside Creel Surveys
#' @examples
#' \dontrun{ds_rel_data <- ds_releases_pull('10', '2021-11-01', '2021-12-04')}
#' \dontrun{ds_rel_data <- ds_releases_pull(c('10', '09'), '2021-11-01', '2021-12-04')}
#' @export
ds_releases_pull <- function(catch_area, start_date, end_date, int_dates = FALSE,
                             fill_zero = FALSE, check_fresh = TRUE, fresh = FALSE){


  catch_area <- purrr::map_chr(catch_area, ~rlang::arg_match0(.x,
                     values = c('05', '06', '61', '16', '04', '4B',
                                    '62', '07', '81', '30', '15', '17',
                                    '82', '09', '10', '26', '28', '31',
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
             from mvw_dockside_releases
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
    janitor::clean_names()


}



#' @title Pulls the dockside encounters
#' @description Pulls Dockside Encounters
#' @param catch_area Valid catch area code
#' @param start_date Start date of search
#' @param end_date End date of search
#' @param pretty If TRUE will return a summarized dataframe, if FALSE will return a list to be used in other calculations
#' @param bias_corrected Will return proportions with bias correction
#' @param minimum_size_limit_inches Minimum size limit for fishery
#' @family Dockside Creel Surveys
#' @examples
#' \dontrun{dockside_encounters <- ds_encounters('10', '2021-11-01', '2021-12-04')}
#' @export
ds_encounters <- function(catch_area,
                          start_date,
                          end_date,
                          pretty = TRUE,
                          bias_corrected = TRUE,
                          minimum_size_limit_inches = 22) {

  catch_area <- purrr::map_chr(catch_area, ~rlang::arg_match0(.x,
                            values = c('05', '06', '61', '16', '04', '4B',
                                    '62', '07', '81', '30', '15', '17',
                                    '82', '09', '10', '26', '28', '31',
                                    '11', '12', '13'), arg_nm = 'catch_area'))


  bio <- ds_bio_pull(catch_area, start_date, end_date)
  rel <- ds_releases_pull(catch_area, start_date, end_date)


  bio_ <- bio |>
    calculate_total_length(fork_length_column = .data$fork_length,
                           minimum_size_limit_inches = minimum_size_limit_inches) |>
    dplyr::filter(
      !is.na(.data$fork_length),
      .data$adipose_clip_status_code %in% c('AD', 'UM'),
      .data$size_class %in% c('Legal', 'Sublegal'),
      .data$common_name == 'Chinook Salmon'
    ) |>
    dplyr::group_by(.data$survey_event_id, .data$adipose_clip_status_code, .data$size_class) |>
    dplyr::summarize(value = dplyr::n(), .groups = 'drop') |>
    tidyr::unite('legal_status', .data$size_class, .data$adipose_clip_status_code) |>
    tidyr::pivot_wider(names_from = .data$legal_status, values_from = .data$value)


  rel_ <- rel |>
    dplyr::filter(
      .data$common_name == 'Chinook Salmon',
      .data$legal_size_status_description %in% c('Legal', 'Sublegal'),
      .data$adipose_clip_status_code %in% c('AD', 'UM')
    ) |>
    dplyr::group_by(.data$survey_event_id,
                    .data$adipose_clip_status_code,
                    .data$legal_size_status_description) |>
    dplyr::summarize(value = sum(.data$fish_count), .groups = 'drop') |>
    tidyr::unite('legal_status',
                 .data$legal_size_status_description,
                 .data$adipose_clip_status_code) |>
    tidyr::pivot_wider(names_from = .data$legal_status, values_from = .data$value)


  total_encounters <- bio_ |>
    dplyr::bind_rows(rel_) |>
    dplyr::group_by(.data$survey_event_id) |>
    dplyr::summarise_all(sum, na.rm = TRUE) |>
    dplyr::rowwise() |>
    dplyr::mutate(total = sum(dplyr::c_across(-.data$survey_event_id))) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$total > 0)

  if (!'Legal_AD' %in% colnames(total_encounters)) {
    cli::cli_alert_warning('There are no legal marked encounters, you will not be able to make an encounter estimate')
    total_encounters <- total_encounters |>
      dplyr::mutate(Legal_AD = 0)
  }

  if (!'Legal_UM' %in% colnames(total_encounters)) {
    cli::cli_alert_warning('There are no legal unmarked encounters')
    total_encounters <- total_encounters |>
      dplyr::mutate(Legal_UM = 0)
  }


  if (!'Sublegal_AD' %in% colnames(total_encounters)) {
    cli::cli_alert_warning('There are no sublegal marked encounters')
    total_encounters <- total_encounters |>
      dplyr::mutate(Sublegal_AD = 0)
  }


  if (!'Sublegal_UM' %in% colnames(total_encounters)) {
    cli::cli_alert_warning('There are no sublegal unmarked encounters')
    total_encounters <- total_encounters |>
      dplyr::mutate(Sublegal_UM = 0)
  }

  mean_cluster <- mean(total_encounters$total)
  total_encounters_sum <- sum(total_encounters$total)
  total_interviews <- nrow(total_encounters)



  LM_sum <- sum(total_encounters$Legal_AD)
  LU_sum <- sum(total_encounters$Legal_UM)
  SM_sum <- sum(total_encounters$Sublegal_AD)
  SU_sum <- sum(total_encounters$Sublegal_UM)

  #calulcated the percentage of each category
  tryCatch(expr = {
    pLM <- LM_sum / (LM_sum + LU_sum + SM_sum + SU_sum)
  },
  error = {
    pLM <- 0
  })
  tryCatch(expr = {
    pLU <- LU_sum / (LM_sum + LU_sum + SM_sum + SU_sum)
  },
  error = {
    pLU <- 0
  })
  tryCatch(expr = {
    pSM <- SM_sum / (LM_sum + LU_sum + SM_sum + SU_sum)
  },
  error = {
    pSM <- 0
  })
  tryCatch(expr = {
    pSU <- SU_sum / (LM_sum + LU_sum + SM_sum + SU_sum)
  },
  error = {
    pSU <- 0
  })

  encounters <- list(
    legal_marked = LM_sum,
    legal_unmarked = LU_sum,
    sublegal_marked = SM_sum,
    sublegal_unmarked = SU_sum
  )

  original_proportions <- list(
    legal_marked = pLM,
    legal_unmarked = pLU,
    sublegal_marked = pSM,
    sublegal_unmarked = pSU
  )


  total_encounters <- total_encounters |>
    dplyr::mutate(
      enc_comp_LM = (.data$Legal_AD - (.data$total * pLM)) ^ 2,
      enc_comp_LU = (.data$Legal_UM - (.data$total * pLU)) ^ 2,
      enc_comp_SM = (.data$Sublegal_AD - (.data$total * pSM)) ^ 2,
      enc_comp_SU = (.data$Sublegal_UM - (.data$total * pSU)) ^ 2,
    )

  pLM.v <-
    (sum(total_encounters$enc_comp_LM) / (total_interviews - 1)) / (total_interviews * mean_cluster ^
                                                                      2)
  pLU.v <-
    (sum(total_encounters$enc_comp_LU) / (total_interviews - 1)) / (total_interviews * mean_cluster ^
                                                                      2)
  pSM.v <-
    (sum(total_encounters$enc_comp_SM) / (total_interviews - 1)) / (total_interviews * mean_cluster ^
                                                                      2)
  pSU.v <-
    (sum(total_encounters$enc_comp_SU) / (total_interviews - 1)) / (total_interviews * mean_cluster ^
                                                                      2)


  variance_of_proportions <- list(
    legal_marked = pLM.v,
    legal_unmarked = pLU.v,
    sublegal_marked = pSM.v,
    sublegal_unmarked = pSU.v
  )

  data <-
    list(
      encounters = encounters,
      original_proportions = original_proportions,
      variance_of_proportions = variance_of_proportions
    )


  class_text <- 'non-bias adjusted proportions'
  if (bias_corrected) {
    pSU <- pSU + (pLM * (1 - .89))
    pLM <- pLM * .89

    class_text <- 'bias adjusted proportions'


    bias_corrected_proportions <- list(
      legal_marked = pLM,
      legal_unmarked = pLU,
      sublegal_marked = pSM,
      sublegal_unmarked = pSU
    )


    data <- list(
      encounters = encounters,
      original_proportions = original_proportions,
      variance_of_proportions = variance_of_proportions,
      bias_corrected_proportions = bias_corrected_proportions
    )
  }





  if (pretty) {
    data <- total_encounters |>
      dplyr::summarise_at(dplyr::vars(
        -c(
          .data$survey_event_id,
          .data$enc_comp_LM,
          .data$enc_comp_LU,
          .data$enc_comp_SM,
          .data$enc_comp_SU
        )
      ), sum) |>
      dplyr::mutate(class = 'encounters') |>
      dplyr::bind_rows(
        tibble::tibble(
          Legal_AD = pLM,
          Legal_UM = pLU,
          Sublegal_AD = pSM,
          Sublegal_UM = pSU,
          class = class_text

        ),
        tibble::tibble(
          Legal_AD = pLM.v,
          Legal_UM = pLU.v,
          Sublegal_AD = pSM.v,
          Sublegal_UM = pSU.v,
          class = 'variance of proportions'
        )
      ) |>
      dplyr::select(sort(tidyselect::peek_vars()))
  }
  data
}

