#' @title In-Sample filter (Murthy)
#' @description Filters to in sample locations with a Murthy study design
#' @param .data Dataframe
#' @family Dockside Creel Surveys
#' @examples
#' \dontrun{df |> ds_filter_insample_murthy()}
#' @export
ds_filter_insample_murthy <- function(.data){
  .data |>
    identify_in_sample(design = 'murthy') |>
    dplyr::filter(.data$in_sample == TRUE) |>
    dplyr::select(-.data$in_sample)

}


#' @title In-Sample filter (Aerial)
#' @description Filters to in sample locations with a Aerial study design
#' @param .data Dataframe
#' @family Dockside Creel Surveys
#' @examples
#' \dontrun{df |> ds_filter_insample_aerial()}
#' @export
ds_filter_insample_aerial <- function(.data){
  .data |>
    identify_in_sample(design = 'aerial') |>
    dplyr::filter(.data$in_sample == TRUE) |>
    dplyr::select(-.data$in_sample)

}


#' @title In-Sample idenifier
#' @description Identifys in-sample locations by adding in_sampling column
#' @param .data Dataframe
#' @param design Either murthy or aerial
#' @param location_code_column The column in dataframe where the location codes are.
#' @param named Boolen. Returns a column of in/out of sample.
#' @family Dockside Creel Surveys
#' @examples
#' \dontrun{df |> identify_in_sample(design = 'murthy')}
#' @export
identify_in_sample <- function(.data, location_code_column = "location_code", design = c('murthy', 'aerial'), named = FALSE){
  design <- rlang::arg_match(design)
  location_code <- rlang::enquo(location_code_column)

  if (design == 'murthy'){
    df <- .data |>
      dplyr::mutate(
        in_sample =
          dplyr::if_else(
            (.data$catch_area_code == '10' & !!location_code %in% c('1006', '1023', '1434',
                                                              '1132','1227')
            ) |
              (.data$catch_area_code == '09' & !!location_code %in% c('1382','1151','1067',
                                                                '1434','1144','1192')
              ) |
              (.data$catch_area_code == '11' & !!location_code %in% c('1006','1173','1174', '1202')
              ) |

              (.data$catch_area_code == '05' & !!location_code %in% c('1044','1450','1452',
                                                                '1451','1456','1457')
              ) |
              (.data$catch_area_code == '61' & !!location_code %in% c('1072', '1186', '1187')), TRUE, FALSE)



      )
  } else {
    df <- .data |>
      dplyr::mutate(
        in_sample = dplyr::if_else(
          (.data$catch_area_code == '09' & location_code %in% c('1151', '1192', '1434', '1382')) |
            (.data$catch_area_code == '06' & location_code %in% c('1184', '1041', '1186', '1107')) |
            (.data$catch_area_code == '07' & location_code %in% c('1073', '1268', '1013', '1041')), TRUE, FALSE
        )
      )
  }

  if (named == TRUE){

    df <- df |>
      dplyr::mutate(in_sample = dplyr::if_else(.data$in_sample, 'in_sample', 'out_of_sample'))
  }


  df

}


#' @title In-Sample filter
#' @description Filters to in sample locations with study design input
#' @param .data Dataframe
#' @param design Either murthy or aerial
#'
#'
#' @family Dockside Creel Surveys
#' @examples
#' \dontrun{df |> ds_filter_insample(design = 'murthy')}
#' @export
ds_filter_insample <- function(.data, design = c('murthy', 'aerial')){
  design <- rlang::arg_match(design)
  switch(
    design,
    murthy = ds_filter_insample_murthy(.data),
    aerial = ds_filter_insample_aerial(.data)
  )

}
