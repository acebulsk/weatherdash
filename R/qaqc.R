## this code contains functions to clean hydromet data

#' Clean Snow Depth Data
#'
#' @param data dataframe with snow depth data
#' @param datetime col for datetime to fill gaps
#' @param snow col name for snow depth
#' @param spike_th threshold to be used for pos. AND neg. spikes
#' @param roc_hi_th rate of change threshold
#' @param roc_low_th rate of change threshold
#'
#' @return cleaned dataframe
#' @export
#'
#' @example
#' spike_clean(apelake, 'DateTime', 'Snow_Depth', spike_th = 10, roc_hi_th = 40, roc_low_th = 75)
spike_clean <- function(data, datetime, snow, spike_th, roc_hi_th, roc_low_th) {
  data <- tsibble::as_tsibble(data, index = !!rlang::sym(datetime)) |>
    tsibble::fill_gaps() |>
    dplyr::mutate(
      snow_clean =
        dplyr::case_when(
          (!!rlang::sym(snow) - dplyr::lag(!!rlang::sym(snow))) < -spike_th & (dplyr::lead(!!rlang::sym(snow)) - dplyr::lag(!!rlang::sym(snow))) < -spike_th & (dplyr::lead(!!rlang::sym(snow), n = 2) - !!rlang::sym(snow)) > spike_th ~ dplyr::lag(!!rlang::sym(snow)), #  low spike
          (!!rlang::sym(snow) - dplyr::lag(!!rlang::sym(snow))) > spike_th & (dplyr::lead(!!rlang::sym(snow)) - !!rlang::sym(snow)) < -spike_th ~ dplyr::lag(!!rlang::sym(snow)), #  hi spike
          (!!rlang::sym(snow) - dplyr::lag(!!rlang::sym(snow))) < -spike_th & (dplyr::lead(!!rlang::sym(snow)) - !!rlang::sym(snow)) > spike_th ~ dplyr::lag(!!rlang::sym(snow)), #  low spike
          TRUE ~ !!rlang::sym(snow) # else set to raw
        )
    )

  # while the dataset does not have a rate of change higher than the threshold
  while(TRUE %in% (data$snow_clean - dplyr::lag(data$snow_clean) > roc_hi_th)){
    data <- data |>
      dplyr::mutate(
        snow_clean =
          dplyr::case_when(

            (snow_clean - dplyr::lag(snow_clean)) > roc_hi_th ~ dplyr::lag(snow_clean), # rate of change
            (snow_clean - dplyr::lead(snow_clean)) > roc_low_th ~ dplyr::lead(snow_clean), # rate of change
            TRUE ~ snow_clean # else set to raw
          )
      )
  }
  return(data)
}
