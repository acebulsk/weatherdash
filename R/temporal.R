## functions related to temporal changes

#' set year of posix datetime using lubridate for if else statement
#'
#' @param DATE_TIME datetime to set year
#' @param YEAR year to set
#'
#' @return posixct with updated yr
#' @export
#'
set_yr <- function(DATE_TIME, YEAR) {
  lubridate::year(DATE_TIME) <- YEAR

  DATE_TIME
}

#' calculate water year by date
#'
#' @param dates Date 'YYYY-MM-DD'
#' @param start_month water year typically starts with 10 but can change if different.
#'
#' @return year
#' @export
#'
wtr_yr <- function(dates, start_month=10) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  adj.year
}
