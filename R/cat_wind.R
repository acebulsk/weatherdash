#' Categorize wind speed from numerical to cardinal names (N,...)
#'
#' @param df dataframe object that contains at least one column with numerical wind speed directions.
#' @param wind_dir_num string of column name that contains the wind directions.
#' @param dir_res integer representing the degree resolution of the bins. E.g. 45 deg. is 8 bins
#' @param bin_col_name string of the new column name to contain the wind direction bin.
#'
#' @return dataframe with additional numerical column that contains binned wind speed directions
#' @export
#'
#' @examples cat_wind(apelake, 'Wind_Dir', 'test', 45) |> dplyr::select(Wind_Dir, test)
cat_wind <- function(df, wind_dir_num, bin_col_name, dir_res = 45) {

  dir.breaks <- c(-dir_res/2,
                  seq(dir_res/2, 360-dir_res/2, by = dir_res),
                  360+dir_res/2)

  dir.labels <- c(seq(0, 360, by = dir_res))

  df[, bin_col_name] <- cut(df[,wind_dir_num], breaks = dir.breaks, labels = dir.labels)

  return(df)
}
