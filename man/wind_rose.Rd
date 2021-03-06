% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph.R
\name{wind_rose}
\alias{wind_rose}
\title{Create Wind Rose Plot}
\usage{
wind_rose(
  data,
  datetime,
  wind_spd_avg,
  wind_dir_avg,
  start_time = NA,
  end_time = NA,
  plot_title = "",
  dir_res = 15,
  ws_breaks = 5,
  spd_unit = "km/h"
)
}
\arguments{
\item{data}{dataframe with datetime, wind speed and wind dir}

\item{datetime}{a string that matches the name of the datetime column name}

\item{wind_spd_avg}{a string that matches the name of the wind speed column}

\item{wind_dir_avg}{a string that matches the name of the wind direction column (string)}

\item{start_time}{a POSIXct formatted time for begining of chart data}

\item{end_time}{a POSIXct formatted time for end of chart data}

\item{plot_title}{Title of the plot}

\item{dir_res}{an integer of the degree resolution of the wind rose}

\item{ws_breaks}{integer, the desired number of wind speed bins}

\item{spd_unit}{string, wind speed unit}
}
\value{
plotly wind rose
}
\description{
Create Wind Rose Plot
}
