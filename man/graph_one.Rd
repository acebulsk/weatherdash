% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph.R
\name{graph_one}
\alias{graph_one}
\title{Create Graph With One Parameter}
\usage{
graph_one(
  data,
  x,
  y1,
  y1_name,
  col_one = "rgb(0,119,187)",
  margin = list(b = 0, r = 50, l = 50, t = 10)
)
}
\arguments{
\item{data}{with posixct datetime and at least one weather parameter}

\item{x}{datetime as posixct}

\item{y1}{weather variable}

\item{y1_name}{pretty name to be used on plot}

\item{col_one}{colour for line ex. "rgb(0,119,187)"}

\item{margin}{standard margins for plotly plot}
}
\value{
plotly graph
}
\description{
Create Graph With One Parameter
}
