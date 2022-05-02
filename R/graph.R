#' Create Graph With One Parameter
#'
#' @param data with posixct datetime and at least one weather parameter
#' @param x datetime as posixct
#' @param y weather variable
#' @param y_name pretty name to be used on plot
#' @param margin standard margins for plotly plot
#'
#' @return plotly graph
#' @export
#'
graph_one <- function(data, x, y1, y1_name, col_one = "rgb(0,119,187)",
                      margin = list(b = 0, r = 50, l = 50, t = 10)){
  plotly::plot_ly(data = data, mode ="lines", type = "scatter") |>
    plotly::add_trace(
      x = data[,x],
      y = data[,y1],
      name = y1_name,
      line = list(color = col_one, width = 1)
    ) |>
    plotly::layout(
      xaxis = c(list(title = ""), zeroline = FALSE, showline = TRUE),
      yaxis = c(list(zeroline = FALSE, showline = TRUE, title=paste0("<b>",y1_name,"</b>"),titlefont = list(color = col_one))),
      margin = margin,
      showlegend = F,
      plot_bgcolor = "#f5f5f5",
      paper_bgcolor = "#f5f5f5"
    )
}

#' Create Graph With Two Parameters
#'
#' @param data with posixct datetime and at least one weather parameter
#' @param x datetime as posixct
#' @param y1 weather variable 1
#' @param y2 weather variable 2
#' @param y1_name pretty name to be used on plot
#' @param y2_name pretty name to be used on plot
#' @param margin standard margins for plotly plot
#'
#' @return plotly graph
#' @export
#'
graph_two <- function(data, x, y1, y2, y1_name, y2_name,
                      col_one = "rgb(0,119,187)",
                      col_two = "rgb(204,51,17)",
                      margin = list(b = 0, r = 50, l = 50, t = 10)){
  plotly::plot_ly(data = data, mode ="lines", type = "scatter") |>
    plotly::add_trace(
      x = data[,x],
      y = data[,y1],
      name = y1_name,
      line = list(color = col_one, width = 1)
    ) |>
    plotly::add_trace(
      x = data[,x],
      y = data[,y2],
      yaxis = "y2",
      name = y2_name,
      line = list(color = col_two, width = 1)
    ) |>
    plotly::layout(
      xaxis = c(list(title = ""), zeroline = FALSE, showline = TRUE),
      yaxis = c(list(title=paste0("<b>",y1_name,"</b>"),
                     titlefont = list(color = col_one),
                     zeroline = FALSE, showline = TRUE)),
      yaxis2 = c(list(title = paste0("<b>",y2_name,"</b>"),
                      titlefont = list(color = col_two),
                      overlaying = "y",side = "right",
                      zeroline = FALSE, showline = TRUE)),
      margin = margin,
      showlegend = F,
      plot_bgcolor = "#f5f5f5",
      paper_bgcolor = "#f5f5f5"
    )

}

#' Create Wind Rose Plot
#'
#' @param data dataframe with datetime, wind speed and wind dir
#' @param datetime datetime column name (string)
#' @param wind_spd_avg name of the wind speed column (string)
#' @param wind_dir_avg name of the wind direction column (string)
#' @param hrs the number of hours to create the plot over, 168 for weekly, 24 for daily
#' @param plotTitle Title of the plot
#' @param dirres the degree resolution of the wind rose

#'
#' @return plotly wind rose
#' @export
#'
wind_rose <- function(data, datetime, wind_spd_avg, wind_dir_avg, hrs = 168, plotTitle = "", dirres = 15){

  # account for stations that dont have ws sensors and if there have been gaps for over half the data period
  if(all(data[, wind_spd_avg] == 0) | length(data[,wind_spd_avg]) < (hrs / 2)){
    plot <- plotly::plotly_empty(type = "barpolar") |>
      plotly::layout(
        margin = list(t = 60, l = 20, r = 20),
        annotations = list(text = plotTitle, xanchor = "centre",
                           yref="paper",y=1,yshift=50,showarrow=FALSE,
                           font=list(size=18,color='rgb(0,0,0)')),
        xaxis = list(
          title = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
        ),
        legend = list(orientation = "h"),
        polar = list(
          radialaxis = list(
            nticks = 7,
            angle = 45,
            tickangle = 45,
            ticksuffix = " %"
          ),
          angularaxis = list(
            tickmode = "array",
            tickvals = c(0 , 45, 90, 135, 180, 225, 270, 315),
            ticktext = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
            direction = "clockwise"
          )
        )
      )
  } else {
    # now do the actual plotting if we have data to work with
    dirres <- dirres
    dir.breaks <- c(-dirres/2,
                    seq(dirres/2, 360-dirres/2, by = dirres),
                    360+dirres/2)
    dir.labels <- c(seq(0, 360, by = dirres))

    startTime <- (Sys.time() - lubridate::hours(8))-lubridate::hours(hrs)

    wind <- data |>
      dplyr::filter(!!rlang::sym(datetime) >= startTime) |>
      dplyr::select(!!rlang::sym(datetime), !!rlang::sym(wind_spd_avg), !!rlang::sym(wind_dir_avg)) |>
      dplyr::mutate(ws_bin = cut(!!rlang::sym(wind_spd_avg), breaks=c(-Inf, 5, 10, 20, 30, 40, 50, Inf), labels = c("< 5 km/h", "5-10 km/h", "10-20 km/h", "20-30 hm/h", "30-40 km/h", "40-50 km/h",">50 km/h"))) |>
      dplyr::mutate(wd_bin = cut(!!rlang::sym(wind_dir_avg), breaks = dir.breaks, labels = dir.labels)) |>
      dplyr::group_by(wd_bin, ws_bin) |>
      dplyr::summarise(Freq=(dplyr::n()/hrs)*100) |>
      dplyr::arrange(ws_bin)

    plot <- plotly::plot_ly(wind, type = "barpolar", hovertemplate = paste('Freq (%): %{r:.2f}<br>Dir: %{theta}\u00b0;'), colors = c("#4575B4", "#91BFDB", "#E0F3F8", "#FEE090", "#FC8D59", "#D73027")) |>
      plotly::add_trace(r = ~Freq, theta = ~wd_bin, color = ~ws_bin) |>
      plotly::layout(
        plot_bgcolor = "#f5f5f5",
        paper_bgcolor = "#f5f5f5",
        autosize = TRUE,
        margin = list(l = 10, r = 10),
        annotations = list(text = plotTitle, xanchor = "centre",
                           yref="paper",y=1,yshift=50,showarrow=FALSE,
                           font=list(size=18,color='rgb(0,0,0)')),
        xaxis = list(
          title = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
        ),
        legend = list(orientation = "h"),
        polar = list(
          radialaxis = list(
            nticks = 7,
            angle = 45,
            tickangle = 45,
            ticksuffix = " %"
          ),
          angularaxis = list(
            tickmode = "array",
            tickvals = c(0 , 45, 90, 135, 180, 225, 270, 315),
            ticktext = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
            direction = "clockwise"
          )
        )
      )
  }
  plot
}
