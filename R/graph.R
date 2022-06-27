#' Create Graph With One Parameter
#'
#' @param data with posixct datetime and at least one weather parameter
#' @param x datetime as posixct
#' @param y1 weather variable
#' @param y1_name pretty name to be used on plot
#' @param col_one colour for line ex. "rgb(0,119,187)"
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
#' @param col_one colour for line ex. "rgb(0,119,187)"
#' @param col_two colour for line ex. "rgb(0,119,187)"
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
#' @param datetime a string that matches the name of the datetime column name
#' @param wind_spd_avg a string that matches the name of the wind speed column
#' @param wind_dir_avg a string that matches the name of the wind direction column (string)
#' @param start_time a POSIXct formatted time for begining of chart data
#' @param end_time a POSIXct formatted time for end of chart data
#' @param plot_title Title of the plot
#' @param dir_res an integer of the degree resolution of the wind rose
#' @param ws_breaks integer, the desired number of wind speed bins
#' @param spd_unit string, wind speed unit

#'
#' @return plotly wind rose
#' @export
#'
wind_rose <- function(data,
                      datetime,
                      wind_spd_avg,
                      wind_dir_avg,
                      start_time = NA,
                      end_time = NA,
                      plot_title = "",
                      dir_res = 15,
                      ws_breaks = 5,
                      spd_unit = 'km/h'){
  # account for stations that dont have ws sensors and if there have been gaps for over half the data period
  if(all(data[, wind_spd_avg] == 0) | all(is.na(data[, wind_spd_avg]) == T)){
    plot <- plotly::plotly_empty(type = "barpolar") |>
      plotly::layout(
        margin = list(t = 60, l = 20, r = 20),
        annotations = list(text = plot_title, xanchor = "centre",
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
    dir.breaks <- c(-dir_res/2,
                    seq(dir_res/2, 360-dir_res/2, by = dir_res),
                    360+dir_res/2)
    dir.labels <- c(seq(0, 360, by = dir_res))

    if (is.na(start_time) == T) {
      start_time <- min(data[,datetime], na.rm = T)
    }

    if (is.na(end_time) == T) {
      end_time <- max(data[,datetime], na.rm = T)
    }

    ws_max <- max(data[,wind_spd_avg], na.rm = T)

    ws_brk_res <- round(ws_max / 5)

    ws_lab <- NA

    for (x in 0:(ws_breaks-1)) {
      if (x == 0) {
        ws_lab[x+1] <- paste0(0, '-', ws_brk_res, ' ', spd_unit)
      } else {
        ws1 <- ws_brk_res * x
        ws2 <- ws_brk_res * (x + 1)

        ws_lab[x+1] <- paste0(ws1, '-', ws2, " ", spd_unit)
      }
    }

    wind <- data |>
      dplyr::filter(!!rlang::sym(datetime) >= start_time,
                    !!rlang::sym(datetime) <= end_time) |>
      dplyr::select(!!rlang::sym(datetime), !!rlang::sym(wind_spd_avg), !!rlang::sym(wind_dir_avg)) |>
      dplyr::mutate(ws_bin = cut(!!rlang::sym(wind_spd_avg), breaks=ws_breaks, labels = ws_lab)) |>
      dplyr::mutate(wd_bin = cut(!!rlang::sym(wind_dir_avg), breaks = dir.breaks, labels = dir.labels)) |>
      dplyr::group_by(wd_bin, ws_bin) |>
      dplyr::summarise(Freq=(dplyr::n()/nrow(data))*100) |>
      dplyr::arrange(ws_bin)

    plot <- plotly::plot_ly(wind, type = "barpolar", hovertemplate = paste('Freq (%): %{r:.2f}<br>Dir: %{theta}\u00b0;'), colors = c("#4575B4", "#91BFDB", "#E0F3F8", "#FEE090", "#FC8D59", "#D73027")) |>
      plotly::add_trace(r = ~Freq, theta = ~wd_bin, color = ~ws_bin) |>
      plotly::layout(
        plot_bgcolor = "#f5f5f5",
        paper_bgcolor = "#f5f5f5",
        autosize = TRUE,
        margin = list(l = 10, r = 10),
        annotations = list(text = plot_title, xanchor = "centre",
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
