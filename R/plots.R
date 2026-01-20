#' @importFrom ggplot2 ggplot aes
#' @importFrom dplyr `%>%` filter mutate
NULL

#' Plot parameters from a dataframe vs latitude
#'
#' @param df a dataframe with Dec_LAT
#' @param property Chemical property to plot
#' @param lat_range a two element vector of decimal degrees
#'
#' @return A ggplot object
#' @export
#'
lat_prop_plot <- function(df, property, lat_range = NULL) {
  if (!is.null(lat_range)) {
    df <- df %>%
      filter(Dec_LAT < lat_range[2],
             Dec_LAT > lat_range[1])
    lter_stations <- lter_stations %>%
      filter(latitude < lat_range[2],
             latitude > lat_range[1])
  }

  lter_stations <- lter_stations %>%
    filter(stringr::str_starts(name, "L"),
           !stringr::str_ends(name, "12|13"))

  #stn_name_y <- min(df[[{property}]]) - 1
  stn_name_y <- summarize(df, ymin = min({{ property }})) %>%
      pull(ymin)
  ggplot(df) +
    ggplot2::geom_vline(data = lter_stations,
                        aes(xintercept=latitude),
                        color = "gray") +
    ggplot2::geom_text(data = lter_stations,
                       aes(x=latitude, y = stn_name_y, label = name),
                       hjust = 0, nudge_x = 0.01,
                       size = 3) +
    ggplot2::geom_point(aes(Dec_LAT, {{property}},
                            color = factor(as.Date(df$timestamp))),
                        size = 1.5) +
    ggplot2::geom_path(aes(Dec_LAT, {{property}},
                           color = factor(as.Date(df$timestamp))),
                       size = 1.5) +
    ggplot2::scale_x_reverse() +
    ggplot2::scale_color_viridis_d(name = "Date") +
    ggplot2::labs(x = "Latitude (deg N)",
                  y = substitute(property))# +
  #  ggplot2::theme(legend.position = c(0.20, 0.20),
  #                 legend.background = ggplot2::element_rect(fill = "white", color = "black"))
}

#' Plot EIMS timeseries with shaded air_cal bands and trimmed-band means
#'
#' @param eims  Data frame containing at least: timestamp, air_cal, and the y column
#' @param y_col Character scalar: name of the column to plot on the y-axis, e.g. "mass_4032"
#' @param mean_color Color for mean lines
#' @param trim_seconds Seconds to trim from start and end of each band (default 60)
#' @param yaxis_title Y-axis title (default taken from y_col)
#'
#' @return A plotly object
#'
#' @export
plot_eims_air_bands <- function(
  eims,
  y_col,
  mean_color   = "red",
  trim_seconds = 60,
  yaxis_title  = NULL
) {
  # y_col must be a character string
  if (!is.character(y_col) || length(y_col) != 1L) {
    stop("y_col must be a single character string naming a column in eims")
  }

  if (!y_col %in% names(eims)) {
    stop(sprintf("Column '%s' not found in eims", y_col))
  }

  if (is.null(yaxis_title)) {
    yaxis_title <- y_col
  }

  # Extract y values once, as a plain vector
  y_vec <- eims[[y_col]]

  # --- find contiguous air_cal intervals for shading ---
  air_intervals <- eims %>%
    arrange(timestamp) %>%
    mutate(
      grp = cumsum(air_cal & !dplyr::lag(air_cal, default = FALSE))
    ) %>%
    filter(air_cal) %>%
    group_by(grp) %>%
    summarise(
      start = min(timestamp),
      end   = max(timestamp),
      .groups = "drop"
    )

  # If no intervals, just plot the line and return
  if (nrow(air_intervals) == 0) {
    p <- plot_ly(
      data = eims,
      x = ~timestamp,
      y = y_vec,
      type = "scatter",
      mode = "lines",
      name = y_col
    ) |>
      layout(
        yaxis = list(title = yaxis_title)
      )
    return(p)
  }

  # --- define trimmed intervals: skip first and last `trim_seconds` of each band ---
  air_intervals_trimmed <- air_intervals %>%
    mutate(
      start_trim = start + dseconds(trim_seconds),
      end_trim   = end   - dseconds(trim_seconds)
    ) %>%
    filter(start_trim < end_trim)

  # --- compute mean within trimmed intervals only ---
  air_means <- air_intervals_trimmed %>%
    rowwise() %>%
    mutate(
      band_mean = mean(
        y_vec[
          eims$timestamp >= start_trim &
          eims$timestamp <= end_trim &
          eims$air_cal
        ],
        na.rm = TRUE
      )
    ) %>%
    ungroup()

  # --- base plot ---
  p <- plot_ly(
    data = eims,
    x = ~timestamp,
    y = y_vec,
    type = "scatter",
    mode = "lines",
    name = y_col
  )

  # --- shaded rectangles over full bands (original start/end) ---
  p <- p |>
    layout(
      yaxis = list(title = yaxis_title),
      shapes = lapply(seq_len(nrow(air_intervals)), function(i) {
        list(
          type = "rect",
          xref = "x",
          yref = "paper",
          x0   = air_intervals$start[i],
          x1   = air_intervals$end[i],
          y0   = 0,
          y1   = 1,
          fillcolor = "rgba(0, 150, 255, 0.2)",  # light blue
          line = list(width = 0)
        )
      })
    )

  # --- add mean lines only over trimmed part of each band ---
  if (nrow(air_means) > 0) {
    for (i in seq_len(nrow(air_means))) {
      p <- p |>
        add_trace(
          x = c(air_means$start_trim[i], air_means$end_trim[i]),
          y = rep(air_means$band_mean[i], 2),
          type = "scatter",
          mode = "lines",
          line = list(color = mean_color, width = 2),
          name = paste("Band mean", y_col),
          showlegend = FALSE
        )
    }
  }

  p
}
