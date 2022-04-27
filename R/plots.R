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
