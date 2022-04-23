# Functions for EIMS csv export

#' @importFrom magrittr `%>%`
NULL

#' Read EIMS export file
#'
#' @param filename
#'
#' @export
#'
read_eims <- function(filename) {
  eims <- readLines(filename)
  et <- unlist(strsplit(eims[3], ','))
  eims_start <- as.POSIXlt(paste(et[2], et[4]),
                           tz = "GMT",
                           format = "%m/%d/%Y %H:%M:%S")
  readr::read_csv(I(eims), skip = 26,
                  col_names = c("elapsed_time", "ms", "mass_14",
                                "mass_32", "mass_40",
                                "mass_18", "mass_44"),
                  col_types = c('t', 'd', 'd', 'd', 'd', 'd', 'd'),
                  col_select = 1:7) |>
    dplyr::mutate(timestamp = eims_start + elapsed_time)
}

#' Timeseries plot of eims mass
#'
#' @param data A dataframe
#' @param field Character string of name of column.
#' @param timestamp Character string of name timestamp column.
#'
#' @return Timeseries plot
#' @export
#'
plot_ts <- function(data, field, timestamp = "timestamp") {
  field_ts <- xts::as.xts(data[[field]], data[["timestamp"]])
  dygraphs::dygraph(field_ts, group = "massplot") %>%
    dygraphs::dyRangeSelector()
}

#' Timeseries plot of mass ratio
#'
#' @param eims_data A dataframe of EIMS data
#' @param masses A two element character vector of mass column names
#'
#' @return Timeseries plot
#' @export
#'
plot_eims_ratio <- function(eims_data, masses) {
  stopifnot(length(masses) == 2)
  ratio <- eims_data[[masses[1]]] / eims_data[[masses[2]]]
  ratio_ts <- xts::as.xts(ratio, eims_data[["timestamp"]])
  dygraphs::dygraph(ratio_ts, group = "massplot") %>%
    dygraphs::dyRangeSelector()
}
