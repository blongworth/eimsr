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

#' Read air calibration start and end times from a text file
#'
#' This function reads a text file containing air calibration start and end
#' times in a loose, column-like format. It:
#' @param file_path Character scalar. Path to the calibration text file.
#' @return A \code{data.frame} with two columns:
#'   \describe{
#'     \item{air_start}{POSIXct vector of air calibration start times.}
#'     \item{air_end}{POSIXct vector of air calibration end times.}
#'   }
#' @export
read_air_calibrations <- function(file_path) {
  # Read raw lines from file
  lines <- readLines(file_path, warn = FALSE)

  # Drop empty lines
  lines <- lines[!grepl("^\\s*$", lines)]

  # Drop lines containing only "0" (often used as an end marker)
  lines <- lines[!grepl("^\\s*0\\s*$", lines)]

  # Drop comment lines starting with '%'
  lines <- lines[!grepl("^\\s*%", lines)]

  # Helper: extract datetime tokens (MM/DD/YYYY HH:MM:SS) from a line
  extract_datetimes <- function(x) {
    pattern <- "(\\d{2}/\\d{2}/\\d{4}\\s+\\d{2}:\\d{2}:\\d{2})"
    regmatches(x, gregexpr(pattern, x))[[1]]
  }

  # Collect all datetime strings in order from remaining lines
  all_dt_strings <- unlist(lapply(lines, extract_datetimes), use.names = FALSE)

  # There must be an even number of datetimes to form (start, end) pairs
  if (length(all_dt_strings) < 2 || length(all_dt_strings) %% 2 != 0) {
    stop(
      "Expected an even number of datetime values (start/end pairs). ",
      "Found ", length(all_dt_strings), " values."
    )
  }

  # Convert character datetimes to POSIXct
  all_dt <- as.POSIXct(
    all_dt_strings,
    format = "%m/%d/%Y %H:%M:%S",
    tz = "UTC"
  )

  # Check for failed conversions (NA values)
  if (any(is.na(all_dt))) {
    bad_idx <- which(is.na(all_dt))
    bad_vals <- paste(all_dt_strings[bad_idx], collapse = ", ")
    stop(
      "Failed to parse one or more datetime values. ",
      "Problematic values: ", bad_vals
    )
  }

  # Pair datetimes: (1,2), (3,4), (5,6), ...
  air_start <- all_dt[c(TRUE, FALSE)]
  air_end   <- all_dt[c(FALSE, TRUE)]

  # Return as a data frame
  data.frame(
    air_start = air_start,
    air_end   = air_end,
    stringsAsFactors = FALSE
  )
}

#' Flag rows that fall within any air calibration interval
#'
#' Given a data frame with timestamps and a data frame of air calibration
#' intervals (as returned by \code{read_air_calibrations()}), this function
#' adds a logical \code{air_cal} column indicating whether each row's time
#' falls within any \code{[air_start, air_end]} interval.
#'
#' @param df A data frame containing at least one time column.
#' @param air_cal_df A data frame with columns \code{air_start} and
#'   \code{air_end}, both POSIXct, defining calibration intervals.
#' @param time_col Name of the time column in \code{df}. Can be a string
#'   (e.g. \code{"time"}) or an unquoted column name (e.g. \code{time}).
#'
#' @return A copy of \code{df} with an added logical column \code{air_cal},
#'   where \code{TRUE} indicates the time is within at least one calibration
#'   interval, inclusive of endpoints.
#' @export
add_air_cal_flag <- function(df, air_cal_df, time_col) {
  # Allow both quoted and unquoted column names for time_col
  time_col_quo <- substitute(time_col)

  # Extract the time vector
  time_vec <- df[[deparse(time_col_quo)]]

  # Basic checks
  if (!inherits(time_vec, "POSIXt")) {
    stop("The specified time column must be of class POSIXct/POSIXt.")
  }
  if (!all(c("air_start", "air_end") %in% names(air_cal_df))) {
    stop("air_cal_df must contain 'air_start' and 'air_end' columns.")
  }

  # Extract and sanity-check intervals
  air_start <- air_cal_df$air_start
  air_end   <- air_cal_df$air_end

  if (!inherits(air_start, "POSIXt") || !inherits(air_end, "POSIXt")) {
    stop("'air_start' and 'air_end' in air_cal_df must be POSIXct/POSIXt.")
  }
  if (any(air_end < air_start, na.rm = TRUE)) {
    stop("Found intervals where air_end < air_start in air_cal_df.")
  }

  # Initialize result vector
  air_cal <- rep(FALSE, length(time_vec))

  # For each interval, mark times that fall within [air_start, air_end]
  # This is simple and clear; for very large data, consider interval trees.
  for (i in seq_along(air_start)) {
    if (is.na(air_start[i]) || is.na(air_end[i])) {
      next
    }
    in_interval <- time_vec >= air_start[i] & time_vec <= air_end[i]
    air_cal[in_interval] <- TRUE
  }

  # Attach the flag to the data frame
  df$air_cal <- air_cal
  df
}

#' Timeseries plot
#'
#' Converts the given parameter to a timeseries
#' and plots with `dygraphs`
#'
#' @param data A dataframe
#' @param field Character string of name of column.
#' @param timestamp Character string of name timestamp column.
#'
#' @return Timeseries plot
#' @export
#'
plot_ts <- function(data, field, timestamp = "timestamp", title = field, GMT = TRUE) {
  field_ts <- xts::as.xts(data[[field]], data[[timestamp]])
  dygraphs::dygraph(field_ts, group = "massplot", main = title) |>
    dygraphs::dyOptions(useDataTimezone = GMT) |>
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
