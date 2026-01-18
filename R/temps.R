#' Read temp logger data file
#'
#' @param filename
#'
#' @export
#'
# read_temps.R

read_temps <- function(filename) {
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required but not installed.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }

  expected_cols <- c(
    "date", "time", "ap", "pressure", "water_flow", "air_flow",
    "bucket", "sock", "beaker", "room", "electronics",
    "box", "housing", "cartridge", "thermistor", "humidity"
  )

  # Explicit column specification: allow up to 16 columns, humidity last
  col_spec <- readr::cols(
    X1  = readr::col_character(),  # date
    X2  = readr::col_character(),  # time
    X3  = readr::col_character(),  # ap
    X4  = readr::col_double(),     # pressure
    X5  = readr::col_double(),     # water_flow
    X6  = readr::col_double(),     # air_flow
    X7  = readr::col_double(),     # bucket
    X8  = readr::col_double(),     # sock
    X9  = readr::col_double(),     # beaker
    X10 = readr::col_double(),     # room
    X11 = readr::col_double(),     # electronics
    X12 = readr::col_double(),     # box
    X13 = readr::col_double(),     # housing
    X14 = readr::col_double(),     # cartridge
    X15 = readr::col_double(),     # thermistor
    X16 = readr::col_double()      # humidity (optional, last)
  )

  # Read with read_table2 (handles irregular whitespace)
  df_raw <- readr::read_table2(
    file      = filename,
    col_names = FALSE,
    col_types = col_spec
  )

  # Drop rows with parsing problems (includes "too few columns")
  probs <- readr::problems(df_raw)
  if (nrow(probs) > 0) {
    # rows are 0-based in problems(); convert to 1-based
    bad_rows <- unique(probs$row + 1L)
    df_raw <- df_raw[-bad_rows, , drop = FALSE]
  }

  # Re-check after dropping bad rows
  if (nrow(df_raw) == 0L) {
    stop("No valid rows after removing lines with parsing problems.")
  }

  n_used <- ncol(df_raw)

  # We expect at least 15 columns (no humidity) and at most 16 (with humidity)
  if (n_used < 15) {
    stop("Input file has fewer than 15 columns per remaining row; cannot parse expected structure.")
  }

  # Mandatory first 15 columns
  df <- df_raw[, 1:15, drop = FALSE]
  names(df) <- expected_cols[1:15]

  # Optional humidity column
  if (n_used >= 16) {
    humidity_col <- df_raw[[16]]
    has_humidity_data <- any(!is.na(humidity_col))
    if (has_humidity_data) {
      df$humidity <- humidity_col
    } else {
      df$humidity <- NA_real_
    }
  } else {
    df$humidity <- NA_real_
  }

  # Final column order
  df <- df[expected_cols]

  # Timestamp
  df$timestamp <- as.POSIXct(
    paste(df[["date"]], df[["time"]], df[["ap"]]),
    format = "%m/%d/%Y %I:%M:%S %p",
    tz = "GMT"
  )

  df
}

plot_temps <- function(data) {

}

plot_temp_flow <- function(data) {



}
