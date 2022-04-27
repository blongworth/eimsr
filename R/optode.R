#' Read and parse optode data
#'
#' @param optode_file Optode data file
#'
#' @export
#'
read_optode <- function(optode_file) {
  optode_cols <- c("timestamp", "record", "sensor_status", "O2Concentration",
                   "o2_stat", "air_saturation", "sat_stat", "temperature",
                   "temp_stat", "CalPhase", "CP_stat", "TCPhase", "TCP_stat",
                   "C1RPh", "C1RPh_stat", "C2RPh", "C2RPh_stat", "C1Amp",
                   "C1A_stat", "C2Amp", "C2A_stat", "RawTemp", "RT_stat")
  optode_spec <- cols(
    timestamp = col_datetime(format = ""),
    record = col_double(),
    sensor_status = col_character(),
    O2Concentration = col_double(),
    o2_stat = col_logical(),
    air_saturation = col_double(),
    sat_stat = col_logical(),
    temperature = col_double(),
    temp_stat = col_logical(),
    CalPhase = col_double(),
    CP_stat = col_logical(),
    TCPhase = col_double(),
    TCP_stat = col_logical(),
    C1RPh = col_double(),
    C1RPh_stat = col_logical(),
    C2RPh = col_double(),
    C2RPh_stat = col_logical(),
    C1Amp = col_double(),
    C1A_stat = col_logical(),
    C2Amp = col_double(),
    C2A_stat = col_logical(),
    RawTemp = col_double(),
    RT_stat = col_logical(),
    X24 = col_logical()
  )
  optode <- readr::read_lines(optode_file)
  data_start <- purrr::detect_index(optode,
                                    stringr::str_starts, "\\d{4}-\\d{2}-\\d{2}")
  optode_data <- readr::read_tsv(I(optode), skip = data_start - 1,
                       col_names = optode_cols,
                       col_types = optode_spec)
  optode_data
}

