#' @import data.table
NULL

#' Read and parse underway files
#'
#' @param ship_file
#'
#' @return
#' @export
#'
read_underway <- function(ship_file) {
  read_csv(ship_file, skip = 1,
                   col_types = cols(
                     DATE_GMT = col_character(),
                     TIME_GMT = col_time(),
                     Dec_LAT = col_double(),
                     Dec_LON = col_double(),
                     SPD = col_double(),
                     HDT = col_double(),
                     COG = col_double(),
                     SOG = col_double(),
                     WXTP_Ta = col_double(),
                     WXTS_Ta = col_double(),
                     WXTP_Pa = col_double(),
                     WXTS_Pa = col_double(),
                     WXTP_Ri = col_double(),
                     WXTS_Ri = col_double(),
                     WXTP_Rc = col_double(),
                     WXTS_Rc = col_double(),
                     WXTP_Dm = col_double(),
                     WXTS_Dm = col_double(),
                     WXTP_Sm = col_double(),
                     WXTS_Sm = col_double(),
                     WXTP_Ua = col_double(),
                     WXTS_Ua = col_double(),
                     WXTP_TS = col_double(),
                     WXTS_TS = col_double(),
                     WXTP_TD = col_double(),
                     WXTS_TD = col_double(),
                     BAROM_P = col_double(),
                     BAROM_S = col_double(),
                     RAD_SW = col_double(),
                     RAD_LW = col_double(),
                     PAR = col_double(),
                     SBE45S = col_double(),
                     SBE48T = col_double(),
                     FLR = col_double(),
                     FLOW = col_double(),
                     SSVdslog = col_double(),
                     Depth12 = col_double(),
                     Depth35 = col_double(),
                     EM122 = col_double()
                   )) %>%
  mutate(timestamp = as.POSIXct(paste(DATE_GMT, TIME_GMT),
                            format="%Y/%m/%d %H:%M:%S", tz = "GMT"))
}

combine_underway <- function(underway_files) {
  map_dfr(ship_underway, read_underway)
}
