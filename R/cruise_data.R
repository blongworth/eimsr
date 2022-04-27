# Get and combine cruise data

get_underway <- function(underway_dir) {
  underway_files <- list.files(underway_dir, "AR\\d{6}_.+\\.csv",
                               full.names = TRUE)
  map_dfr(underway_files, read_underway)
}

get_eims <- function(eims_dir) {
  eims_files <- list.files(eims_dir, "202204\\d{2}ar66b.+\\.csv",
                           full.names = TRUE)
  map_dfr(eims_files, read_eims)
}

get_temps <- function(temps_dir) {
  temps_files <- list.files(temps_dir, "EIMSTemp\\d{5}\\.\\d{4}",
                            full.names = TRUE)
  map_dfr(temps_files, read_temps)
}

get_optode <- function(optode_dir) {
  optode_files <- list.files(optode_dir, "202204\\d{2}T\\d{6}\\.txt",
                             full.names = TRUE)
  optode <- map_dfr(optode_files, read_optode)
}

join_cruise_data <- function(eims, temps, optode, underway) {
 eims_underway <- data.table(eims)[data.table(underway), on = .(timestamp), roll = TRUE]
 e_u_t <- eims_underway[data.table(temps), on = .(timestamp), roll = TRUE]
 e_u_t[data.table(optode), on = .(timestamp), roll = TRUE]
}
