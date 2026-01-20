# Get and combine cruise data

# libraries
library(tidyverse)
library(eimsr)
library(data.table)
library(here)

data_dir = "/Users/brett/Documents/AR99"

# Get Underway data
# Shipboard
# underway_dir <- "/Volumes/data_on_memory/underway/proc"
# post cruise
underway_dir <- "/Users/brett/Documents/AR99/underway/proc"
underway_files <- list.files(underway_dir, "AR\\d{6}_.+\\.csv",
                             full.names = TRUE)
underway <- map_dfr(underway_files, read_underway)

# Get EIMS data

air_cal <- read_air_calibrations(file.path(data_dir, "MasterAirAR99.txt"))
eims_dir <- "/Users/brett/Documents/AR99/ar99_eims"
eims_files <- list.files(eims_dir, "202601\\d{2}ar99.+\\.csv",
                             full.names = TRUE)
eims <- map_dfr(eims_files, read_eims)

eims <- add_air_cal_flag(eims, air_cal, time_col = timestamp)

# Get EIMS temp data
temps_dir <- "/Users/brett/Documents/AR99/ar99_temp"
temps_files <- list.files(temps_dir, "EIMSTemp\\d{5}\\.\\d{4}",
                             full.names = TRUE)
temps <- map(temps_files, read_temps) |>
  list_rbind()

# Get optode data
optode_dir <- "/Users/brett/Documents/AR99/ar99_optode"
optode_files <- list.files(optode_dir, "202601\\d{2}T\\d{6}\\.txt",
                             full.names = TRUE)
optode <- map_dfr(optode_files, read_optode)

  # Join data
eims_underway <- data.table(eims)[data.table(underway),
                                  on = .(timestamp), roll = TRUE]
e_u_optode <- data.table(optode)[eims_underway, on = .(timestamp), roll = TRUE]
e_u_opt_temp <- data.table(temps)[e_u_optode, on = .(timestamp), roll = TRUE]

write_csv(eims_underway, here("data/AR99_eims_underway.csv"))

