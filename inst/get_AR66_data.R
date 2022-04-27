# Get and combine cruise data

# libraries
library(tidyverse)
library(eimsr)
library(data.table)
library(here)

# TODO: Make this local to avoid needing network
# lter_stations <- read_csv("https://nes-lter-data.whoi.edu/api/stations/en649.csv")
lter_stations <- read_csv("data/lter_stations.csv")

# Get Underway data
underway_dir <- "//10.100.100.30/data_on_memory/underway/proc"
underway_files <- list.files(underway_dir, "AR\\d{6}_.+\\.csv",
                             full.names = TRUE)
underway <- map_dfr(underway_files, read_underway)

# Get EIMS data
eims_dir <- "C:/Users/brett/Documents/AR66b"
eims_files <- list.files(eims_dir, "202204\\d{2}ar66b.+\\.csv",
                             full.names = TRUE)
eims <- map_dfr(eims_files, read_eims)

# Get EIMS temp data
temps_dir <- eims_dir
temps_files <- list.files(temps_dir, "EIMSTemp\\d{5}\\.\\d{4}",
                             full.names = TRUE)
temps <- map_dfr(temps_files, read_temps)

# Get optode data
optode_dir <- eims_dir
optode_files <- list.files(optode_dir, "202204\\d{2}T\\d{6}\\.txt",
                             full.names = TRUE)
optode <- map_dfr(optode_files, read_optode)

# Join data
eims_underway <- data.table(eims)[data.table(underway),
                                  on = .(timestamp), roll = TRUE]
e_u_optode <- data.table(optode)[eims_underway, on = .(timestamp), roll = TRUE]
e_u_opt_temp <- data.table(temps)[e_u_optode, on = .(timestamp), roll = TRUE]


# Leg B only
eims_underway_b <- e_u_opt_temp %>%
  filter(timestamp > as.POSIXct('2022-04-19 07:00:00', tz='UTC'))

write_csv(eims_underway_b, here("data/AR66b_eims_underway.csv"))
