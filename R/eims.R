# Functions for EIMS csv export


read_eims <- function(file, path = eims_dir) {
  eims <- readLines(file.path(path, file))
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

plot_eims <- function(data) {


}
