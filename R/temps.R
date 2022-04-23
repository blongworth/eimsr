#' Read temp logger data file
#'
#' @param filename
#'
#' @export
#'
read_temps <- function(filename) {
  header <- c("date", "time", "ap", "pressure", "water_flow", "air_flow",
              "bucket", "sock", "beaker", "room", "electronics",
              "box", "housing", "cartridge", "thermistor")
  df <- read.table(filename, header = FALSE, col.names = header)
  df$timestamp <- as.POSIXlt(paste(df[["date"]], df[["time"]], df[["ap"]]),
                                format = "%m/%d/%Y %I:%M:%S %p", tz = "GMT")
  df
}

plot_temps <- function(data) {

}

plot_temp_flow <- function(data) {



}
