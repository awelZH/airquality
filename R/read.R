
#' Reads Swiss BFS life expectancy data ("Kohortensterbetafeln) from official api
#'
#' @param destination_path
#'
#' @export
read_bfs_life_expectancy_data <- function(destination_path = "inst/extdata"){

  # get downoad url from BFS api
  url <- get_bfs_metadata(bfs_nr = "px-x-0102020300_101")

  # download temp file from api
  temp <- download_file(download_url = url, destination_path = destination_path, file_ext = ".px")

  # read *.px
  data <- pxR::read.px(temp, encoding = "UTF-8")

  # delete temp *.px
  unlink(temp)

  return(data)
}

#' Reads and combines all spatial air quality raster data
#'
#' @param ressources
#' @param years
#' @param boundary
#'
#' @export
read_all_raster_data <- function(ressources, years, boundary) {

  print("get PM2.5")
  data_raster_pm25 <- read_bafu_raster_data(filter_ressources(ressources, 15), years_filter = years$PM2.5, boundary)
  print("get PM10")
  data_raster_pm10 <- read_bafu_raster_data(filter_ressources(ressources, 14), years_filter = years$PM10, boundary)
  print("get NO2")
  data_raster_no2 <- read_bafu_raster_data(filter_ressources(ressources, 13), years_filter = years$NO2, boundary) # NO2 may take a while since data from 2020 on are in highres
  print("get O3mp98")
  data_raster_o3mp98 <- read_bafu_raster_data(filter_ressources(ressources, 16), years_filter = years$O3, boundary)
  print("combine")
  years$all <- sort(unique(as.numeric(names(c(data_raster_pm25, data_raster_pm10, data_raster_no2, data_raster_o3mp98)))))
  data_raster_aq <- combine_raster_aq(years, data_raster_pm25, data_raster_pm10, data_raster_no2, data_raster_o3mp98)

  return(data_raster_aq)
}





