

#' just to bring ist into required structure
#'
#' @param years
#' @param data_raster_pm25
#' @param data_raster_pm10
#' @param data_raster_no2
#' @param data_raster_o3mp98
#' @param data_raster_ndep
#'
#' @keywords internal
combine_raster_aq <- function(years, data_raster_pm25, data_raster_pm10, data_raster_no2, data_raster_o3mp98, data_raster_ndep) {

  data_raster_aq <-
    setNames(years$all, years$all) |>
    purrr::map(function(year) list(
      pm25 = data_raster_pm25[[as.character(year)]]$pm25,
      pm10 = data_raster_pm10[[as.character(year)]]$pm10,
      no2 = data_raster_no2[[as.character(year)]]$no2,
      mp98 = data_raster_o3mp98[[as.character(year)]]$mp98
    )) |>
    purrr::map(function(x) x[which(!sapply(x, is.null))])

  return(data_raster_aq )
}

