read_statpop_raster_data <- function(year, destination_path, crs = 2056, boundary){
  
  file_to_read <- list.files(
    destination_path, 
    pattern = paste0("STATPOP", year, "\\.csv"), 
    full.names = TRUE,
    recursive = TRUE
  )
  
  var <- paste0("B", year %% 100, "BTOT")
  delim <- ifelse(as.numeric(year) > 2015, ";", ",")
  data <- readr::read_delim(
    file_to_read,
    delim = delim, 
    col_select = c(RELI, E_KOORD, N_KOORD, !!var),
    locale = readr::locale(encoding = "UTF-8")
  ) %>% 
    dplyr::rename(population = !!var)
  
  
  data_stars <- data %>% 
    sf::st_as_sf(
      coords = c("E_KOORD", "N_KOORD"), 
      dim = "XY",
      crs = sf::st_crs(crs)
    ) %>%
    stars::st_rasterize() 
  
  data_stars_zh <- sf::st_crop(data_stars, boundary)
  
  unlink(destination_path, recursive = TRUE)
  
  return(data_stars_zh)
}


read_bafu_raster_data <- function(destination_path, boundary){
  
  file_to_read <- list.files(
    destination_path, 
    pattern = "\\.shp$", 
    full.names = TRUE,
    recursive = TRUE
  )
  
  data_sf <- sf::read_sf(file_to_read)
  
  data_sf_zh <- dplyr::filter(
    data_sf, 
    sf::st_intersects(data, boundary, sparse = FALSE)
  )
  
  return(data_sf_zh)
}


read_opendataswiss <- function(url, source){

  read_url <- get_opendataswiss_metadata(url)
  data <- purrr::map_df(read_url, function(x) readr::read_delim(x, delim = ","))
  data <- dplyr::mutate(data, source = source)
  
  return(data)
}


read_local_csv <- function(file, delim = ";", locale = readr::locale(encoding = "latin1", tz = "Etc/GMT-1"), ...){
  
  data <- readr::read_delim(file, delim = delim, locale = locale, ...)
  
  return(data)
}


read_pollutant_wcs_stack <- function(wcs_layer, years = NA, na_value = c(0, -999)){
  
  client <- ows4R::WCSClient$new(wcs_layer, serviceVersion = "2.0.1")
  
  cap <- client$getCapabilities()
  cov <- cap$getCoverageSummaries()
  
  cov_ids <- sapply(cov, function(x) x$CoverageId)
  
  if(!is.na(years)){
    cov_ids <- cov_ids[grepl(as.character(years), cov_ids)]
  }
  
  cov_list <- lapply(cov_ids, function(x) cap$findCoverageSummaryById(x))
  
  data_list <- lapply(cov_list, function(x) read_single_pollutant_wcs(x, na_value))
  
  list_names <- gsub("pm-", "pm", cov_ids)
  list_names <- gsub("jahre-", "", list_names)
  
  names(data_list) <- list_names
  
  return(data_list)
  
}


read_single_pollutant_wcs <- function(coverage, na_value){
  
  data <- coverage$getCoverage() %>% 
    stars::st_as_stars() %>% 
    sf::st_set_crs(value = 2056)
  
  data <- setNames(data, "value")
  data <-
    data %>% 
    dplyr::mutate(
      value = ifelse(value %in% na_value, NA, value),
    )
  
  name <- gsub("\\d{4}|jahre|-", "",coverage$CoverageId)
  
  data <- setNames(data, name)
  
  return(data)
}


read_geolion_wfs <- function(apiurl, version = "2.0.0", crs = 2056){
  
  request <- get_geolion_wfs_metadata(apiurl, version = version, crs = crs)
  data <- 
    request |> 
    sf::read_sf(type = 6) |> 
    sf::st_transform(crs = sf::st_crs(crs))
  
  return(data)
}




