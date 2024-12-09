read_statpop_raster_data <- function(year, destination_path, boundary, crs = 2056){

  # download zip
  download_statpop_data(year, destination_path, file_filter = paste0("STATPOP", year, "\\.csv"))
  
  # read and georeference file
  file_to_read <- list.files(
    destination_path, 
    pattern = paste0("STATPOP", year, "\\.csv"), 
    full.names = TRUE,
    recursive = TRUE
  )
  data_stars <- read_statpop_csv(file_to_read, year, crs = crs)
  
  # delete csv file
  unlink(file_to_read)
    
  # crop to boundary
  data_stars <- sf::st_crop(data_stars, boundary)

  return(data_stars)
}


read_bafu_raster_data <- function(id, boundary, crs = 2056){
  
  download_url <- get_swisstopo_metadata(id)
  years <- extract_year(download_url)
  download_url <- setNames(download_url, years)
 
  # FIXME: read_stars returns a curvilinear LV95 grid in this case which creates problems later on (?)
  data <-
    years |> 
    as.character() |> 
    purrr::map(function(yr) {
      
      data <- 
        download_url[[yr]] |>
        stars::read_stars() |>
        sf::st_transform(crs = sf::st_crs(crs))
      
      names(data) <- "ndep_exmax" # FIXME: derive from data
      
      # crop to boundary
      # FIXME regular grid workaround: 
      data <- 
        data |> 
        tibble::as_tibble() |> 
        stars::st_as_stars() |> 
        sf::st_set_crs(value = crs) |> 
        sf::st_crop(boundary)
      
      # convert to tibble
      data <- 
        data |> 
        tibble::as_tibble() |> 
        na.omit() |> 
        dplyr::mutate(
          year = as.numeric(yr),
          source = "BAFU"
        )
      
      return(data)
    }) |> 
    dplyr::bind_rows()
  
  return(data)
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


read_geolion_wfs <- function(apiurl, version = "2.0.0", crs = 2056){
  
  request <- get_geolion_wfs_metadata(apiurl, version = version, crs = crs)
  data <- 
    request |> 
    sf::read_sf(type = 6) |> 
    sf::st_transform(crs = sf::st_crs(crs))
  
  return(data)
}


read_geolion_wcs_stack <- function(cov_stack, layer_names, boundary, na_value = c(0, -999)){

  cov_stack_filtered <- cov_stack[sapply(cov_stack, function(x) x$CoverageId %in% layer_names)]
  data_list <- lapply(cov_stack_filtered, function(x) read_single_pollutant_wcs(x, na_value))
  
  list_names <- gsub("pm-", "pm", layer_names)
  list_names <- gsub("jahre-", "", list_names)
  names(data_list) <- list_names
  
  return(data_list)
}


filter_availability <- function(cov_stack, years_pollumap = 2015) {
  
  data_availability <- 
    cov_stack |> 
  to_stack_df() |> 
    dplyr::filter(
      (!stringr::str_detect(layer_name, "jahre") & as.numeric(year) %in% years_pollumap) | # only select pollumap for the year in which it calibrated with monitoring data
        as.numeric(year) < lubridate::year(Sys.Date()) & # no future pollumap projections
        stringr::str_detect(layer_name, "jahre") # apart from that: always use jahreskarte
    ) |> 
    dplyr::filter(pollutant != "bc") # no bc since this only available for pollumap
  
  return(data_availability)
}








