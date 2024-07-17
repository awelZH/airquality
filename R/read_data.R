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


read_local_csv <- function(file_name, encoding = "latin1"){
  file <- paste0("inst/extdata/", file_name)
  
  locale <- readr::locale(encoding = encoding)
  
  data <- readr::read_delim(file, delim =";", locale = locale)
  
  return(data)
}
