read_statpop_data <- function(year, destination_path, crs = 2056, boundary){
  
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