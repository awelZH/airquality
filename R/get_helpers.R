


## Macht es hier vielleicht Sinn, den download und lese-Schritt zu trennen?
## am Schluss könnte man einen cleanup-Schritt machen umd die ganzen ordner wieder zu löschen 

# courtesy of Statistikamt, modified
get_bfs_statpop_rasterdata <- function(year, path_destination, boundary) {
  

  # download the ZIP file to a temporary location
  temp <- tempfile(tmpdir = path_destination, fileext = ".zip")
  command <- paste0("curl ", download_url, " --output ", temp)
  system(command, intern = TRUE)
  
  # list files within the ZIP archive
  files_in_zip <- 
    archive::archive(temp) %>% 
    dplyr::mutate(path_lower = tolower(path)) %>% 
    dplyr::filter(stringr::str_detect(path_lower, "^.*statpop\\d{4}\\.csv$")) %>% 
    dplyr::select(path) %>% 
    dplyr::pull(path)
  
  # Select the file that matches the pattern "STATPOP##.csv" (case-insensitive)
  # Assuming there's only one such file per archive
  target_file <- files_in_zip[stringr::str_detect(tolower(files_in_zip), "statpop\\d{4}\\.csv")]
  
  if (length(target_file) == 0) {
    stop("No file matching 'STATPOP[year].csv' pattern found in the ZIP archive.")
  }
  
  # Assuming the first match is the file we want (if there are multiple matches)
  largest_file <- target_file[1]
  
  # Open a connection to the matched file inside the ZIP
  con <- archive::archive_read(temp, file = largest_file)
  
  # Read the file into a stars raster
  data <- read_statpop_csv(con, year = extract_year(largest_file))
  
  # crop to boundary
  data <- sf::st_crop(data, boundary)
  
  # Remove the temporary file
  unlink(temp)
  
  return(data)
}





get_bafu_zip_shp <- function(url, path_destination) {
  
  # download to temp zip
  temp <- tempfile(tmpdir = path_destination, fileext = ".zip")
  download.file(url = url, dest = temp)
  
  # unzip
  files <- unzip(temp, list = TRUE)
  unzip(temp, exdir = path_destination)
  
  # delete temp zip
  unlink(temp)
  
  # read shapefile
  shp <- sf::read_sf(fs::path(path_destination, files$Name[stringr::str_detect(files$Name, ".shp")]))
  
  # delete unzipped files
  unlink(fs::path(path_destination, files$Name))
  
  return(shp)
} 




get_crop_all_bafu_rasterdata <- function(id, boundary, path = "inst/extdata") {
  
  meta <- find_swisstopo_metadata(id)
  data <- get_bafu_zip_shp(meta$url, path_destination = path)
  data <- dplyr::filter(data, sf::st_intersects(data, boundary, sparse = FALSE))
  
  # FIXME: work in progress, see issue 13 => anpassen, wenn mehrere Jahre vorliegen
  data <- list("2020" = data)
  
  return(data)
}



get_geolion_wcs <- function(coverage, capabilities, name, na_value = c(0,-999), divisor = 10, crs = 2056) {
  
  chla <- capabilities$findCoverageSummaryById(coverage)
  # des <- chla$getDescription()
  # des$rangeType$field$nilValues
  data <- 
    chla$getCoverage() %>% 
    stars::st_as_stars() %>% 
    sf::st_set_crs(value = crs)
  data <- setNames(data, "value")
  data <-
    data %>% 
    dplyr::mutate(
      value = ifelse(value %in% na_value, NA, value),
      value = value / divisor
    )
  data <- setNames(data, name)
  
  return(data)
}



get_aggregate_aq_rasterdata <- function(coverage, capablilitylist, maplist, parameter, grid, boundary) {
  
  capabilities <- extract_from_capabilitylist(capablilitylist, maplist, coverage, parameter)
  data <- get_geolion_wcs(coverage, capabilities, parameter) 
  data <- aggregate_to_grid(data, grid, parameter, boundary)
  
  return(data)
}




get_all_aq_rasterdata <- function(parameter, maps, capabilitylist, grid, boundary) {
  
  #FIXME: matching anpassen, wenn wcs Jahreskarten funktionieren (O3p98)
  parameter2 <- dplyr::case_when(
    parameter == "NO2" ~ "no2",
    parameter == "PM10" ~ "10",
    parameter == "PM2.5" ~ "25",
    parameter == "eBC" ~ "bc",
    TRUE ~ parameter
  )
  data <- lapply(maps[stringr::str_detect(maps, parameter2)], function(coverage) get_aggregate_aq_rasterdata(coverage, capabilitylist, maps, parameter, grid, boundary))
  
  return(data)
}








