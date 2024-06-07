
find_map_geolion <- function(wfs, version = "2.0.0", crs = 2056) {
  
  # query in R, see e.g. here: https://inbo.github.io/tutorials/tutorials/spatial_wfs_services/
  # client <- ows4R::WFSClient$new(files$boundaries$wfs, serviceVersion = "2.0.0")
  # client$getFeatureTypes(pretty = TRUE)
  # client$getCapabilities()
  # client$
  #   getCapabilities()$
  #   getOperationsMetadata()$
  #   getOperations() %>%
  #   purrr::map_chr(function(x){x$getName()})
  # client$
  #   describeFeatureType(typeName = "ms:grenzen") %>%
  #   purrr::map_chr(function(x){x$getName()})
  
  url <- httr2::url_parse(wfs)
  url$query <- list(service = "wfs",
                    version = version,
                    request = "GetFeature",
                    typename = "ms:gem_grenzen", # "ms:gem_seen_grenzen",
                    srsName = paste0("EPSG:", crs)
  )
  request <- httr2::url_build(url)
  
  return(request)
}




find_emikat_opendataswiss <- function(apiurl) {
  
  req <- httr2::request(apiurl)
  req <- httr2::req_perform(req)
  emikat <- httr2::resp_body_json(req)$result        
  emikat <- unlist(purrr::map(emikat$resources, function(x) x$url))
  emikat <- emikat[stringr::str_detect(emikat, ".csv")]
  
  return(emikat)
}





find_rsd_opendataswiss <- function(apiurl) {
  
  req <- httr2::request(apiurl)
  req <- httr2::req_perform(req)
  rsd <- httr2::resp_body_json(req)$result        
  rsd <- unlist(purrr::map(rsd$resources, function(x) x$url))
  rsd <- rsd[stringr::str_detect(rsd, ".csv")]
 
  return(rsd)
}





find_swisstopo_metadata <- function(id){

  metadata_url <- paste0("https://data.geo.admin.ch/api/stac/v0.9/collections/",id,"/items")
  
  metadata <- rjson::fromJSON(file = metadata_url)
  
  features <- metadata$features[[1]]
  
  url <- features$assets[which(grepl("shp", features$assets))][[1]]$href
  
  # FIXME: wrong BAFU metadata => anpassen wenn mehrere Jahre vorhanden sind!
  reference_year <- 2020 # as.Date(feature$properties$datetime)
  swisstopo_metadata <- list(
    url = url,
    reference_year = reference_year 
  )
  
  return(swisstopo_metadata)
}



# courtesy of Statistikamt, modified
get_bfs_statpop_rasterdata <- function(year, path_destination, boundary) {
  
  # derive dataset url
  bfs_nr <- paste0("ag-b-00.03-vz", year, "statpop")
  meta_url <- gsub("bfs_nr", bfs_nr, "https://www.bfs.admin.ch/bfs/de/home/statistiken/kataloge-datenbanken.assetdetail.bfs_nr.html")
  
  command <- paste0("curl ", meta_url)
  asset_page <- system(command, intern = TRUE)
  
  asset_page_total <- paste0(asset_page, collapse = " ")
  
  #asset_page <- RCurl::getURLContent(meta_url, .encoding = "latin1")
  asset_number <- gsub(".*(https://.*assets/[0-9]+/).*", "\\1", asset_page_total)
  asset_number <- gsub(".*/([0-9]+)/", "\\1", asset_number)
  
  download_url <- paste0("https://www.bfs.admin.ch/bfsstatic/dam/assets/",asset_number,"/master")
  
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








