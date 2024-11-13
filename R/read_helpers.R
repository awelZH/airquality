read_statpop_csv <- function(file, year, crs = 2056) {

  var <- ifelse(as.numeric(year) > 2022, "BBTOT", paste0("B", year %% 100, "BTOT")) #FIXME: derive from data itself 
  delim <- ifelse(as.numeric(year) > 2015, ";", ",") #FIXME: derive from data itself 
  
  data <- readr::read_delim(
    file,
    delim = delim, 
    col_select = c(RELI, E_KOORD, N_KOORD, !!var),
    locale = readr::locale(encoding = "UTF-8")
  ) |> 
    dplyr::rename(population = !!var)
  
  data_stars <- 
    data |> 
    sf::st_as_sf(
      coords = c("E_KOORD", "N_KOORD"), 
      dim = "XY",
      crs = sf::st_crs(crs)
    ) |>
    stars::st_rasterize() 

  return(data_stars)
}


read_single_pollutant_wcs <- function(coverage, na_value){

  data <- coverage$getCoverage() %>% 
    stars::st_as_stars() %>% 
    sf::st_set_crs(value = 2056)
  
  divisor <- ifelse(stringr::str_detect(names(data), "jahre"), 1, 10)
  divisor <- ifelse(stringr::str_detect(names(data), "eBC") & !stringr::str_detect(names(data), "jahre"), 100, divisor)
  
  data <- setNames(data, "value")
  data <-
    data %>% 
    dplyr::mutate(
      value = ifelse(value %in% na_value, NA, value),
      value = value / divisor
    )
  
  name <- gsub("\\d{4}|jahre|-", "",coverage$CoverageId)
  
  data <- setNames(data, name)
  
  return(data)
}


to_stack_df <- function(cov_stack){
  
  df <- purrr::map_df(cov_stack, ~data.frame(
    pollutant = gsub("-jahre", "", gsub("^(.*)-.*", "\\1", .x$CoverageId)),
    year = gsub(".*-(\\d{4})$", "\\1", .x$CoverageId),
    layer_name = .x$CoverageId
  )) |> 
    dplyr::mutate(pollutant = gsub("pm-", "pm", pollutant))
  
  return(df)
}


get_swisstopo_metadata <- function(id){
  
  metadata_url <- paste0("https://data.geo.admin.ch/api/stac/v0.9/collections/",id,"/items")
  metadata <- rjson::fromJSON(file = metadata_url)
  features <- metadata$features[[1]]
  url <- features$assets[which(grepl("shp", features$assets))][[1]]$href
  
  # FIXME: wrong BAFU metadata => anpassen wenn mehrere Jahre vorhanden sind!
  reference_year <- 2020 # as.Date(feature$properties$datetime)
  swisstopo_metadata <- list(
    download_url = url,
    reference_year = reference_year 
  )
  
  return(swisstopo_metadata)
}


get_bfs_metadata <- function(year){
  
  # derive dataset url
  bfs_nr <- paste0("ag-b-00.03-vz", year, "statpop")
  base_url <- "https://dam-api.bfs.admin.ch/hub/api/dam/assets"
  
  # Use withr::with_envvar to set no_proxy environment variable
  withr::with_envvar(
    new = c("no_proxy" = "dam-api.bfs.admin.ch"),
    code = {
      # Build the request URL with the order number as a query parameter
      response <- httr2::request(base_url) %>%
        httr2::req_url_query(orderNr = bfs_nr) %>%
        httr2::req_headers(
          "accept" = "application/json",      # Ensure we accept JSON
          "Content-Type" = "application/json" # Request content type is JSON
        ) %>%
        httr2::req_perform()
      
      # Parse the JSON response body into a list
      data <- httr2::resp_body_json(response)
    }
  )
  
  links <- data[["data"]][[1]][["links"]]
  download_url <- links[sapply(links, function(x) grepl("master", x$href))][[1]]$href
  
  return(download_url)
}


get_opendataswiss_metadata <- function(apiurl){
  
  req <- httr2::request(apiurl)
  req_data <- httr2::req_perform(req)
  metadata <- httr2::resp_body_json(req_data)$result        
  links <- unlist(purrr::map(metadata$resources, function(x) x$url))
  download_link <- links[stringr::str_detect(links, ".csv")]
  
  if (any(stringr::str_detect(download_link, "ostluft_emissionsbilanzen"))) { # since this dataset may contain different files from various submissions => use only the latest one
    download_link <- download_link[which.max(extract_year(download_link))]
  }
  
  return(download_link)
}


get_geolion_wfs_metadata <- function(apiurl, type = "ms:gem_grenzen", version = "2.0.0", crs = 2056){
  
  url <- httr2::url_parse(apiurl)
  url$query <- list(service = "wfs",
                    version = version,
                    request = "GetFeature",
                    typename = type, # "ms:gem_seen_grenzen",
                    srsName = paste0("EPSG:", crs)
  )
  request <- httr2::url_build(url)
  
  return(request)
}


get_geolion_wcs_metadata <- function(wcs_stack, version = "2.0.1"){
  
  client <- ows4R::WCSClient$new(wcs_stack, serviceVersion = version)
  cap <- client$getCapabilities()
  cov <- cap$getCoverageSummaries()
  cov_ids <- sapply(cov, function(x) x$CoverageId)
  cov_list <- lapply(cov_ids, function(x) cap$findCoverageSummaryById(x))
  
  return(cov_list)
}


download_zip <- function(download_url, destination_path, file_filter = NULL){

  temp <- tempfile(tmpdir = destination_path, fileext = ".zip")
  
  # if (!dir.exists(destination_path)) {dir.create(destination_path, recursive = TRUE)}

  # op <- options(timeout = 1000,
  #               download.file.method="curl",
  #               download.file.extra = paste0('--noproxy "*"'))
  # 
  # on.exit(options(op))
  
  command <- paste0("curl ", download_url, " --output ", temp)
  system(command, intern = TRUE)
  
  # which files are in there?
  files <- unzip(temp, list = TRUE)
  file <- files$Name[stringr::str_detect(files$Name, file_filter)]

  # unzip specific file
  unzip(temp, files = file, exdir = destination_path, junkpaths = TRUE)
  
  # delete temp zip
  unlink(temp)
}


download_statpop_data <- function(year, destination_path, file_filter = NULL){
  
  download_url <- get_bfs_metadata(year)
  download_zip(download_url, destination_path, file_filter)
  
}


download_bafu_data <- function(id, destination_path, file_filter = NULL){
  
  download_url <- get_swisstopo_metadata(id)$download_url
  download_zip(download_url, destination_path, file_filter)
  
}
