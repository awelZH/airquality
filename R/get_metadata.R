# FIXME: integrate into download_helpers or read_helpers?

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
  meta_url <- gsub("bfs_nr", bfs_nr, "https://www.bfs.admin.ch/bfs/de/home/statistiken/kataloge-datenbanken.assetdetail.bfs_nr.html")
  
  command <- paste0("curl ", meta_url)
  asset_page <- system(command, intern = TRUE)
  
  asset_page_total <- paste0(asset_page, collapse = " ")
  
  #asset_page <- RCurl::getURLContent(meta_url, .encoding = "latin1")
  asset_number <- gsub(".*(https://.*assets/[0-9]+/).*", "\\1", asset_page_total)
  asset_number <- gsub(".*/([0-9]+)/", "\\1", asset_number)
  
  download_url <- paste0("https://www.bfs.admin.ch/bfsstatic/dam/assets/",asset_number,"/master")
  
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


get_geolion_wcs_metadata <- function(apiurl, typ){
  
  client <- ows4R::WCSClient$new("http://wms.zh.ch/ImmissionenZHWCS", serviceVersion = "2.0.1")
  capabilities <- client$getCapabilities()
  
  return(capabilities)
}