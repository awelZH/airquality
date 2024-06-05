

download_boundaries_geolion <- function(wfs, version = "2.0.0", crs = 2056) {
  
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
