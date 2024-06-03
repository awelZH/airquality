get_swisstopo_metadata <- function(id){
  # FIXME: anpassen wenn mehrere Jahre vorhanden sind!
  
  
  metadata_url <- paste0("https://data.geo.admin.ch/api/stac/v0.9/collections/",id,"/items")
  
  
  metadata <- rjson::fromJSON(file = metadata_url)
  
  features <- metadata$features[[1]]
  
  url <- features$assets[which(grepl("shp", features$assets))][[1]]$href
  
  datetime <- as.Date(feature$properties$datetime)
  
  swisstopo_metadata <- list(
    url = url,
    datetime = datetime
  )
  
  return(swisstopo_metadata)
}