
### RSD filter criteria for NOx emission data analysis

rsd_filters <- 
  list(
    nmin = 50, # minimum number of valid records per aggregation
    vehicleyears = 1992:lubridate::year(Sys.Date()), # vehicle model years
    velocityrange = c(5, 60), # range of vehicle velocity in km/h
    accelerationrange = c(-2, 4), # range of vehicle acceleration in km/h/s
    vsprange = c(1, 35), # range of vehicle specific power in n kW/t
    weightmax = 3500 # maximum vehicle unloaded weight in kg
  )

### LRV threshold limit values & WHO air quality guideline values

threshold_values <-
  fs::path("data/input", files$airquality$thresh) %>%
  readr::read_delim(delim = ";",locale = readr::locale(encoding = "UTF-8"))

### map projection CRS = CH1903+ / LV95

crs <- 2056

### map boundaries Canton Zürich and municipalities
### from https://geolion.zh.ch/
### query in R, see e.g. here: https://inbo.github.io/tutorials/tutorials/spatial_wfs_services/

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

url <- httr2::url_parse(files$boundaries$wfs)
url$query <- list(service = "wfs",
                  version = "2.0.0",
                  request = "GetFeature",
                  typename = "ms:gem_grenzen", # "ms:gem_seen_grenzen",
                  srsName = paste0("EPSG:", crs)
)
request <- httr2::url_build(url)
boundaries <- 
  request %>% 
  sf::read_sf(type = 6) %>% 
  sf::st_transform(crs = sf::st_crs(crs))

# ggplot() +
#   geom_sf(data = boundaries) +
#   theme_void()

# boundaries %>%
#   st_union() %>%
#   st_boundary() %>%
#   st_cast("POLYGON") %>%
#   ggplot() +
#   geom_sf() +
#   theme_void()

boundaries_hull <-
  boundaries %>%
  sf::st_union() %>%
  sf::st_boundary() %>% 
  sf::st_cast("POLYGON")
