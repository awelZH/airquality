# load / install packages

load_packages <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE, quietly = TRUE)) {
      install.packages(package)
      library(package, character.only = TRUE)
    }
  }
}

# packages necessary for function functionality

load_packages(c("tibble", "tidyr", "dplyr", "purrr", "stringr", "rlang", "rjson", "lubridate",
    "RCurl", "readr", "archive", "sf", "stars", "ows4R", "ggplot2"))                                                    # RCurl will later be used in read_bfs_statpop_data()

# packages required for script functionality

load_packages(c("devtools", "httr2", "ggh4x", "scales", "lemon", "openair", "shades", 
              "wesanderson", "rOstluft.plot"))                                                                          # rOstluft.plot see: https://github.com/Ostluft/rOstluft.plot

# load functions 

devtools::load_all()
                                                                                                          

# read input dataset table
# FIXME: integrate table / dataset structure
# ...


# map projection CRS = CH1903+ / LV95 throughout analysis

crs <- 2056

# map boundaries Canton Zürich and municipalities

map_municipalities <- 
  download_boundaries_geolion(wfs = files$boundaries$wfs, version = "2.0.0", crs = crs) %>% 
  sf::read_sf(type = 6) %>% 
  sf::st_transform(crs = sf::st_crs(crs))

map_canton <-
  map_municipalities %>%
  sf::st_union() %>%
  sf::st_boundary() %>% 
  sf::st_cast("POLYGON")

# ggplot() +
#   ggplot2::geom_sf(data = map_municipalities) +
#   ggplot2::theme_void()
# 
# ggplot() +
#   ggplot2::geom_sf(data = map_canton) +
#   ggplot2::theme_void()
