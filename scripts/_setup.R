# loading packages and functions:
# ---
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
load_packages(c("tibble", "tidyr", "dplyr", "purrr", "stringr", "rlang", "rjson", "httr2", "lubridate",
                "RCurl", "readr", "archive", "sf", "stars", "ows4R", "ggplot2"))

# packages required for script functionality
load_packages(c("devtools", "ggh4x", "scales", "lemon", "openair", "shades", 
                "wesanderson", "rOstluft.plot"))

# load functions 
devtools::load_all()



# reading input data for several scripts:
# ---
# read ressource table for input datasets
ressources <- 
  read_local_csv("inst/extdata/meta/ressources.csv") |> 
  prepare_ressources()

# map projection CRS = CH1903+ / LV95 throughout analysis
crs <- 2056

# map boundaries Canton Zürich and municipalities
map_municipalities <- read_geolion_wfs(filter_ressources(ressources, 11), version =  "2.0.0", crs = crs); update_log(11)
map_canton <- aggregate_map(map_municipalities)

# ggplot() +
#   ggplot2::geom_sf(data = map_municipalities) +
#   ggplot2::theme_void()
#
# ggplot() +
#   ggplot2::geom_sf(data = map_canton) +
#   ggplot2::theme_void()




# clean up:
# ---
rm(list = c("load_packages"))
