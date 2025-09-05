# loading packages and functions:
# ---

# make sure renv is instaled
if (!("renv" %in% installed.packages())) {install.packages("renv")}

# function to instakll / load packages
load_packages <- function(packages) {
  
  for (package in packages) {
    if (!require(package, character.only = TRUE, quietly = TRUE)) {
      if (package %in% c("rOstluft", "rOstluft.plot", "rOstluft.data")) {
        renv::install(paste0("Ostluft/",package), prompt = FALSE)
      } else if (package %in% c("airquality.methods", "airquality.data")) {
        renv::install(paste0("awelZH/",package), prompt = FALSE)
      } else {
        renv::install(package)
      }
    }
    library(package, character.only = TRUE)
  }
  
}

# make sure airquality.data is installed
load_packages("airquality.data")

# update package airquality.data if newer version available
renv::update("airquality.data", prompt = FALSE)

# packages required for script functionality
imports <- c("devtools", "renv", "airquality.methods", "airquality.data", "scales", "openair", "ggplot2",
             "RColorBrewer", "colorspace", "rmweather", "ranger", "MASS", "rOstluft.plot", "quarto", "kableExtra")
load_packages(imports)
# sapply(imports, function(x) usethis::use_package(x, "Import", min_version = TRUE))

# packages necessary for function functionality
depends <- c("tibble", "tidyr", "dplyr", "purrr", "stringr", "rlang", "rjson", "httr2", "lubridate",
             "readr", "sf", "stars", "withr", "pxR")
load_packages(depends)
# sapply(depends, function(x) usethis::use_package(x, "Suggests", min_version = TRUE))


# reading input data for several scripts:
# ---
# read ressource table for input datasets
ressources <- airquality.methods::prepare_ressources(airquality.methods::read_local_csv("inst/extdata/meta/ressources.csv"))

# read all available raster data?
read_all_raster <- FALSE

# reference year for all pollutants in exposition & outcomes calculation
base_scenario_year <- 2010

# map projection CRS = CH1903+ / LV95 throughout analysis
crs <- 2056

# map boundaries Canton Zürich and municipalities
map_municipalities <- airquality.methods::read_geolion_wfs(filter_ressources(ressources, 11), version =  "2.0.0", crs = crs)
map_canton <- airquality.methods::aggregate_map(map_municipalities)

# ggplot() +
#   ggplot2::geom_sf(data = map_municipalities) +
#   ggplot2::theme_void()
#
# ggplot() +
#   ggplot2::geom_sf(data = map_canton) +
#   ggplot2::theme_void()


# clean up:
# ---
rm(list = c("load_packages", "imports", "depends"))
