# loading packages and functions:
# ---
# load packages
load_packages <- function(packages) {
  
  if (!("renv" %in% installed.packages())) {install.packages("renv")}
  for (package in packages) {
    if (!require(package, character.only = TRUE, quietly = TRUE)) {
      if (package %in% c("airquality.data", "rOstluft", "rOstluft.plot", "rOstluft.data")) {
        renv::install(paste0("Ostluft/",package))
      } else if (package %in% c("airquality", "airquality.data")) {
        renv::install(paste0("awelZH/",package))
      } else {
        renv::install(package)
      }
    }
    library(package, character.only = TRUE)
  }
  
}

# packages necessary for function functionality
imports <- c("tibble", "tidyr", "dplyr", "purrr", "stringr", "rlang", "rjson", "httr2", "lubridate",
             "readr", "sf", "stars", "withr", "pxR")
load_packages(imports)
# sapply(imports, function(x) usethis::use_package(x, "Import", min_version = TRUE))

# packages required for script functionality
depends <- c("devtools", "renv", "airquality.data", "scales", "lemon", "openair", "ggplot2",
             "RColorBrewer", "colorspace", "MASS", "rOstluft.plot", "quarto")
load_packages(depends)
# sapply(depends, function(x) usethis::use_package(x, "Suggests", min_version = TRUE))

# load functions 
devtools::load_all()


# reading input data for several scripts:
# ---
# read ressource table for input datasets
ressources <- prepare_ressources(read_local_csv("inst/extdata/meta/ressources.csv"))

# read all available raster data?
read_all_raster <- FALSE

# reference year for all pollutants in exposition & outcomes calculation
base_scenario_year <- 2010

# map projection CRS = CH1903+ / LV95 throughout analysis
crs <- 2056

# map boundaries Canton Zürich and municipalities
map_municipalities <- read_geolion_wfs(filter_ressources(ressources, 11), version =  "2.0.0", crs = crs)
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
rm(list = c("load_packages", "imports", "depends"))
