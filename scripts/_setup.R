# loading packages and functions:
# ---

# make sure renv is instaled
if (!("renv" %in% installed.packages())) {install.packages("renv")}

# function to install / load packages
load_packages <- function(packages) {
  
  for (package in packages) {
    if (!require(package, character.only = TRUE, quietly = TRUE)) {
      if (package %in% c("rOstluft", "rOstluft.plot", "rOstluft.data")) {
        renv::install(paste0("Ostluft/",package), prompt = FALSE)
      } else if (package %in% c("airquality.methods", "airquality.data")) {
        renv::install(paste0("awelZH/",package), prompt = FALSE)
      } else if (package == "healthiar") {
        renv::install(paste0("SwissTPH/",package), prompt = FALSE)
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
packages <- c("devtools", "renv", "tibble", "tidyr", "dplyr", "purrr", "stringr", "rlang", "rjson", "httr2", "lubridate",
             "readr", "sf", "stars", "withr", "healthiar", "airquality.methods", "airquality.data", "scales", "openair", "ggplot2",
             "RColorBrewer", "colorspace", "rmweather", "ranger", "MASS", "rOstluft.plot", "quarto", "kableExtra")
load_packages(packages)
# sapply(imports, function(x) usethis::use_package(x, "Import", min_version = TRUE))

# load local functions
devtools::load_all()


# reading input data for several scripts:
# ---
# read ressource table for input datasets
ressources <- prepare_ressources(airquality.methods::read_local_csv("inst/extdata/meta/ressources.csv"))


# analysis settings (all constants of the analysis in one place, grouped by topic):
# ---
source("scripts/_settings.R", encoding = "UTF-8")


# map boundaries:
# ---

# map boundaries of the municipalities (current boundaries, used for all years;
# without the Kloster Fahr, an enclave of the Canton of Aargau)
map_municipalities <-
  airquality.methods::read_geolion_wfs(filter_ressources(ressources, 11), version =  "2.0.0", crs = crs) |>
  airquality.methods::drop_foreign_enclaves()


# clean up:
# ---
rm(list = c("load_packages", "packages"))
