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
                "RCurl", "readr", "archive", "sf", "stars", "ows4R", "ggplot2"))                                                    # RCurl will later be used in read_bfs_statpop_data()

# packages required for script functionality
load_packages(c("devtools", "ggh4x", "scales", "lemon", "openair", "shades", 
                "wesanderson", "rOstluft.plot"))                                                                          # rOstluft.plot see: https://github.com/Ostluft/rOstluft.plot

# load functions 
devtools::load_all()

# read ressources for input datasets
ressources <- 
  readr::read_delim("inst/extdata/meta/ressources.csv", delim = ";") |> 
  dplyr::mutate(
    get = dplyr::case_when(
      stringr::str_detect(DOWNLOAD_URL, "inst/extdata") ~ paste(DOWNLOAD_URL, DATASET_NAME, sep = "/"),
      DOWNLOAD_SOURCE == "swisstopo" ~ DATASET_NAME,
      TRUE ~ DOWNLOAD_URL
    )
  )


# map projection CRS = CH1903+ / LV95 throughout analysis
crs <- 2056

# map boundaries Canton Zürich and municipalities
map_municipalities <- get_geolion_wfs(wfs = filter_ressources(ressources, 11), version = "2.0.0", crs = crs) 
update_log(11)
map_canton <- aggregate_map(map_municipalities)

# ggplot() +
#   ggplot2::geom_sf(data = map_municipalities) +
#   ggplot2::theme_void()

# ggplot() +
#   ggplot2::geom_sf(data = map_canton) +
#   ggplot2::theme_void()

# clean up
rm(list = c("load_packages"))
