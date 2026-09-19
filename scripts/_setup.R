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
             "readr", "sf", "stars", "withr", "pxR", "healthiar", "airquality.methods", "airquality.data", "scales", "openair", "ggplot2",
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
# => general
# last year analysed = current year - year_offset (data of a year usually appear in the following year)
year_offset <- 1
year_last <- lubridate::year(Sys.Date()) - year_offset

# reference year for all pollutants in exposition & outcomes calculation
base_scenario_year <- 2015

# map projection CRS = CH1903+ / LV95 throughout analysis
crs <- 2056

# => emissions
# last year kept in the emission data: EMIKAT projections beyond the current year are dropped, and it is the newest
# vehicle model year in the RSD data
emis_year_max <- lubridate::year(Sys.Date())

# => trends
# years to consider for analysis and later plotting
trend_years <- 1990:year_last

# cantons whose monitoring sites are analysed
trend_cantons <- "ZH"

# parameters for trend analysis
trend_parameters <- c("PM2.5", "PM10", "NOx", "eBC", "NHx", "Ndep", "O3_max_98p_m1")

# meteorological variables (daily) used for meteo-normalisation
trend_vars_d1 <- c("T", "T_max_min10", "Hr", "StrGlo", "p", "WVs", "WD", "RainSum")

# minimum number of years available for trend analysis per site
trend_yearmin_per_site <- 4

# reference year for relative trends with monitoring data
trend_reference_year <- function(parameter, base_year = 2015) {
  dplyr::case_when(
    parameter == "PM2.5" ~ 2021,
    parameter == "eBC" ~ 2020,
    parameter == "NHx" ~ 2020,
    parameter == "NH3" ~ 2020,
    parameter == "Ndep" ~ 2020,
    TRUE ~ base_year
  )
}

# minimum number of sites per year for which median trend is derived
trend_nmin_sites <- function(parameter) {
  dplyr::case_when(
    parameter == "PM10" ~ 3,
    parameter == "PM2.5" ~ 3,
    parameter == "eBC" ~ 2, #! simply not a lot high timeres sites available
    parameter == "NOx" ~ 5,
    parameter == "NO2" ~ 5,
    parameter == "O3" ~ 5,
    parameter == "O3_nb_h1>120" ~ 5,
    parameter == "O3_max_h1" ~ 5,
    parameter == "O3_nb_d1_max_h1>120" ~ 5,
    parameter == "O3_max_98p_m1" ~ 5,
    parameter == "O3_peakseason_mean_d1_max_mean_h8gl" ~ 5,
    parameter == "NHx" ~ 4,
    parameter == "Ndep" ~ 4,
    TRUE ~ 4
  )
}

# => exposition
# years to analyse: STATPOP is available from 2010 on
expo_years <- 2010:year_last

# subtract STATPOP collector pixels (inhabitants that cannot be located) and spread them over their municipality?
expo_correct_noloc <- TRUE

# years in which PM2.5 is derived from PM10 (no PM2.5 raster data before 2015)
expo_years_pm25_from_pm10 <- min(expo_years):2014

# minimum number of monitoring sites per year for fitting the O3 peak-season model
expo_o3_nmin_sites <- 7

# => plots
# years to consider for plotting
plot_years <- 1995:year_last

# number of latest years for plotting relative threshold comparison
plot_n_years <- 3

# parameters to include for timeseries plotting
plot_parameters_timeseries <- c("NO2", "PM10", "PM2.5", "O3_max_98p_m1", "O3_peakseason_mean_d1_max_mean_h8gl", "eBC")

# parameters to include for exposition plotting
plot_parameters_exposition <- c("NO2", "O3_max_98p_m1", "PM10", "PM2.5", "O3_peakseason_mean_d1_max_mean_h8gl")

# reference year for relative emission trends (independent of base_scenario_year)
plot_reference_year_emissions <- 2015


# map boundaries:
# ---

# map boundaries Canton Zürich and municipalities (current boundaries, used for all years;
# without the Kloster Fahr, an enclave of the Canton of Aargau)
map_municipalities <-
  airquality.methods::read_geolion_wfs(filter_ressources(ressources, 11), version =  "2.0.0", crs = crs) |>
  drop_foreign_enclaves()
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
rm(list = c("load_packages", "packages"))
