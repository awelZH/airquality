# compiling available raster data air quality modelling as well as inhabitant population data to derive population exposition
# also compile ecosystem exposition towards nitrogen deposition
# ---
# read datasets ...
# => check for available raster data in geolion "pollumap" & "jahreskarte" wcs datasets
cov_stack <- unlist(lapply(12:16, function(x) get_geolion_wcs_metadata(filter_ressources(ressources, x))), recursive = FALSE)

# => availability matrix, filter for desired source & pollutants & years
availability <- 
  cov_stack |> 
  to_stack_df() |> 
  dplyr::filter(
    (!stringr::str_detect(layer_name, "jahre") & as.numeric(year) == 2015) | # only select pollumap for the year 2015 only this year is calibrated with monitoring data
      as.numeric(year) < lubridate::year(Sys.Date()) & # no future pollumap projections
      stringr::str_detect(layer_name, "jahre") # apart from that: always use jahreskarte
  ) |> 
  dplyr::filter(pollutant != "bc") # no bc since this only available for pollumap

years <- as.numeric(unique(availability$year))

# => get air pollutant raster data accordingly
data_raster_aq <- read_geolion_wcs_stack(cov_stack, availability$layer_name, map_canton)
data_raster_aq <- lapply(setNames(years, years), function(year) data_raster_aq[which(extract_year(names(data_raster_aq)) == year)])

# => download / read BFS statpop data for same years as pollutant raster data
data_raster_bfs <- lapply(setNames(years, years), function(year) read_statpop_raster_data(year, "inst/extdata", map_canton))

# => download BAFU nitrogen deposition raster data including precompiled ecosystem exposition data
# ...




# prepare datasets ...
# => spatially average pollutant raster data to the grid of statpop data (100x100m)
data_raster_aq <- purrr::map2(data_raster_bfs, data_raster_aq, average_to_statpop)

# => convert pollutant and statpop data into a tibble
data_statpop <- 
  years |> 
  as.character() |> 
  purrr::map(function(year) dplyr::mutate(tibble::as_tibble(data_raster_bfs[[year]]), year = as.numeric(year))) |> 
  dplyr::bind_rows()

data_aq <- 
  years |> 
  as.character() |> 
  purrr::map(function(y) {
    purrr::map(data_raster_aq[[y]], function(x) tibble::as_tibble(x[[1]]))
  }) |> 
  dplyr::bind_rows()

data <- dplyr::bind_rows()

# => calculate inhabitant exposition per raster cell





# aggregate datasets ...
# => ...




# write output datasets & clean up:
# ---

















# get all available raster data regarding inhabitant population (from BFS), air pollutants (from geolion) and reactive nitrogen (from data.geo.admin); join population and air pollutant data
get_list <- list(
  pollumap = 12,
  jahreskarte_no2 = 13,
  jahreskarte_pm10 = 14,
  jahreskarte_pm25 = 15,
  jahreskarte_o3 = 16,
  statpop = 20,
  nh3 = 17,
  ndep = 18,
  exmax = 19
)
data_raster <- get_prepare_raster_data(get_list, ressources, map_canton)
pollutants <- names(data_raster)[!(names(data_raster) %in% c("population", "NH3", "Ndep", "Ndep_exceedance"))]

# air pollution inhabitant exposition: population-weighted mean values per municipality (and for the whole canton)
data_weighted_means <- 
  lapply(setNames(pollutants, pollutants), function(pollutant) {
    data <- join_raster_data_with_municipalities(pollutant, data_raster, map_municipalities)
    calc_all_population_weighted_means(pollutant, data, map_municipalities)
  })

# air pollution inhabitant population exposition distribution (over concentration bins and cumulative) for the entire canton
data_expo_distr <- 
  lapply(setNames(pollutants, pollutants), function(pollutant) {
    calc_all_population_expo_distr(pollutant, data_raster[[pollutant]])
  })

# sensitive ecosystem reactive nitrogen deposition exposition: distribution across all sensitive ecosystems
data_expo_distr$Ndep <- calc_all_ndep_ecosystem_expo_distr(data_raster$Ndep_exceedance)

# write output datasets
lapply(pollutants, function(pollutant) extract_weighted_mean_canton(data_weighted_means[[pollutant]], pollutant)) |> 
  dplyr::bind_rows() |>
  readr::write_delim(file = "inst/extdata/output/data_exposition_weighted_means_canton.csv", delim = ";", na = "NA")
update_log(25)

lapply(pollutants, function(pollutant) extract_weighted_mean_municipalities(data_weighted_means[[pollutant]], pollutant)) |> 
  dplyr::bind_rows() |> 
  readr::write_delim(file = "inst/extdata/output/data_exposition_weighted_means_municipalities.csv", delim = ";", na = "NA")
update_log(26)

lapply(pollutants, function(pollutant) extract_exposition_distr_pollutants(data_expo_distr[[pollutant]], pollutant)) |> 
  dplyr::bind_rows() |> 
  readr::write_delim(file = "inst/extdata/output/data_exposition_distribution_pollutants.csv", delim = ";", na = "NA")
update_log(27)

write_local_csv(extract_exposition_distr_ndep(data_expo_distr$Ndep), file = "inst/extdata/output/data_exposition_distribution_ndep.csv")
update_log(28)

# clean up
rm(list = c("data_raster", "data_weighted_means", "data_expo_distr", "pollutants"))











