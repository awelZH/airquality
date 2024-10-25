# compiling available raster data air quality modelling as well as inhabitant population data to derive population exposition
# also compile ecosystem exposition towards nitrogen deposition
# ---
# read datasets ...
# => check which pollutant datasets for which years are available

# => download air pollutant raster data accordingly

download_statpop_data(2020, "inst/extdata")

# => download BFS statpop data for required years

# => download BAFU nitrogen deposition raster data including precompiled ecosystem exposition data





# prepare datasets ...
# => spatially average pollutant raster data to the grid of statpop data (100x100m)
# => join pollutant and statpop data





# aggregate datasets ...





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











