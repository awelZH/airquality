# compiling available raster data air quality modelling as well as inhabitant population data to derive population exposition
# also compile ecosystem exposition towards nitrogen deposition


# read datasets ...
# ---
# => get all available BAFU air pollutant PM2.5, PM10, NO2, O3 raster data as well as nitrogen deposition including pre-compiled ecosystem exposition data from geo.admin.ch
data_raster_pm25 <- read_bafu_raster_data(filter_ressources(ressources, 15), yearmin = 2010, map_canton) # since statpop raster data only available from 2010 on
data_raster_pm10 <- read_bafu_raster_data(filter_ressources(ressources, 14), yearmin = 2010, map_canton)
data_raster_no2 <- read_bafu_raster_data(filter_ressources(ressources, 13), yearmin = 2010, map_canton)
data_raster_o3mp98 <- read_bafu_raster_data(filter_ressources(ressources, 16), yearmin = 2010, map_canton)
data_raster_ndep <- read_bafu_raster_data(filter_ressources(ressources, 19), map_canton)

years <- unique(as.numeric(names(c(data_raster_pm25, data_raster_pm10, data_raster_no2, data_raster_o3mp98))))

data_raster_aq <- purrr::map(setNames(years, years), function(year) list(
  pm25 = data_raster_pm25[[as.character(year)]]$pm25, 
  pm10 = data_raster_pm10[[as.character(year)]]$pm10,
  no2 = data_raster_no2[[as.character(year)]]$no2,
  mp98 = data_raster_o3mp98[[as.character(year)]]$mp98
))

# => download / read BFS statpop data for same years as pollutant raster data
data_raster_bfs <- lapply(setNames(years, years), function(year) read_statpop_raster_data(year, "inst/extdata", map_canton))

# prepare datasets ...
# ---
# => spatially average pollutant raster data to the grid of statpop data (100x100m)
data_raster_aq <- purrr::map2(data_raster_bfs, data_raster_aq, average_to_statpop)

# => derive & add O3 peak-season rasterdata by statistical relationships
source("scripts/_derive_o3_peak-season_rasterdata.R", encoding = "UTF-8")

# => convert pollutant and statpop data into a common tibble & calculate inhabitant exposition per raster cell (will also later be used to derive health outcomes)
data_expo_pop <- prepare_exposition(data_raster_bfs, data_raster_aq, years)

# => get minimum year base-scenario pollutant raster data and average them to the grid ofg statpop data for the remaining years
outcomes_meta <- read_local_csv(filter_ressources(ressources, 24), locale = readr::locale(encoding = "UTF-8"))
base_scenario_year <- get_base_scenario_year(unique(outcomes_meta$base_scenario_year)[1])(years)
data_raster_aq_basescenario <- prepare_rasterdata_aq_base(data_raster_bfs, data_raster_aq, base_scenario_year)

# => also convert base-scenario pollutant and statpop data into a common tibble & calculate inhabitant exposition per raster cell
data_expo_pop_basescenario <- prepare_exposition(data_raster_bfs[as.character(years[years != base_scenario_year])], data_raster_aq_basescenario, years[years != base_scenario_year])

# => join raster and municipality data 
data_expo_municip <- prepare_weighted_mean(data_raster_bfs, data_raster_aq, years, map_municipalities)
data_expo_municip_basescenario <- prepare_weighted_mean(data_raster_bfs[as.character(years[years != base_scenario_year])], data_raster_aq_basescenario, years[years != base_scenario_year], map_municipalities)

# aggregate datasets ...
# ---
# => inhabitant population exposition distribution by concentration class
data_expo_population_dist <- aggregate_population_exposition_distrib(data_expo_pop)

# => sensitive ecosystem reactive nitrogen deposition exposition: distribution across all sensitive ecosystems
data_raster_ndep <- bafu_rasterlist_to_tibble(data_raster_ndep)
data_expo_ecosys_dist <- aggregate_ndep_exposition_distrib(data_raster_ndep) 

# => population-weighted mean values per year, pollutant and municipality / canton (including base-scenario)
data_pop_weighted_mean <- list(canton = 
                                 aggregate_population_weighted_mean(data_expo_municip_basescenario) |> 
                                 dplyr::rename(population_weighted_mean_base = population_weighted_mean) |> 
                                 dplyr::select(year, pollutant, metric, parameter, population_weighted_mean_base) |> 
                                 dplyr::mutate(base_year = base_scenario_year) |> 
                                 dplyr::right_join(aggregate_population_weighted_mean(data_expo_municip), by = c("year", "pollutant", "metric", "parameter")) |> 
                                 dplyr::arrange(year, pollutant)
)
data_pop_weighted_mean$munipalities <- aggregate_population_weighted_mean(data_expo_municip, groups = c("year", "pollutant", "metric", "parameter", "geodb_oid", "gemeindename"))
data_pop_weighted_mean$munipalities <- dplyr::mutate(data_pop_weighted_mean$munipalities , population_weighted_mean = ifelse(is.na(gemeindename), NA, population_weighted_mean)) # lakes = NA


# write output datasets & clean up:
# ---
write_local_csv(data_pop_weighted_mean$canton, file = "inst/extdata/output/data_exposition_weighted_means_canton.csv")
write_local_csv(data_pop_weighted_mean$munipalities, file = "inst/extdata/output/data_exposition_weighted_means_municipalities.csv")
write_local_csv(data_expo_population_dist, file = "inst/extdata/output/data_exposition_distribution_pollutants.csv")
write_local_csv(data_expo_ecosys_dist, file = "inst/extdata/output/data_exposition_distribution_ndep.csv")
rm(list = c("cov_stack", "availability", "years", "data_raster_bfs", "data_raster_aq", "data_raster_ndep", "data_expo_municip", 
            "data_expo_population_dist", "data_expo_ecosys_dist", "data_pop_weighted_mean", "map_canton", "data_raster_pm25", "data_raster_pm10", 
            "data_raster_no2", "data_raster_o3mp98"))
