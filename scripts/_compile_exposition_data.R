# compiling available raster data air quality modelling as well as inhabitant population data to derive population exposition
# also compile ecosystem exposition towards nitrogen deposition


# read datasets ...
# ---
# => check for available raster data in geolion "pollumap" & "jahreskarte" wcs datasets
cov_stack <- unlist(lapply(12:16, function(x) get_geolion_wcs_metadata(filter_ressources(ressources, x))), recursive = FALSE)

# => availability matrix, filtered for desired source & pollutants => years
availability <- filter_availability(cov_stack)
years <- as.numeric(unique(availability$year))

# => get air pollutant raster data from geolion accordingly
data_raster_aq <- read_geolion_wcs_stack(cov_stack, availability$layer_name, map_canton)
data_raster_aq <- lapply(setNames(years, years), function(year) data_raster_aq[which(extract_year(names(data_raster_aq)) == year)])

# => download / read BFS statpop data for same years as pollutant raster data
data_raster_bfs <- lapply(setNames(years, years), function(year) read_statpop_raster_data(year, "inst/extdata", map_canton))

# => download BAFU nitrogen deposition raster data including pre-compiled ecosystem exposition data
data_raster_bafu <- read_bafu_raster_data(filter_ressources(ressources, 19), map_canton)

# prepare datasets ...
# ---
# => spatially average pollutant raster data to the grid of statpop data (100x100m)
data_raster_aq <- purrr::map2(data_raster_bfs, data_raster_aq, average_to_statpop)

# => derive & add O3 peak-season rasterdata by statistical relationships
source("scripts/_derive_o3_peak-season_rasterdata.R", encoding = "UTF-8")

# => convert pollutant and statpop data into a common tibble & calculate inhabitant exposition per raster cell (will also later be used to derive health outcomes)
data_expo_pop <- prepare_exposition(data_raster_bfs, data_raster_aq, years)

# => get minimum year base-scenario pollutant raster data and average them to the grid ofg statpop data for the remaining years
data_raster_aq_basescenario <- prepare_rasterdata_aq_base(data_raster_bfs, data_raster_aq, years)

# => also convert base-scenario pollutant and statpop data into a common tibble & calculate inhabitant exposition per raster cell
data_expo_pop_basescenario <- prepare_exposition(data_raster_bfs[as.character(years[years != min(years)])], data_raster_aq_basescenario, years[years != min(years)])

# => join raster and municipality data 
data_expo_municip <- prepare_weighted_mean(data_raster_bfs, data_raster_aq, years, map_municipalities)
data_expo_municip_basescenario <- prepare_weighted_mean(data_raster_bfs[as.character(years[years != min(years)])], data_raster_aq_basescenario, years[years != min(years)], map_municipalities)

# aggregate datasets ...
# ---
# => inhabitant population exposition distribution by concentration class
data_expo_population_dist <- aggregate_population_exposition_distrib(data_expo_pop)

# => sensitive ecosystem reactive nitrogen deposition exposition: distribution across all sensitive ecosystems
data_expo_ecosys_dist <- aggregate_ndep_exposition_distrib(data_raster_bafu) 

# => population-weighted mean values per year, pollutant and municipality / canton (including base-scenario)
data_pop_weighted_mean <- list(canton = 
                                 aggregate_population_weighted_mean(data_expo_municip_basescenario, groups = c("year", "pollutant")) |> 
                                 dplyr::rename(population_weighted_mean_base = population_weighted_mean) |> 
                                 dplyr::select(year, pollutant, population_weighted_mean_base) |> 
                                 dplyr::mutate(base_year = min(years)) |> 
                                 dplyr::right_join(aggregate_population_weighted_mean(data_expo_municip, groups = c("year", "pollutant")), by = c("year", "pollutant")) |> 
                                 dplyr::arrange(year, pollutant)
)
data_pop_weighted_mean$munipalities <- aggregate_population_weighted_mean(data_expo_municip, groups = c("year", "pollutant", "geodb_oid", "gemeindename"))
data_pop_weighted_mean$munipalities <- dplyr::mutate(data_pop_weighted_mean$munipalities , population_weighted_mean = ifelse(is.na(gemeindename), NA, population_weighted_mean)) # lakes = NA


# write output datasets & clean up:
# ---
write_local_csv(data_pop_weighted_mean$canton, file = "inst/extdata/output/data_exposition_weighted_means_canton.csv")
write_local_csv(data_pop_weighted_mean$munipalities, file = "inst/extdata/output/data_exposition_weighted_means_municipalities.csv")
write_local_csv(data_expo_population_dist, file = "inst/extdata/output/data_exposition_distribution_pollutants.csv")
write_local_csv(data_expo_ecosys_dist, file = "inst/extdata/output/data_exposition_distribution_ndep.csv")
rm(list = c("cov_stack", "availability", "years", "data_raster_bfs", "data_raster_aq", "data_raster_bafu", "data_expo_municip", 
            "data_expo_population_dist", "data_expo_ecosys_dist", "data_pop_weighted_mean", "map_canton"))
