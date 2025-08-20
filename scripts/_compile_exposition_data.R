# compiling available raster data air quality modelling as well as inhabitant population data to derive population exposition
# also compile ecosystem exposition towards nitrogen deposition


# => check which year for each parameter is last available and which might be added, always include base_scenario_year for pollutants
years <- airquality.methods::get_years(read_all_raster, lubridate::year(Sys.Date()) - 2, base_scenario_year)

if ((length(years$all) == 1 & !years$base_analysed) | length(years$all) > 1) {
  
  # read datasets ...
  # ---
  # => get all available BAFU air pollutant PM2.5, PM10, NO2, O3 raster data as well as nitrogen deposition including pre-compiled ecosystem exposition data from geo.admin.ch for specified years
  data_raster_aq <- airquality.methods::read_all_raster_data(ressources, years, map_canton)
  data_raster_ndep <- airquality.methods::read_bafu_raster_data(airquality.methods::filter_ressources(ressources, 19), years_filter = years$ndep_exmax, map_canton) # ndep is already exposition data and exists from 1990 on
  years$all <- as.numeric(names(data_raster_aq))
  
  # => download / read BFS statpop data for same years as pollutant raster data
  data_raster_bfs <- purrr::map(setNames(years$all, years$all), function(year) airquality.methods::read_statpop_raster_data(year, "inst/extdata", map_canton))

  # prepare datasets ...
  # ---
  # => spatially average pollutant raster data to the grid of statpop data (100x100m)
  data_raster_aq <- purrr::map2(data_raster_bfs, data_raster_aq, airquality.methods::average_to_statpop)
  
  # => derive & add O3 peak-season rasterdata by statistical relationships
  source("scripts/_derive_o3_peak-season_rasterdata.R", encoding = "UTF-8")
  
  # => derive PM2.5 raster data from PM10 raster data before 2015 using measured PM2.5:PM10 ratios
  if (any(years$all %in% 2010:2014)) {source("scripts/_derive_pm25_rasterdata.R", encoding = "UTF-8")}
  
  # if already analysed: exclude base_scenario_year from analysis
  if (years$base_analysed) {years$all <- years$all[years$all != base_scenario_year]}
  
  # => convert pollutant and statpop data into a common tibble & calculate inhabitant exposition per raster cell (will also later be used to derive health outcomes)
  data_expo_pop <- airquality.methods::prepare_exposition(data_raster_bfs, data_raster_aq, years$all)
  
  # => get base-scenario pollutant raster data and average them to the grid of statpop data for the remaining years
  data_raster_aq_basescenario <- airquality.methods::prepare_rasterdata_aq_base(data_raster_bfs, data_raster_aq, base_scenario_year)
  
  # => also convert base-scenario pollutant and statpop data into a common tibble & calculate inhabitant exposition per raster cell
  data_expo_pop_basescenario <- airquality.methods::prepare_exposition(data_raster_bfs[as.character(years$all[years$all != base_scenario_year])], data_raster_aq_basescenario, years$all[years$all != base_scenario_year])
  
  # => join raster and municipality data 
  data_expo_municip <- airquality.methods::prepare_weighted_mean(data_raster_bfs, data_raster_aq, years$all, map_municipalities)
  data_expo_municip_basescenario <- airquality.methods::prepare_weighted_mean(data_raster_bfs[as.character(years$all[years$all != base_scenario_year])], data_raster_aq_basescenario, years$all[years$all != base_scenario_year], map_municipalities)
  
  # aggregate datasets ...
  # ---
  # => inhabitant population exposition distribution by concentration class
  data_expo_population_dist <- airquality.methods::aggregate_population_exposition_distrib(data_expo_pop)
  
  # => sensitive ecosystem reactive nitrogen deposition exposition: distribution across all sensitive ecosystems
  if (length(data_raster_ndep) > 0) {
    data_raster_ndep <- airquality.methods::bafu_rasterlist_to_tibble(data_raster_ndep)
    data_expo_ecosys_dist <- airquality.methods::aggregate_ndep_exposition_distrib(data_raster_ndep) 
  }
  
  # => population-weighted mean values per year, pollutant and municipality / canton (including base-scenario)
  data_pop_weighted_mean <- list(canton = 
                                   airquality.methods::aggregate_population_weighted_mean(data_expo_municip_basescenario) |> 
                                   dplyr::rename(population_weighted_mean_base = population_weighted_mean) |> 
                                   dplyr::select(year, pollutant, metric, parameter, population_weighted_mean_base) |> 
                                   dplyr::mutate(base_year = base_scenario_year) |> 
                                   dplyr::right_join(aggregate_population_weighted_mean(data_expo_municip), by = c("year", "pollutant", "metric", "parameter")) |> 
                                   dplyr::arrange(year, pollutant)
  )
  data_pop_weighted_mean$munipalities <- airquality.methods::aggregate_population_weighted_mean(data_expo_municip, groups = c("year", "pollutant", "metric", "parameter", "bfsnr", "gemeindename"))
  data_pop_weighted_mean$munipalities <- dplyr::filter(data_pop_weighted_mean$munipalities, !is.na(gemeindename)) # lakes = NA
  
  
  # write output datasets & clean up:
  # ---
  airquality.methods::write_local_csv(data_pop_weighted_mean$canton, file = "inst/extdata/output/data_exposition_weighted_means_canton.csv", append = !read_all_raster)
  airquality.methods::write_local_csv(data_pop_weighted_mean$munipalities, file = "inst/extdata/output/data_exposition_weighted_means_municipalities.csv", append = !read_all_raster)
  airquality.methods::write_local_csv(data_expo_population_dist, file = "inst/extdata/output/data_exposition_distribution_pollutants.csv", append = !read_all_raster)
  if (length(data_raster_ndep) > 0) {airquality.methods::write_local_csv(data_expo_ecosys_dist, file = "inst/extdata/output/data_exposition_distribution_ndep.csv", append = !read_all_raster)}
  
}

rm(list = c("years", "data_raster_bfs", "data_raster_aq", "data_raster_ndep", "data_expo_municip", 
            "data_expo_population_dist", "data_expo_ecosys_dist", "data_pop_weighted_mean", "map_canton"))
