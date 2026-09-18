# compiling inhabitant population exposition towards air pollutants (NO2, PM10, PM2.5, O3) from BAFU raster
# data and BFS STATPOP inhabitant raster data, as well as sensitive ecosystem exposition towards nitrogen
# deposition (exceedance of critical loads)
#
# all years are recomputed on every run and the output files are overwritten (downloads are cached by
# airquality.methods, see airquality.methods::geo_admin_cache_dir())
#
# aggregation levels: canton (all cells whose centre lies inside the canton) and municipality (one row per
# bfs number); every cell inside the canton has a municipality (lake cells: nearest municipality), so the
# municipalities add up to the canton
#
# STATPOP collector pixels: the inhabitants BFS cannot locate are subtracted from their collector pixel and
# spread over the inhabited cells of their municipality in proportion to the cells' inhabitants


# settings ...
# ---
# => years to analyse: STATPOP is available from 2010 on, new raster data usually appear late in the following year
years_exposition <- 2010:(lubridate::year(Sys.Date()) - year_offset)


# read datasets ...
# ---
# => inhabitants (STATPOP, collector pixels subtracted if expo_correct_noloc) and pollutant raster data,
#    pollutants averaged onto the 100 m STATPOP grid of the same year
data_raster_expo <- read_exposition_rasters(years_exposition, map_municipalities, correct_noloc = expo_correct_noloc)

# => critical load exceedance for nitrogen in sensitive ecosystems, all available model years
data_ndep <- read_ndep_exceedance(map_municipalities)

# => air quality monitoring data for deriving O3 peak-season and PM2.5 raster data
data_monitoring_aq <- airquality.data::data_monitoring_aq_y1


# prepare datasets ...
# ---
# => one row per inhabited cell and year; each cell gets the municipality its centre lies in
data_expo_cells <-
  data_raster_expo |>
  rasters_to_cells() |>
  assign_municipalities(map_municipalities)

# => give the collector pixel inhabitants back to their municipality (no-op if expo_correct_noloc is FALSE)
data_expo_cells <- redistribute_noloc(data_expo_cells, noloc_from_aligned(data_raster_expo), map_municipalities)

# => derive O3 peak-season concentrations from NO2 by the statistical relationship at monitoring sites
coefs_o3_peakseason <- fit_o3_peakseason_model(data_monitoring_aq)
data_expo_cells <- derive_o3_peakseason(data_expo_cells, coefs_o3_peakseason)

# => derive PM2.5 from PM10 before 2015 using measured PM2.5:PM10 ratios at NABEL sites
ratios_pm <- fit_pm_ratio(data_monitoring_aq)
data_expo_cells <- derive_pm25_from_pm10(data_expo_cells, ratios_pm, years = min(years_exposition):2014)

# => both models are refitted on every run with the current monitoring data (earlier years may shift
#    slightly); log the coefficients of each run to make such shifts traceable
append_log(tidy_derivation_coefficients(coefs_o3_peakseason, ratios_pm), "inst/extdata/log/exposition_derivation_coefficients.csv")

# => long format: one row per cell, year and parameter, with base-scenario concentrations
#    (inhabitants of each year exposed to the concentrations of base_scenario_year)
data_expo <-
  data_expo_cells |>
  cells_to_long() |>
  add_base_scenario(base_scenario_year)


# aggregate datasets ...
# ---
# => population-weighted mean values per year and pollutant: canton (including base scenario) and municipalities
data_expo_weighted_mean_canton <- combine_canton_means(data_expo, base_scenario_year)
data_expo_weighted_mean_municipalities <- aggregate_population_weighted_mean(data_expo, level = "municipality")

# => inhabitant population exposition distribution by concentration class (canton)
data_expo_population_dist <- aggregate_population_exposition_distrib(data_expo)

# => sensitive ecosystem exposition distribution by class of critical load exceedance (canton)
data_expo_ecosys_dist <- aggregate_ndep_exposition_distrib(data_ndep)


# write output datasets & clean up:
# ---
# => inhabitant counts rounded to whole persons (redistributed collector inhabitants are fractional)
airquality.methods::write_local_csv(round_population(data_expo_weighted_mean_canton), file = "inst/extdata/output/data_exposition_weighted_means_canton.csv")
airquality.methods::write_local_csv(round_population(data_expo_weighted_mean_municipalities), file = "inst/extdata/output/data_exposition_weighted_means_municipalities.csv")
airquality.methods::write_local_csv(round_population(data_expo_population_dist), file = "inst/extdata/output/data_exposition_distribution_pollutants.csv")
airquality.methods::write_local_csv(data_expo_ecosys_dist, file = "inst/extdata/output/data_exposition_distribution_ndep.csv")

rm(list = c("years_exposition", "data_raster_expo", "data_ndep", "data_monitoring_aq", "data_expo_cells",
            "coefs_o3_peakseason", "ratios_pm", "data_expo", "data_expo_weighted_mean_canton",
            "data_expo_weighted_mean_municipalities", "data_expo_population_dist", "data_expo_ecosys_dist"))
