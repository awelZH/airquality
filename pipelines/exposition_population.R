# exposition: inhabitant population exposition towards NO2, PM10, PM2.5 and O3 from BAFU and BFS rasters
# (functions in R/exposition.R; decisions 2-6) -> the 2 weighted-mean files, data_exposition_distribution_pollutants.csv
#
# all years are recomputed when an asset changes; canton and municipalities come from the same cells

pipeline_exposition_population <- tar_plan(
  # state of the raster assets (about 1 s per collection), checked on every run; the cells are rebuilt only
  # when an asset was added, replaced or updated
  tar_target(expo_pop_assets, geo_admin_asset_state(exposition_collections), cue = tar_cue("always")),

  # one row per inhabited cell and year with its municipality; collector pixel inhabitants given back to their
  # municipality (no-op if expo_correct_noloc is FALSE)
  expo_pop_cells = {
    expo_pop_assets
    build_exposition_cells(expo_years, setup_map_municipalities, correct_noloc = expo_correct_noloc)
  },

  # O3 peak season from NO2 and PM2.5 before 2015 from PM10, fitted on the current monitoring data
  # (decision 6); the coefficients are logged to make shifts of earlier years traceable
  expo_pop_coefs_o3 = fit_o3_peakseason_model(mon_aq_raw, nmin_sites = expo_o3_nmin_sites),
  expo_pop_ratios_pm = fit_pm_ratio(mon_aq_raw),
  tar_target(
    expo_pop_log_coefficients,
    append_output(tidy_derivation_coefficients(expo_pop_coefs_o3, expo_pop_ratios_pm),
                  "exposition_derivation_coefficients.csv", dir = path_log),
    format = "file"
  ),
  expo_pop_cells_derived =
    expo_pop_cells |>
    derive_o3_peakseason(expo_pop_coefs_o3) |>
    derive_pm25_from_pm10(expo_pop_ratios_pm, years = expo_years_pm25_from_pm10),

  # one row per cell, year and parameter, with the concentrations of base_scenario_year
  expo_pop_long = expo_pop_cells_derived |> cells_to_long() |> add_base_scenario(base_scenario_year),

  # population-weighted means (canton with base scenario, municipalities) and the exposition distribution
  # of the canton; inhabitants rounded to whole persons as written (collector inhabitants are fractional)
  expo_pop_means_canton = round_population(combine_canton_means(expo_pop_long, base_scenario_year)),
  expo_pop_means_municipalities = round_population(aggregate_population_weighted_mean(expo_pop_long, level = "municipality")),
  expo_pop_distribution = round_population(aggregate_population_exposition_distrib(expo_pop_long)),

  tar_target(expo_pop_out_means_canton,
             write_output(expo_pop_means_canton, "data_exposition_weighted_means_canton.csv", dir = path_output),
             format = "file"),
  tar_target(expo_pop_out_means_municipalities,
             write_output(expo_pop_means_municipalities, "data_exposition_weighted_means_municipalities.csv", dir = path_output),
             format = "file"),
  tar_target(expo_pop_out_distribution,
             write_output(expo_pop_distribution, "data_exposition_distribution_pollutants.csv", dir = path_output),
             format = "file")
)
