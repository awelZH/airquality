# Plots of the population and ecosystem exposition -> plots_exposition (needs scripts/_plot_setup.R)

# map boundaries of the municipalities (current boundaries, without the Kloster Fahr), as in scripts/_setup.R
map_municipalities <-
  airquality.methods::read_geolion_wfs(filter_ressources(ressources, 11), version =  "2.0.0", crs = crs) |>
  airquality.methods::drop_foreign_enclaves()

plots <- list()


# read exposition data
data_expo_distr_pollutants <- airquality.methods::read_local_csv(ressources_plotting$exposition$expo_distr_pollutants, locale = readr::locale(encoding = "UTF-8"))
data_expo_distr_ndep <- airquality.methods::read_local_csv(ressources_plotting$exposition$expo_distr_ndep, locale = readr::locale(encoding = "UTF-8"))
data_expo_weighmean_canton <- airquality.methods::read_local_csv(ressources_plotting$exposition$weightedmean_canton, locale = readr::locale(encoding = "UTF-8"))
data_expo_weighmean_municip <- airquality.methods::read_local_csv(ressources_plotting$exposition$weightedmean_municip, locale = readr::locale(encoding = "UTF-8"))
parameters_exposition <- rlang::set_names(plot_parameters_exposition)


# inhabitants over thresholds: time series, and share in the last plot_n_years
colours_population_over_thresh <- c("über LRV-Grenzwert" = col_lrv, "über WHO-Richtwert" = col_who, "unter Grenz-/Richtwert" = ggplot2::alpha("gray60", 0.3))
data_population_over_thresh <- population_over_thresholds(data_expo_distr_pollutants, data_expo_weighmean_canton, immission_threshold_values)

plots$exposition$population_over_thresh$timeseries_various <-
  plot_population_over_thresholds(data_population_over_thresh, colours = colours_population_over_thresh, theme = theme_ts)

plots$exposition$population_over_thresh$rel_various <-
  plot_population_over_thresholds_share(data_population_over_thresh, n_years = plot_n_years, colours = colours_population_over_thresh, theme = theme_ts)

# --- für ZUP ---
# data_population_over_thresh |>
#   dplyr::mutate(dplyr::across(dplyr::where(is.factor), as.character)) |>
#   write.table("luftschadstoffbelastete_bevoelkerung.csv", sep = ";", quote = F, fileEncoding = "latin1", row.names = F)

# --- für Umweltbericht ---
# plots$exposition$population_over_thresh$timeseries_various %+% dplyr::filter(data_population_over_thresh, pollutant == "Stickstoffdioxid")
# plots$exposition$population_over_thresh$timeseries_various %+% dplyr::filter(data_population_over_thresh, pollutant == "Feinstaub PM2.5")


# exposition distributions of the population per pollutant and of the sensitive ecosystems (nitrogen):
# histograms per year; cumulative distributions of all years, then per year
plots$exposition$distribution_histogram <-
  purrr::map(parameters_exposition, \(parameter) plot_all_expo_hist(parameter, data_expo_distr_pollutants, immission_threshold_values, theme = theme_ts))
plots$exposition$distribution_histogram$Ndep <- plot_all_expo_hist_ndep(data_expo_distr_ndep, threshold_ndep, theme = theme_ts)

plots$exposition$distribution_cumulative <-
  purrr::map(parameters_exposition, \(parameter) plot_all_expo_cumul(parameter, data_expo_distr_pollutants, immission_threshold_values, theme = theme_ts))
plots$exposition$distribution_cumulative$Ndep <- plot_all_expo_cumul_ndep(data_expo_distr_ndep, threshold_ndep, theme = theme_ts)


# maps of the population-weighted means per municipality (canton mean in the subtitle)
data_expo_weighmean_municip <-
  data_expo_weighmean_municip |>
  dplyr::select(-gemeindename) |>
  dplyr::full_join(dplyr::rename(map_municipalities, bfsnr = bfs), by = dplyr::join_by(bfsnr)) |>
  sf::st_as_sf()

plots$exposition$population_weighted_mean_map <-
  purrr::map(parameters_exposition, \(parameter) {
    plot_all_popweighmean_maps(parameter, data_expo_weighmean_municip, data_expo_weighmean_canton, crs = crs, theme = theme_map)
  })


# time series of the population-weighted means of the canton
plots$exposition$population_weighted_mean <- plot_pars_popmean_timeseries(data_expo_weighmean_canton, plot_parameters_timeseries, theme = theme_ts)


# collect the plots in a catalog for the Quarto pages (get_plot(plots_exposition, "distribution_histogram", "NO2", 2020))
# ---
plots_exposition <-
  dplyr::bind_rows(
    plot_catalog(plots$exposition$population_over_thresh$timeseries_various, "population_over_thresh"),
    plot_catalog(plots$exposition$population_over_thresh$rel_various, "population_over_thresh_share"),
    plot_catalog(plots$exposition$distribution_histogram, "distribution_histogram", names_to = c("parameter", "year")),
    plot_catalog(plots$exposition$distribution_cumulative, "distribution_cumulative", names_to = c("parameter", "year")),
    plot_catalog(plots$exposition$population_weighted_mean, "population_weighted_mean", names_to = "parameter"),
    plot_catalog(plots$exposition$population_weighted_mean_map, "population_weighted_mean_map", names_to = c("parameter", "year"))
  )
