# Plots of the air quality monitoring data -> plots_monitoring, a plot catalog (needs report/plots/_plot_setup.R)

plots <- list()


# read and prepare the monitoring data
data_monitoring_aq <-
  airquality.methods::read_local_csv(ressources_plotting$monitoring$airquality, locale = readr::locale(encoding = "UTF-8")) |>
  prepare_plot_airquality(years = plot_years, parameters = plot_parameters_timeseries, siteclass_levels = siteclass_levels)

data_monitoring_ndep <-
  airquality.methods::read_local_csv(ressources_plotting$monitoring$ndep, locale = readr::locale(encoding = "UTF-8")) |>
  prepare_plot_ndep()

data_monitoring_ndep_pars <-
  airquality.methods::read_local_csv(ressources_plotting$monitoring$ndep_pars, locale = readr::locale(encoding = "UTF-8")) |>
  prepare_plot_ndep_components()


# time series of the yearly values per site class
plots$monitoring$timeseries_siteclass <-
  plot_monitoring_timeseries(data_monitoring_aq, plot_parameters_timeseries, axes = plot_axes_timeseries, threshold_values = immission_threshold_values,
                                  colour_scale = scale_color_siteclass, pointsize = pointsize, theme = theme_ts)

# eBC: its own title, the values are yearly means of samples
plots$monitoring$timeseries_siteclass$eBC <-
  plots$monitoring$timeseries_siteclass$eBC +
  ggplot2::ggtitle(
    label = openair::quickText("Luftqualitätsmesswerte Russ im Feinstaub"),
    subtitle = openair::quickText("eBC, Sichproben-Jahresmittelwert (μg/m3)")
  )


# the last plot_n_years relative to the LRV limits and critical loads of nitrogen, and to the WHO guidelines
# one plot per moving window of plot_n_years, the newest window last (shown first by the slider); the
# comparison is prepared once over all years, so every window shows the same categories and the same scale
comparison_all <- threshold_comparison_data(data_monitoring_aq, data_monitoring_ndep, immission_threshold_values, years = plot_years)
comparison_limits <- threshold_comparison_limits(comparison_all)
comparison_categories <- threshold_comparison_categories(comparison_all)

plots$monitoring$threshold_comparison <-
  purrr::map(year_windows(data_monitoring_aq$year, width = plot_n_years), \(years) {
    comparison_all |>
      dplyr::filter(year %in% years) |>
      plot_threshold_comparison(threshold_styles = dplyr::distinct(immission_threshold_values, source, col, lty, lsz), years = years,
                                limits = comparison_limits, categories = comparison_categories,
                                colour_scale = scale_color_siteclass, pointsize = pointsize,
                                jitter_seed = jitter_seed, theme = theme_ts)
  })


# nitrogen deposition: long time series at Bachtel (since 2001), all sites since 2019 (absolute and relative
# to the critical load)
threshold_lrv_no2 <- dplyr::filter(immission_threshold_values, source == "LRV Grenzwert" & pollutant == "NO2")

plots$monitoring$timeseries_ndep_bachtel$Ndep <-
  data_monitoring_ndep_pars |>
  dplyr::filter(site == "BA", ecosys == "Wald") |>
  plot_ndep_bars(xlim = c(2000,NA), linewidth = threshold_lrv_no2$lsz, colour = threshold_lrv_no2$col,
                            title = "Luftqualitätsmesswerte Stickstoffeintrag in empfindliche Ökosysteme am Bachtel", theme = theme_ts) +
  ggplot2::labs(caption = "krit. Eintragsraten nach heutigem Stand, Daten: Ostluft & FUB") +
  ggplot2::facet_wrap(ecosys~., ncol = 1, scales = "free_y", axes = "all_x")

plots$monitoring$timeseries_ndep_all$Ndep <-
  plot_ndep_sites(data_monitoring_ndep, colour_scale = scale_color_ecosys, fill_scale = scale_fill_ecosys, shape_scale = scale_shape_estimated,
                  pointsize = pointsize, jitter_seed = jitter_seed, theme = theme_ts)

plots$monitoring$timeseries_ndep_exceedance$Ndep <-
  plot_ndep_sites_vs_cln(data_monitoring_ndep, relative = FALSE, colour_scale = scale_color_ecosys, linewidth = threshold_lrv_no2$lsz,
                         colour = threshold_lrv_no2$col, pointsize = pointsize, jitter_seed = jitter_seed, theme = theme_ts)

plots$monitoring$timeseries_ndep_all_vs_CLN$Ndep <-
  plot_ndep_sites_vs_cln(data_monitoring_ndep, colour_scale = scale_color_ecosys, linewidth = threshold_lrv_no2$lsz, colour = threshold_lrv_no2$col,
                         pointsize = pointsize, jitter_seed = jitter_seed, theme = theme_ts)


# pollutant maps of the canton per year and their verification against the measured values; the data are not
# part of the contract and come from the targets store (pipelines/monitoring_maps.R), so the pipeline has to
# have run
maps_raster <- targets::tar_read(mon_maps_raster, store = path_store)
maps <- dplyr::bind_rows(maps_raster$maps, targets::tar_read(mon_maps_o3, store = path_store),
                         targets::tar_read(mon_maps_pm25, store = path_store))
maps_boundary <- targets::tar_read(mon_maps_boundary, store = path_store)
maps_validation <- targets::tar_read(mon_maps_validation, store = path_store)
maps_fit <- targets::tar_read(mon_maps_fit, store = path_store)
rm(maps_raster)

# captions of the derived maps (O3 peak season: all years; PM2.5: before 2015)
captions_derived <- c(
  O3_peakseason_mean_d1_max_mean_h8gl = "Datengrundlage: BAFU (NO2), abgeleitet mit Messwerten von Ostluft & NABEL",
  PM2.5 = "Datengrundlage: BAFU (PM10), abgeleitet mit dem PM2.5:PM10-Verhältnis der NABEL-Messwerte"
)
plots$monitoring$map <-
  purrr::map(rlang::set_names(mon_maps_parameters), \(parameter) {
    plot_pollutant_maps(maps, parameter, boundary = maps_boundary, crs = crs, caption_derived = captions_derived[parameter],
                        theme = theme_map)
  })

# exceedance of the critical loads for nitrogen, the raster of the exposition (no verification)
plots$monitoring$map$Ndep <-
  plot_ndep_exceedance_maps(targets::tar_read(expo_eco_ndep, store = path_store), years = plot_years, boundary = maps_boundary,
                            crs = crs, theme = theme_map)

plots$monitoring$map_validation <-
  purrr::map(rlang::set_names(mon_maps_parameters), \(parameter) {
    plot_map_validation(maps_validation, maps_fit, parameter, in_sample = parameter == "O3_peakseason_mean_d1_max_mean_h8gl",
                        pointsize = pointsize, theme = theme_scatter)
  })


# collect the plots in a catalog for the Quarto pages (airquality.methods::get_plot(plots_monitoring, "timeseries_siteclass", "NO2"))
# ---
plots_monitoring <-
  dplyr::bind_rows(
    airquality.methods::plot_catalog(plots$monitoring$threshold_comparison, "threshold_comparison", names_to = "year"),
    airquality.methods::plot_catalog(plots$monitoring$timeseries_siteclass, "timeseries_siteclass", names_to = "parameter"),
    airquality.methods::plot_catalog(plots$monitoring$timeseries_ndep_bachtel$Ndep, "timeseries_ndep_bachtel"),
    airquality.methods::plot_catalog(plots$monitoring$timeseries_ndep_all$Ndep, "timeseries_ndep_all"),
    airquality.methods::plot_catalog(plots$monitoring$timeseries_ndep_exceedance$Ndep, "timeseries_ndep_exceedance"),
    airquality.methods::plot_catalog(plots$monitoring$timeseries_ndep_all_vs_CLN$Ndep, "timeseries_ndep_all_vs_CLN"),
    airquality.methods::plot_catalog(plots$monitoring$map, "map", names_to = c("parameter", "year")),
    airquality.methods::plot_catalog(plots$monitoring$map_validation, "map_validation", names_to = "parameter")
  )
