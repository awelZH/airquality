# Plots of the air quality monitoring data -> plots_monitoring (needs scripts/_plot_setup.R)

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
  plot_pars_monitoring_timeseries(data_monitoring_aq, plot_parameters_timeseries, threshold_values = immission_threshold_values,
                                  colour_scale = scale_color_siteclass, pointsize = pointsize, theme = theme_ts)


# the last plot_n_years relative to the LRV limits and critical loads of nitrogen, and to the WHO guidelines
years_recent <- seq(max(plot_years) - plot_n_years + 1, max(plot_years), 1)
plots$monitoring$threshold_comparison$various <-
  threshold_comparison_data(data_monitoring_aq, data_monitoring_ndep, immission_threshold_values, years = years_recent) |>
  plot_threshold_comparison(threshold_styles = dplyr::distinct(immission_threshold_values, source, col, lty, lsz), years = years_recent,
                            colour_scale = scale_color_siteclass, pointsize = pointsize, jitter_seed = jitter_seed, theme = theme_ts)


# nitrogen deposition: long time series at Bachtel (since 2001), all sites since 2019 (absolute and relative
# to the critical load)
threshold_lrv_no2 <- dplyr::filter(immission_threshold_values, source == "LRV Grenzwert" & pollutant == "NO2")

plots$monitoring$timeseries_ndep_bachtel$Ndep <-
  data_monitoring_ndep_pars |>
  dplyr::filter(site == "BA", ecosys == "Wald") |>
  plot_timeseries_ndep_bars(xlim = c(2000,NA), linewidth = threshold_lrv_no2$lsz, color = threshold_lrv_no2$col,
                            title = "Luftqualitätsmesswerte Stickstoffeintrag in empfindliche Ökosysteme am Bachtel", theme = theme_ts) +
  ggplot2::labs(caption = "Daten: Ostluft & FUB") +
  ggplot2::facet_wrap(ecosys~., ncol = 1, scales = "free_y", axes = "all_x")

plots$monitoring$timeseries_ndep_all$Ndep <-
  plot_ndep_sites(data_monitoring_ndep, colour_scale = scale_color_ecosys, fill_scale = scale_fill_ecosys, shape_scale = scale_shape_estimated,
                  pointsize = pointsize, jitter_seed = jitter_seed, theme = theme_ts)

plots$monitoring$timeseries_ndep_all_vs_CLN$Ndep <-
  plot_ndep_sites_vs_cln(data_monitoring_ndep, colour_scale = scale_color_ecosys, linewidth = threshold_lrv_no2$lsz, color = threshold_lrv_no2$col,
                         pointsize = pointsize, jitter_seed = jitter_seed, theme = theme_ts)


# collect plots in a tibble for use in *.qmd
# ---
plots_monitoring <-
  dplyr::bind_rows(
    plotlist_to_tibble(plots$monitoring$threshold_comparison, "monitoring", "threshold_comparison"),
    plotlist_to_tibble(plots$monitoring$timeseries_siteclass, "monitoring", "timeseries_siteclass"),
    plotlist_to_tibble(plots$monitoring$timeseries_ndep_bachtel, "monitoring", "timeseries_ndep_bachtel"),
    plotlist_to_tibble(plots$monitoring$timeseries_ndep_all, "monitoring", "timeseries_ndep_all"),
    plotlist_to_tibble(plots$monitoring$timeseries_ndep_all_vs_CLN, "monitoring", "timeseries_ndep_all_vs_CLN")
  )
