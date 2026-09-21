# Plots of the relative trends of emissions and immissions -> plots_trends (needs scripts/_plot_setup.R)

plots <- list()


# read emission and trend data
data_emikat <- airquality.methods::read_local_csv(ressources_plotting$emissions$emikat, delim = ";", locale = readr::locale(encoding = "UTF-8"))
data_trends <- airquality.methods::read_local_csv(ressources_plotting$trends$trends, delim = ";", locale = readr::locale(encoding = "UTF-8"))
data_trends_agg <- airquality.methods::read_local_csv(ressources_plotting$trends$trends_agg, delim = ";", locale = readr::locale(encoding = "UTF-8"))


# relative emissions of all pollutants vs. the reference year
plots$trends$relative$timeseries_emissions <-
  emission_trends_relative(data_emikat, reference_year = plot_reference_year_emissions) |>
  plot_emission_trends_relative(reference_year = plot_reference_year_emissions, linewidth = linewidth, theme = theme_ts)


# relative trends of emissions and immissions vs. the reference year: medians, and trends per site
titlelab_trends <- ggplot2::ggtitle(
  label = "Relative Entwicklung Emissionen & Immissionen im Kanton Zürich",
  subtitle = "Veränderung gegenüber Bezugsjahr (gestrichelte Linie)"
)
captionlab_trends <- ggplot2::labs(caption = "Datengrundlage: Ostluft, BAFU, NABEL (BAFU & Empa)")

plots$trends$relative$timeseries <-
  trend_data_overview(data_trends_agg) |>
  plot_timeseries_trend_relative(theme = theme_ts, facet_ncol = 2, titlelab = titlelab_trends, captionlab = captionlab_trends)

plots$trends$relative$timeseries_detailed <-
  trend_data_detailed(data_trends_agg, data_trends, year_max = lubridate::year(Sys.Date())) |>
  plot_timeseries_trend_relative(detailed = TRUE, theme = theme_ts, facet_ncol = 2, titlelab = titlelab_trends, captionlab = captionlab_trends)


# collect the plots in a catalog for the Quarto pages (get_plot(plots_trends, "timeseries"))
# ---
plots_trends <-
  dplyr::bind_rows(
    plot_catalog(plots$trends$relative$timeseries_emissions, "timeseries_emissions"),
    plot_catalog(plots$trends$relative$timeseries, "timeseries"),
    plot_catalog(plots$trends$relative$timeseries_detailed, "timeseries_detailed"),
    # one plot per pollutant (German name as in the panels), legend on the right
    plot_catalog(plot_trends_per_pollutant(plots$trends$relative$timeseries), "timeseries_pollutant", names_to = "parameter"),
    plot_catalog(plot_trends_per_pollutant(plots$trends$relative$timeseries_detailed), "timeseries_detailed_pollutant", names_to = "parameter")
  )
