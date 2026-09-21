# Plots of the relative trends of emissions and immissions -> plots_trends (needs scripts/_plot_setup.R)

# emission data for the relative emission trends
data_emikat <- airquality.methods::read_local_csv(ressources_plotting$emissions$emikat, delim = ";", locale = readr::locale(encoding = "UTF-8"))

plots <- list()


# plotting relative trends of emissions and immissions
# ---
# read trend-data 
data_trends <- airquality.methods::read_local_csv(ressources_plotting$trends$trends, delim = ";", locale = readr::locale(encoding = "UTF-8"))
data_trends_agg <- 
  airquality.methods::read_local_csv(ressources_plotting$trends$trends_agg, delim = ";", locale = readr::locale(encoding = "UTF-8")) |> 
  dplyr::mutate(type = dplyr::recode(type, Trend = "Median Trend", Messwerte = "Median Messwerte", emission = "Emission"))
emissions_relative <- 
  data_emikat |> 
  airquality.methods::aggregate_groups(y = "emission", groups = c("year", "pollutant"), nmin = 1) |>
  dplyr::select(year, pollutant, sum) |>
  dplyr::rename(emission = sum) |>
  dplyr::mutate(emission = ifelse(is.na(emission), 0, emission)) |>  
  prepare_emission_trends(reference_year_fun = function(x) plot_reference_year_emissions) |> 
  tidyr::gather(class, value, -year, -pollutant, -type, -reference_year) |> 
  dplyr::mutate(site = "Kanton Zürich")

plots$trends$relative$timeseries_emissions <-
  emissions_relative |> 
  ggplot2::ggplot(ggplot2::aes(x = year, y = value - 1, color = pollutant)) +
  ggplot2::geom_hline(yintercept = 0, color = "gray80", linetype = 2) +
  ggplot2::geom_vline(data = . %>% dplyr::distinct(pollutant, reference_year), mapping = ggplot2::aes(xintercept = reference_year), color = "gray80", linetype = 2) +
  ggplot2::geom_line(linewidth = linewidth) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(), expand = c(0.02,0.02)) +
  # ggplot2::scale_color_manual(name = "Schadstoff", values = colorspace::diverging_hcl(palette = "Tofino", n = 8)) +
  # ggplot2::scale_color_manual(name = "Schadstoff", values = colorspace::sequential_hcl(palette = "YlGnBu", n = 8)) +
  ggplot2::scale_color_manual(name = "Schadstoff", values = c(colorspace::sequential_hcl(palette = "Mako", n = 4), colorspace::sequential_hcl(palette = "ag_sunset", n = 4))) +
  theme_ts +
  ggplot2::theme(legend.title = ggplot2::element_blank()) +
  ggplot2::ggtitle(
    label = "Relative Entwicklung Emissionen im Kanton Zürich",
    subtitle = paste0("Veränderung gegenüber dem Jahr ", plot_reference_year_emissions)) +
  ggplot2::labs(caption = "Daten: Ostluft, Grundlage: EMIS Schweiz")

# plotting relative trends of emissions and immissions vs. reference year
plots$trends$relative$timeseries <-
  data_trends_agg |> 
  dplyr::filter(type %in% c("Emission", "Median Trend", "Median Messwerte")) |>
  dplyr::mutate(pollutant = dplyr::case_when(pollutant == "Ozon" ~ paste0(pollutant,", ",airquality.methods::longmetric(parameter)), TRUE ~ pollutant)) |> 
  dplyr::mutate(type = factor(type, levels = c("Emission", "Median Trend", "Median Messwerte"))) |> 
  plot_timeseries_trend_relative(
    theme = theme_ts, facet_ncol = 2, 
    titlelab =  ggplot2::ggtitle(
      label = "Relative Entwicklung Emissionen & Immissionen im Kanton Zürich",
      subtitle = "Veränderung gegenüber Bezugsjahr (gestrichelte Linie)"),
    captionlab = ggplot2::labs(caption = "Datengrundlage: Ostluft, BAFU, NABEL (BAFU & Empa)")
  )

plots$trends$relative$timeseries_detailed <-
  data_trends_agg |>
  dplyr::filter(type == "Emission") |>
  dplyr::bind_rows(dplyr::filter(data_trends, type == "Trend" & class == "relative Immission" & year <= lubridate::year(Sys.Date()))) |>
  dplyr::mutate(
    pollutant = dplyr::case_when(pollutant == "Ozon" ~ paste0(pollutant,", ",airquality.methods::longmetric(parameter)), TRUE ~ pollutant),
    type = dplyr::recode(type, Trend = "Trend pro Standort"),
    type = factor(type, levels = c("Emission", "Trend pro Standort", "Median Messwerte"))
  ) |> 
  plot_timeseries_trend_relative(
    detailed = TRUE, theme = theme_ts, facet_ncol = 2, 
    titlelab =  ggplot2::ggtitle(
      label = "Relative Entwicklung Emissionen & Immissionen im Kanton Zürich",
      subtitle = "Veränderung gegenüber Bezugsjahr (gestrichelte Linie)"),
    captionlab = ggplot2::labs(caption = "Datengrundlage: Ostluft, BAFU, NABEL (BAFU & Empa)")
  )


# collect plots in a tibble for use in *.qmd
# ---
plots_trends <-
  dplyr::bind_rows(
    plotlist_to_tibble(plots$trends$relative, "trends", "relative")
  )
