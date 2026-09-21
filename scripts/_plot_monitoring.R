# Plots of the air quality monitoring data -> plots_monitoring (needs scripts/_plot_setup.R)

plots <- list()


# plotting air pollutant monitoring data
# ---
# read airquality monitoring data
data_monitoring_aq <- 
  airquality.methods::read_local_csv(ressources_plotting$monitoring$airquality, locale = readr::locale(encoding = "UTF-8")) |> 
  dplyr::mutate(siteclass = factor(siteclass, levels = siteclass_levels)) |> 
  dplyr::filter(year %in% plot_years & parameter %in% plot_parameters_timeseries & !is.na(siteclass) & !(siteclass %in% c("ländlich - verkehrsbelastet", "klein-/vorstädtisch - verkehrsbelastet"))) 


# plot timeseries of yearly values for selected pollutants
plots$monitoring$timeseries_siteclass <- plot_pars_monitoring_timeseries(data_monitoring_aq, plot_parameters_timeseries)


# read pre-compiled Ostluft y1 monitoring data for nitrogen deposition to sensitive ecosystems into separate dataset
data_monitoring_ndep <- 
  airquality.methods::read_local_csv(ressources_plotting$monitoring$ndep, locale = readr::locale(encoding = "UTF-8")) |> 
  dplyr::filter(dplyr::when_all(!is.na(ecosys), ecosys != "Siedlungen", !is.na(cln)))

data_monitoring_ndep_pars <- 
  airquality.methods::read_local_csv(ressources_plotting$monitoring$ndep_pars, locale = readr::locale(encoding = "UTF-8")) |> 
  dplyr::filter(dplyr::when_all(!is.na(ecosys), ecosys != "Siedlungen", !is.na(cln))) |> 
  dplyr::summarise(deposition = sum(deposition), .by = c("year", "site", "ecosys", "cln", "pollutant", "unit", "source_cat")) |> 
  dplyr::rename(component = source_cat) |> 
  dplyr::mutate(
    component = factor(component, levels = rev(c("aus NH3-Quellen", "aus NOx-Quellen"))),
    ecosys = factor(ecosys, levels = rev(c("Hochmoor", "Flachmoor", "Trockenrasen", "Wald")))
  )


# plot relative comparison latest plot_n_years of measurement data vs. LRV Immissionsgrenzwerte + Critical Loads of Nitrogen and WHO-Richtwerte
data_thrshlds <- dplyr::distinct(immission_threshold_values, source, col, lty, lsz)
data_temp <-
  data_monitoring_ndep |>
  dplyr::filter(dplyr::when_all(year %in% seq(max(plot_years) - plot_n_years + 1, max(plot_years), 1))) |>
  dplyr::mutate(
    value = deposition / cln,
    reference = factor("value_relative_lrv"),
    siteclass = "empf. Ökosystem"
  ) |>
  dplyr::select(year, pollutant, metric, value, reference, siteclass)

plots$monitoring$threshold_comparison$various <-
  data_monitoring_aq |>
  combine_thresholds(immission_threshold_values) |>
  dplyr::mutate(
    value_relative_lrv = concentration / `LRV Grenzwert`,
    value_relative_who = concentration / `WHO Richtwert`
  ) |>
  dplyr::filter(year %in% seq(max(plot_years) - plot_n_years + 1, max(plot_years), 1)) |>
  dplyr::select(year, pollutant, metric, value_relative_lrv, value_relative_who, siteclass) |>
  tidyr::gather(reference, value, -year, -pollutant, -metric, -siteclass) |>
  dplyr::filter(!is.na(value) & !(siteclass %in% c("ländlich - verkehrsbelastet", "klein-/vorstädtisch - verkehrsbelastet"))) |>
  dplyr::bind_rows(data_temp) |>
  dplyr::mutate(
    pollutant = dplyr::recode_factor(pollutant, "Ndep" = "Stickstoffeintrag in empf. Ökosysteme"),
    metric = dplyr::recode_factor(metric, "Jahressumme" = ""),
    x = paste0(pollutant, " ", metric),
    x = factor(x, levels = rev(sort(unique(.data$x)))),
    reference = dplyr::recode(reference, !!!c("value_relative_lrv" = "relativ zu Immissionsgrenzwerten bzw. kritischen Eintragsraten:", 
                                              "value_relative_who" = "relativ zu Richtwerten der Weltgesundheitsorganisation:"))
  ) |> 
  ggplot2::ggplot(aes(x = x, y = value, color = siteclass)) +
  ggplot2::geom_hline(yintercept = 1, linetype = data_thrshlds$lty, color = data_thrshlds$col, linewidth = data_thrshlds$lsz, show.legend = FALSE) +
  ggplot2::geom_jitter(shape = 21, size = pointsize, width = 0.2) +
  ggplot2::facet_wrap(reference~., scales = "free_y", ncol = 1, axes = "all_x") +
  ggplot2::scale_y_continuous(breaks = seq(0,10,1), limits = c(0,NA), labels = scales::percent_format(), expand = c(0.01,0.01)) +
  ggplot2::coord_flip() +
  ggplot2::guides(color = ggplot2::guide_legend(nrow = 3)) +
  ggplot2::ggtitle(
    label = openair::quickText("Luftqualitätsmesswerte Referenzwertvergleich"),
    subtitle = paste0("Jahre ", max(plot_years) - plot_n_years + 1, " bis ", max(plot_years))
  ) +
  ggplot2::labs(caption = "Daten: Ostluft & NABEL (BAFU & Empa)") +
  theme_ts +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.major.x = element_line(),
    strip.text = ggplot2::element_text(hjust = 0)
  ) +
  scale_color_siteclass


# plot long-standing timeseries of yearly nitrogen deposition at Bachtel site (since 2001)
temp <- dplyr::filter(immission_threshold_values, source == "LRV Grenzwert" & pollutant == "NO2")
plots$monitoring$timeseries_ndep_bachtel$Ndep <-
  data_monitoring_ndep_pars |>
  dplyr::filter(dplyr::when_all(site == "BA", ecosys == "Wald")) |>
  plot_timeseries_ndep_bars(xlim = c(2000,NA), linewidth = temp$lsz, color = temp$col, title = "Luftqualitätsmesswerte Stickstoffeintrag in empfindliche Ökosysteme am Bachtel") +
  ggplot2::labs(caption = "Daten: Ostluft & FUB") +
  ggplot2::facet_wrap(ecosys~., ncol = 1, scales = "free_y", axes = "all_x")


# plot timeseries of yearly nitrogen deposition across several monitoring sites (structured per ecosystem type) since 2019
plots$monitoring$timeseries_ndep_all$Ndep <-
  data_monitoring_ndep |>
  dplyr::filter(year >= 2019) |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = deposition, color = ecosys, fill = ecosys, shape = estimated_class)) +
  ggplot2::geom_jitter(size = pointsize * 1.5, width = 0.1) +
  ggplot2::scale_x_continuous(limits = c(2019,NA), expand = c(0.01,0.01)) +
  ggplot2::scale_y_continuous(limits = c(0,NA), expand = ggplot2::expansion(mult = c(0, 0.02))) +
  scale_color_ecosys +
  scale_fill_ecosys +
  scale_shape_estimated +
  ggplot2::ggtitle(
    label = openair::quickText("Luftqualitätsmesswerte - Stickstoffeintrag in empfindliche Ökosysteme seit 2019"),
    subtitle = expression("Stickstoffeintrag (kg-N " * ha^-1 * Jahr^-1 * ")")
  ) +
  ggplot2::labs(caption = "geschätzt: je nach Messprogramm versch. Anteile statistisch geschätzt (NH3 immer gemessen)\nQuelle: Ostluft") +
  theme_ts 


# plot timeseries of yearly nitrogen deposition vs. critical loads of nitrogen across several monitoring sites (structured per ecosystem type)
plots$monitoring$timeseries_ndep_all_vs_CLN$Ndep <-
  data_monitoring_ndep |>
  dplyr::filter(year >= 2019) |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = deposition / cln, color = ecosys)) +
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), color = temp$col, linewidth = temp$lsz, show.legend = FALSE) +
  ggplot2::geom_jitter(size = pointsize * 1.5, width = 0.1) +
  ggplot2::scale_x_continuous(limits = c(2019,NA), expand = c(0.01,0.01)) +
  ggplot2::scale_y_continuous(limits = c(0,NA), expand = ggplot2::expansion(mult = c(0, 0.02)), labels = scales::percent_format()) +
  scale_color_ecosys +
  ggplot2::ggtitle(
    label = openair::quickText("Luftqualitätsmesswerte - Stickstoffeintrag in empfindliche Ökosysteme seit 2019"),
    subtitle = expression("Stickstoffeintrag vs. kritische Eintragsrate (relativ)")
  ) +
  ggplot2::labs(caption = "Quelle: Ostluft") +
  theme_ts


# plot mean contribution of source categories to nitrogen deposition
plots$monitoring$ndep_mean_sources_fractions$Ndep <-
  data_monitoring_ndep_pars |>
  dplyr::filter(year >= 2019) |> 
  dplyr::group_by(year, component) |> 
  dplyr::summarise(deposition = mean(deposition)) |> 
  dplyr::ungroup() |> 
  ggplot2::ggplot(ggplot2::aes(x = year, y = deposition, fill = component)) +
  geom_bar(stat = "identity", position = "fill") +
  ggplot2::scale_x_continuous(breaks = seq(2018,max(plot_years),1), expand = c(0.01,0.01)) +
  ggplot2::scale_y_continuous(expand = c(0.01,0.01), labels = scales::percent_format()) +
  ggplot2::scale_fill_manual(values = c("aus NOx-Quellen" = "#B696D6", "aus NH3-Quellen" = "#2A5676")) +
  ggplot2::ggtitle(
    label = openair::quickText("Luftqualitätsmesswerte Stickstoffeintrag in empfindliche Ökosysteme seit dem Jahr 2019"),
    subtitle = expression("mittlerer Beitrag der Quellgruppen")
  ) +
  ggplot2::labs(caption = "Daten: Ostluft") +
  theme_ts +
  ggplot2::theme(
    legend.title = ggplot2::element_blank()
  )


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
