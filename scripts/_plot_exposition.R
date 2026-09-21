# Plots of the population and ecosystem exposition -> plots_exposition (needs scripts/_plot_setup.R)

# map boundaries of the municipalities (current boundaries, without the Kloster Fahr), as in scripts/_setup.R
map_municipalities <-
  airquality.methods::read_geolion_wfs(filter_ressources(ressources, 11), version =  "2.0.0", crs = crs) |>
  airquality.methods::drop_foreign_enclaves()

plots <- list()


# plotting air pollutant population and ecosystem exposition
# ---
# read exposition data and setup
data_expo_distr_pollutants <- airquality.methods::read_local_csv(ressources_plotting$exposition$expo_distr_pollutants, locale = readr::locale(encoding = "UTF-8")) 
data_expo_distr_ndep <- airquality.methods::read_local_csv(ressources_plotting$exposition$expo_distr_ndep, locale = readr::locale(encoding = "UTF-8")) 
data_expo_weighmean_canton <- airquality.methods::read_local_csv(ressources_plotting$exposition$weightedmean_canton, locale = readr::locale(encoding = "UTF-8")) 
data_expo_weighmean_municip <- airquality.methods::read_local_csv(ressources_plotting$exposition$weightedmean_municip, locale = readr::locale(encoding = "UTF-8")) 
parameters_exposition <- setNames(plot_parameters_exposition, plot_parameters_exposition)


# plotting time series of population over threshold values for all air pollutants
d <- 
  immission_threshold_values |> 
  dplyr::select(source, parameter, threshold) |> 
  tidyr::spread(source, threshold) |> 
  dplyr::right_join(data_expo_distr_pollutants, by = "parameter") |> 
  dplyr::mutate(
    "über LRV-Grenzwert" = ifelse(concentration <= `LRV Grenzwert`, NA, population),
    "über WHO-Richtwert" = ifelse(concentration <= `WHO Richtwert`, NA, population),
    `über LRV-Grenzwert` = ifelse(parameter == "O3_peakseason_mean_d1_max_mean_h8gl", NA, `über LRV-Grenzwert`),
    `über WHO-Richtwert` = ifelse(parameter == "O3_max_98p_m1", NA, `über WHO-Richtwert`)
  ) |> 
  dplyr::group_by(pollutant, year) |> 
  dplyr::summarise(
    `über LRV-Grenzwert` = sum(`über LRV-Grenzwert`, na.rm = TRUE),
    `über WHO-Richtwert` = sum(`über WHO-Richtwert`, na.rm = TRUE)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    `über WHO-Richtwert` = `über WHO-Richtwert` - `über LRV-Grenzwert`
  ) |>
  tidyr::gather(reference, population, -year,  -pollutant) |> 
  dplyr::filter(!is.na(population)) |> 
  dplyr::mutate(pollutant = airquality.methods::longpollutant(pollutant)) 


# plotting time series of population over threshold values for all air pollutants
pop <-
  data_expo_weighmean_canton |>
  dplyr::filter(parameter != "O3_peakseason_mean_d1_max_mean_h8gl") |> 
  dplyr::distinct(year, population, pollutant) |>
  dplyr::group_by(year, pollutant) |> 
  dplyr::summarise(population_total = sum(population)) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(pollutant = airquality.methods::longpollutant(pollutant)) 

pop <-
  d |> 
  dplyr::group_by(year, pollutant) |> 
  dplyr::summarise(population = sum(population)) |> 
  dplyr::ungroup() |> 
  dplyr::left_join(pop, by = c("year", "pollutant")) |> 
  dplyr::mutate(
    population = population_total - population,
    reference = "unter Grenz-/Richtwert"
  ) |> 
  dplyr::select(-population_total)

plots$exposition$population_over_thresh$timeseries_various <-
  d |> 
  dplyr::bind_rows(pop) |> 
  dplyr::mutate(
    reference = factor(reference, levels = c("unter Grenz-/Richtwert", "über WHO-Richtwert", "über LRV-Grenzwert")),
    population = pmax(0, population) # small negative numbers possible due to different O3 metrics and fundamental data
  ) |> 
  ggplot2::ggplot(ggplot2::aes(x = year, y = population)) + 
  ggplot2::geom_bar(mapping = ggplot2::aes(fill = reference), stat = "identity", position = "stack", width = 0.8) + 
  ggplot2::scale_x_continuous(breaks = seq(1990,2100,5), expand = c(0.01,0.01)) +
  ggplot2::scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = "'"), expand = c(0.01, 0.01)) +
  ggplot2::scale_fill_manual(values = c("über LRV-Grenzwert" = col_lrv, "über WHO-Richtwert" = col_who, "unter Grenz-/Richtwert" = alpha("gray60", 0.3))) +
  ggplot2::facet_wrap(pollutant~., axes = "all_x") + 
  theme_ts + 
  ggplot2::theme(
    strip.text.x = ggplot2::element_text(hjust = 0),
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom"
  ) + 
  ggplot2::ggtitle(
    label = "Entwicklung luftschadstoffbelasteter Wohnbevölkerung",
    subtitle = "Anzahl Personen, Wohnbevölkerung im Kanton Zürich") + 
  ggplot2::labs(caption = "Datengrundlage: BAFU & BFS")


# donought plot of relative population over threshold values for all air pollutants for last x years
plots$exposition$population_over_thresh$rel_various <-
  plots$exposition$population_over_thresh$timeseries_various$data |> 
  dplyr::filter(year %in% tail(unique(year), !!plot_n_years)) |> 
  dplyr::group_by(pollutant, reference) |> 
  dplyr::summarise(population = sum(population)) |> 
  dplyr::group_by(pollutant) |> 
  dplyr::mutate(population_relative = population / sum(population)) |> 
  dplyr::ungroup() |> 
  ggplot2::ggplot(ggplot2::aes(x = 1, y = population_relative, fill = reference)) +
  ggplot2::geom_bar(stat = "identity", width = 0.5) +
  ggplot2::scale_x_continuous(limits = c(0.25,1.25), expand = c(0,0)) +
  ggplot2::scale_y_continuous(limits = c(0,1), labels = scales::percent_format(), expand = c(0,0)) +
  ggplot2::scale_fill_manual(values = c("über LRV-Grenzwert" = col_lrv, "über WHO-Richtwert" = col_who, "unter Grenz-/Richtwert" = alpha("gray60", 0.3))) +
  ggplot2::coord_polar(theta = "y") +
  ggplot2::facet_wrap(pollutant~., nrow = 1) + 
  theme_ts +
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(size = ggplot2::rel(1), hjust = 0.5),
    plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.8), hjust = 0.5),
    plot.caption = ggplot2::element_text(hjust = 0.5, color = "gray40", size = ggplot2::rel(0.66))
  ) +
  ggplot2::ggtitle(
    label = "Luftschadstoffbelastete Wohnbevölkerung",
    subtitle = paste0("Anteil Personen im Kanton Zürich in den Jahren ", max(plots$exposition$population_over_thresh$timeseries_various$data$year) - plot_n_years + 1, " bis ", max(plots$exposition$population_over_thresh$timeseries_various$data$year))
  ) +
  ggplot2::labs(caption = "Datengrundlage: BAFU & BFS")



# --- für ZUP ---

# plots$exposition$population_over_thresh$timeseries_various$data |> 
#   dplyr::mutate_if(is.factor, as.character()) |> 
#   write.table("luftschadstoffbelastete_bevoelkerung.csv", sep = ";", quote = F, fileEncoding = "latin1", row.names = F)

# --- für Umweltbericht ---

# plots$exposition$population_over_thresh$timeseries_various %+% dplyr::filter(plots$exposition$population_over_thresh$timeseries_various$data, pollutant == "Stickstoffdioxid")
# plots$exposition$population_over_thresh$timeseries_various %+% dplyr::filter(plots$exposition$population_over_thresh$timeseries_various$data, pollutant == "Feinstaub PM2.5")

# d |> 
# dplyr::filter(year %in% seq(max(plot_years) - plot_n_years + 1, max(plot_years), 1)) |> 
# dplyr::group_by(pollutant, reference) |> 
# dplyr::summarise(population = sum(population)) |> 
# dplyr::ungroup() |> 
# dplyr::mutate(population_relative = population / !!pop) |> 
# ggplot2::ggplot(ggplot2::aes(x = 1, y = population_relative, fill = reference)) +
# ggplot2::geom_bar(stat = "identity", width = 0.75) +
# ggplot2::scale_y_continuous(limits = c(0,1), labels = scales::percent_format(), expand = c(0,0)) +
# ggplot2::scale_fill_manual(values = c("über LRV-Grenzwert" = col_lrv, "zusätzlich über WHO-Richtwert" = col_who)) +
# ggplot2::coord_polar(theta = "y") +
# facet_wrap(pollutant~.) +
# theme_minimal() +
# ggplot2::theme(
#   legend.title = ggplot2::element_blank(),
#   axis.text.y = element_blank(),
#   axis.title = element_blank(), 
#   panel.spacing.x = unit(2, "lines")
# ) +
# ggplot2::ggtitle(
#   label = "Luftschadstoffbelastete Wohnbevölkerung im Kanton Zürich ",
#   subtitle = paste0("Durchschnittlicher Anteil an Gesamtbevölkerung in den Jahren ", max(plot_years) - plot_n_years + 1, " bis ", max(plot_years))
# ) + 
# ggplot2::labs(caption = "Datengrundlage: BAFU & BFS")


# --- für Umweltbericht ---



# plotting histograms for air pollutants
plots$exposition$distribution_histogram <-
  lapply(parameters_exposition, function(parameter) {
    plot_all_expo_hist(parameter, data_expo_distr_pollutants)
  })

# plotting histograms for sensitive ecosystems nitrogen deposition exceedance
plots$exposition$distribution_histogram$Ndep <- plot_all_expo_hist_ndep(data_expo_distr_ndep, threshold_ndep)

# plotting cumulative distributions for air pollutants
plots$exposition$distribution_cumulative <-
  lapply(parameters_exposition, function(parameter) {
    
    plots_years <- plot_all_expo_cumul(parameter, data_expo_distr_pollutants)
    
    data_plot <- dplyr::filter(data_expo_distr_pollutants, parameter == !!parameter)
    pollutant <- unique(data_plot$pollutant)
    metric <- unique(data_plot$metric)
    thresh <- extract_threshold(immission_threshold_values, pollutant, metric)
    plot_all <-
      ggplot2::ggplot(data_plot, mapping = ggplot2::aes(x = concentration, y = population_cum_rel, color = factor(year), group = year)) +
      ggplot2::geom_vline(xintercept = thresh$value, color = thresh$color, linetype = thresh$linetype, linewidth = thresh$linesize) +
      ggplot2::geom_line(linewidth = 1) +
      # ggiraph::geom_line_interactive(mapping = ggplot2::aes(data_id = year, tooltip = population_cum), linewidth = 1) +
      ggplot2::scale_x_continuous(limits = range(expositionpars(parameter)$xbreaks), breaks = expositionpars(parameter)$xbreaks, expand = c(0.01,0.01)) +
      ggplot2::scale_y_continuous(limits = c(0,1), expand = c(0.01,0.01), labels = scales::percent_format()) +
      # ggplot2::scale_color_brewer(name = "Jahr") +
      colorspace::scale_color_discrete_diverging(name = "Jahr", palette = "Blue-Yellow") +
      ggplot2::xlab(openair::quickText(paste0(pollutant," ",metric," (µg/m3)"))) +
      ggplot2::ggtitle(
        label = openair::quickText(paste0("Bevölkerungsexposition ",longpollutant(parameter))),
        subtitle = "relativer Anteil (kumuliert), Wohnbevölkerung im Kanton Zürich"
      ) +
      ggplot2::labs(caption = "Datengrundlage: BAFU & BFS") +
      theme_ts +
      ggplot2::theme(axis.title.x = ggplot2::element_text()) +
      ggplot2::geom_text(data = tibble::tibble(x = thresh$value, label = thresh$labels), mapping = ggplot2::aes(x = x, y = 0, label = label), size = thresh$labelsize,
                         hjust = 0, vjust = 0, angle = 90, nudge_x = pmin(0, -0.01 * max(expositionpars(parameter)$xbreaks), na.rm = TRUE), inherit.aes = FALSE)
    
    # plot_all <- ggiraph::girafe(ggobj = plot_all, width_svg = 6, height_svg = 3, options = list(ggiraph::opts_hover_inv(css = "opacity:0.1;")))
    
    c(list(alle = plot_all), plots_years)
    
  })


# plotting cumulative distributions for sensitive ecosystems nitrogen deposition exceedance
plots$exposition$distribution_cumulative$Ndep <- plot_all_expo_cumul_ndep(data_expo_distr_ndep, threshold_ndep)
plots$exposition$distribution_cumulative$Ndep$alle <-
  ggplot2::ggplot(data_expo_distr_ndep, mapping = ggplot2::aes(x = ndep_exmax, y = n_ecosys_cum_rel, color = factor(year), group = year)) +
  ggplot2::geom_vline(xintercept = threshold_ndep$value, color = threshold_ndep$color, linetype = threshold_ndep$linetype, linewidth = threshold_ndep$linesize) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::scale_x_continuous(limits = range(expositionpars("Ndep")$xbreaks), breaks = expositionpars("Ndep")$xbreaks, expand = c(0.01,0.01)) +
  ggplot2::scale_y_continuous(limits = c(0,1), expand = c(0.01,0.01), labels = scales::percent_format()) +
  colorspace::scale_color_discrete_diverging(name = "Jahr", palette = "Blue-Yellow") +
  ggplot2::xlab(expression("max. Stickstoff-Überschuss im Vergleich zu den kritischen Eintragsraten (kgN " * ha^-1 * Jahr^-1 * ")")) +
  ggplot2::ggtitle(
    label = openair::quickText("Exposition empfindlicher Ökosysteme durch Stickstoffeinträge"),
    subtitle = "relativer Anteil empfindlicher Ökosysteme (kumuliert) im Kanton Zürich"
  ) +
  ggplot2::labs(caption = "Daten: BAFU") +
  theme_ts +
  ggplot2::theme(axis.title.x = ggplot2::element_text()) +
  ggplot2::geom_text(data = tibble::tibble(x = threshold_ndep$value, label = threshold_ndep$labels), mapping = ggplot2::aes(x = x, y = 0, label = label), size = threshold_ndep$labelsize,
                     hjust = 0, vjust = 0, angle = 90, nudge_x = pmin(0, -0.01 * max(expositionpars("Ndep")$xbreaks), na.rm = TRUE), inherit.aes = FALSE)


# plotting maps of population-weighted mean pollutant concentration (single value for Kanton Zürich & per municipality)
data_expo_weighmean_municip <- 
  data_expo_weighmean_municip |> 
  dplyr::select(-gemeindename) |> 
  dplyr::full_join(dplyr::rename(map_municipalities, bfsnr = bfs), by = "bfsnr") |> 
  sf::st_as_sf()

plots$exposition$population_weighted_mean_map <-
  lapply(parameters_exposition, function(parameter) {
    plot_all_popweighmean_maps(parameter, data_expo_weighmean_municip, data_expo_weighmean_canton)
  })


# plotting timeseries of population-weighted mean pollutant concentration for Canton Zürich
plots$exposition$population_weighted_mean <- plot_pars_popmean_timeseries(data_expo_weighmean_canton, plot_parameters_timeseries)



# collect plots in a tibble for use in *.qmd
# ---
plots_exposition <-
  dplyr::bind_rows(
    plotlist_to_tibble(plots$exposition$population_over_thresh, "exposition", "population_over_thresh"),
    plotlist_to_tibble(plots$exposition$distribution_histogram, "exposition", "distribution_histogram"),
    plotlist_to_tibble(plots$exposition$distribution_cumulative, "exposition", "distribution_cumulative"),
    plotlist_to_tibble(plots$exposition$population_weighted_mean, "exposition", "population_weighted_mean"),
    plotlist_to_tibble(plots$exposition$population_weighted_mean_map, "exposition", "population_weighted_mean_map")
  )
