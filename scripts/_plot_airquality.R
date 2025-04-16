# setup plotting
# ---

# list of output data sources for plotting
ressources_plotting <-
  list(
    emissions = list(
      emikat = "inst/extdata/output/data_emissions.csv",
      rsd_norm = "inst/extdata/output/data_nox_vehicle_emissions_rsd_per_norm.csv",
      rsd_yearmodel = "inst/extdata/output/data_nox_emissions_rsd_per_yearmodel.csv",
      rsd_yearmeas = "inst/extdata/output/data_nox_emissions_rsd_per_yearmeas.csv"
    ),
    monitoring = list(
      airquality = "inst/extdata/output/data_airquality_monitoring_y1.csv",
      ndep = "inst/extdata/output/data_ndep_monitoring_y1.csv"
    ),
    exposition = list(
      weightedmean_canton = "inst/extdata/output/data_exposition_weighted_means_canton.csv",
      weightedmean_municip = "inst/extdata/output/data_exposition_weighted_means_municipalities.csv",
      expo_distr_pollutants = "inst/extdata/output/data_exposition_distribution_pollutants.csv",
      expo_distr_ndep ="inst/extdata/output/data_exposition_distribution_ndep.csv"
    ),
    outcomes = list(
      outcomes = "inst/extdata/output/data_health_outcomes.csv"
    )
  )


# data subsetting parameters
years <- 1995:(lubridate::year(Sys.Date()) - 1) # years to consider for plotting 
n_years <- 3 # consider last 3 years for plotting relative threshold comparison    
parameters_timeseries <- c("NO2", "PM10", "PM2.5", "O3_max_98p_m1", "O3_peakseason_mean_d1_max_mean_h8gl") # parameters to include for timeseries plotting
parameters_exposition <- c("NO2", "O3_max_98p_m1", "PM10", "PM2.5", "O3_peakseason_mean_d1_max_mean_h8gl") # parameters to include for exposition plotting
siteclass_levels <- rev(c("ländlich - Hintergrund", "klein-/vorstädtisch - Hintergrund",
                          "städtisch - Hintergrund", "städtisch - verkehrsbelastet"))


# plotting size parameters
basesize <- 12 # ggplot theme base_size
pointsize <- 2 # size of point markers
linewidth <- 1 # width of lines


# read LRV legal threshold limit values & WHO air quality guideline values
immission_threshold_values <- readr::read_delim(filter_ressources(ressources, 10), delim = ";",locale = readr::locale(encoding = "UTF-8"))


# add plotting parameter to LRV threshold limit values & WHO air quality guideline values
col_lrv <- "red3" # color of LRV threshold value
col_who <- "gray30" # color of WHO guideline threshold value
lty_lrv <- 1 # line type of LRV threshold value
lty_who <- 2 # line type WHO guideline threshold value
lsz_lrv <- 1 # line width of LRV threshold value
lsz_who <- 1 # line width of WHO guideline threshold value
lbsz <- 4 # label size of threshold value line text
immission_threshold_values <-
  tibble::tibble(
    source = c("LRV Grenzwert", "WHO Richtwert"),
    col = c(col_lrv, col_who),
    lty = c(lty_lrv, lty_who),
    lsz = c(lsz_lrv, lsz_who),
    lbsz = lbsz
  ) |> 
  dplyr::right_join(immission_threshold_values, by = "source")

threshold_ndep <- extract_threshold(dplyr::filter(immission_threshold_values, source == "LRV Grenzwert"), "NO2")
threshold_ndep$value <- 0
threshold_ndep$labels <- "kritische Eintragsrate CLN"


# colors and color scales
scale_fill_siteclass <- 
  ggplot2::scale_fill_manual(name = "Standortklasse", values = c(
    "ländlich - Hintergrund" = viridis_pal(option = "D", begin = 0.2, end = 0.97)(4)[4],
    "klein-/vorstädtisch - Hintergrund" = viridis_pal(option = "D", begin = 0.2, end = 0.97)(4)[3],
    "städtisch - Hintergrund" = viridis_pal(option = "D", begin = 0.2, end = 0.97)(4)[2],
    "städtisch - verkehrsbelastet" = viridis_pal(option = "D", begin = 0.2, end = 0.97)(4)[1],
    "empf. Ökosystem" = "gray20"
  ))

scale_color_siteclass <- 
  ggplot2::scale_color_manual(name = "Standortklasse", na.value = "gray60", values = c(
    "ländlich - Hintergrund" = viridis_pal(option = "D", begin = 0.2, end = 0.97)(4)[4],
    "klein-/vorstädtisch - Hintergrund" = viridis_pal(option = "D", begin = 0.2, end = 0.97)(4)[3],
    "städtisch - Hintergrund" = viridis_pal(option = "D", begin = 0.2, end = 0.97)(4)[2],
    "städtisch - verkehrsbelastet" = viridis_pal(option = "D", begin = 0.2, end = 0.97)(4)[1],
    "empf. Ökosystem" = "gray20"
  ))

cols_emissions <- c("Dienstleistungen" = "Gold", "Haushalte" =  "Green", "Industrie" = "Blue", 
                    "Land- und Forstw." = "Purple", "Verkehr" = "Gray", "natürl. Emissionen" = "natural")

# ggplot2 custom themes
theme_ts <-
  theme_minimal(base_size = basesize, base_family = "Arial") +
  theme(
    plot.title = element_text(size = ggplot2::rel(1)),
    plot.subtitle = element_text(size = ggplot2::rel(0.8)),
    plot.caption = element_text(hjust = 1, color = "gray40", face = "italic", size = ggplot2::rel(0.66)),
    plot.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_blank(),
    axis.line.x = element_line(color = "gray30"),
    axis.ticks = element_line(color = "gray30"),
    axis.title = element_blank()
  )

theme_map <-
  theme_void(base_size = basesize, base_family = "Arial") +
  theme(
    plot.subtitle = element_text(size = ggplot2::rel(0.8)),
    plot.caption = element_text(hjust = 1, color = "gray40", face = "italic", size = ggplot2::rel(0.75)),
    panel.background = element_blank(),
    plot.background = element_blank()
  )

# ggiraph::set_girafe_defaults(
#   opts_hover = ggiraph::opts_hover(css = ggiraph::girafe_css_bicolor(primary = NA, secondary = "grey30")),
#   opts_hover_inv(css = "opacity:0.4"), 
#   opts_zoom = ggiraph::opts_zoom(min = 1, max = 4),
#   opts_tooltip = ggiraph::opts_tooltip(css = "padding:3px;color:white;", opacity = 0.8, use_fill = TRUE),
#   opts_sizing = ggiraph::opts_sizing(rescale = TRUE),
#   opts_toolbar = ggiraph::opts_toolbar(saveaspng = TRUE, position = "bottom", delay_mouseout = 5000)
# )


# empty tibble to collect all plots
plots <- list()







# plotting air pollutant emissions
# ---
# read & plot details of Canton Zürich air pollutant emissions per pollutant, subsector and year (absolute and relative values)
data_emikat <- read_local_csv(ressources_plotting$emissions$emikat, delim = ";", locale = readr::locale(encoding = "UTF-8"))
pollutants <- setNames(unique(data_emikat$pollutant), unique(data_emikat$pollutant))

# absolute values
plots$emissions$inventory_absolute <- 
  lapply(pollutants, function(pollutant) {
    ggplot_emissions(data = dplyr::filter(data_emikat, pollutant == !!pollutant), cols = cols_emissions, theme = theme_ts)
  })

# relative values
plots$emissions$inventory_relative <- 
  lapply(pollutants, function(pollutant) {
    ggplot_emissions(data = dplyr::filter(data_emikat, pollutant == !!pollutant), relative = TRUE, pos = "fill", cols = cols_emissions, theme = theme_ts)
  })


# read & plot RSD NOx emissions by vehicle type, fuel type and euronorm
data_rsd_per_norm <- read_local_csv(ressources_plotting$emissions$rsd_norm)

plots$emissions$rsd_norm$NOx <-
  data_rsd_per_norm |> 
  tidyr::expand(vehicle_type, vehicle_fuel_type, vehicle_euronorm) |>
  dplyr::left_join(data_rsd_per_norm, by = c("vehicle_type", "vehicle_fuel_type", "vehicle_euronorm")) |>
  dplyr::mutate(
    vehicle_type = dplyr::recode_factor(vehicle_type, !!!c("passenger car" = "Personenwagen", "light duty vehicle" = "leichte Nutzfahrzeuge")),
    vehicle_fuel_type = dplyr::recode_factor(vehicle_fuel_type, !!!c("gasoline" = "Benzin", "diesel" = "Diesel")),
    vehicle_euronorm = factor(vehicle_euronorm)
  ) |> 
  ggplot2::ggplot(aes(x = vehicle_euronorm, y = emission, group = vehicle_type, fill = vehicle_type)) +
  ggplot2::geom_bar(stat = "identity", width = 0.75, position = ggplot2::position_dodge()) +
  # ggiraph::geom_bar_interactive(mapping = ggplot2::aes(data_id = vehicle_type, tooltip = round_off(nox_emission, 1)), stat = "identity", width = 0.75, position = ggplot2::position_dodge()) +
  ggplot2::geom_linerange(mapping = aes(ymin = emission - standarderror, ymax = emission + standarderror), color = "gray60", position = ggplot2::position_dodge(width = 0.75)) +
  ggplot2::geom_segment(mapping = aes(x = as.numeric(vehicle_euronorm) - 0.45, xend = as.numeric(vehicle_euronorm) + 0.45, y = nox_emission_threshold_g_per_kg_fuel, yend = nox_emission_threshold_g_per_kg_fuel), color = "red3", linewidth = 1) +
  ggplot2::facet_wrap(vehicle_fuel_type~., strip.position = "bottom") +
  ggplot2::scale_y_continuous(limits = c(0,25), breaks = seq(0,25,5), expand = c(0.01,0.01), labels = function(x) format(x, big.mark = "'")) +
  ggplot2::scale_fill_manual(name = "Fahrzeugkategorie:", values = c("Personenwagen" = "cadetblue3", "leichte Nutzfahrzeuge" = "darkslategray")) +
  # ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1)) +
  ggplot2::ggtitle(
    label = openair::quickText("Abgasmessungen von Stickoxiden im realen Fahrbetrieb"),
    subtitle = openair::quickText("NOx Emissionen, Mittelwert pro Abgasnorm (g/kg Treibstoff)")
  ) +
  ggplot2::labs(caption = "Daten: Kanton Zürich/AWEL") +
  theme_ts +
  ggplot2::theme(
    strip.background = ggplot2::element_blank(),
    # strip.background = ggplot2::element_rect(color = "gray40"),
    strip.placement = "outside",
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom"
  )

# plots$emissions$rsd_norm$NOx <- ggiraph::girafe(ggobj = plots$emissions$rsd_norm$NOx, width_svg = 6, height_svg = 4)


# read & plot RSD NOx emissions by vehicle model year, vehicle type and fuel type
data_rsd_per_yearmodel <- read_local_csv(ressources_plotting$emissions$rsd_yearmodel)

plots$emissions$rsd_yearmodel$NOx <-
  data_rsd_per_yearmodel |> 
  tidyr::expand(vehicle_type, vehicle_fuel_type, vehicle_model_year) |> 
  dplyr::left_join(data_rsd_per_yearmodel, by = c("vehicle_type", "vehicle_fuel_type", "vehicle_model_year")) |> 
  dplyr::mutate(
    vehicle_type = dplyr::recode_factor(vehicle_type, !!!c("passenger car" = "Personenwagen", "light duty vehicle" = "leichte Nutzfahrzeuge")),
    vehicle_fuel_type = dplyr::recode_factor(vehicle_fuel_type, !!!c("gasoline" = "Benzin", "diesel" = "Diesel"))
  ) |> 
  ggplot2::ggplot(aes(x = vehicle_model_year, y = emission, group = vehicle_type, fill = vehicle_type)) +
  ggplot2::geom_bar(stat = "identity", width = 0.75, position = ggplot2::position_dodge()) +
  # ggiraph::geom_bar_interactive(mapping = ggplot2::aes(data_id = vehicle_type, tooltip = round_off(nox_emission, 1)), stat = "identity", width = 0.75, position = ggplot2::position_dodge()) +
  ggplot2::geom_linerange(mapping = aes(ymin = emission - standarderror, ymax = emission + standarderror), color = "gray60", position = ggplot2::position_dodge(width = 0.75)) +
  ggplot2::geom_step(mapping = aes(x = vehicle_model_year + 0.475, y = nox_emission_threshold_g_per_kg_fuel), color = "red3", linewidth = 1) +
  ggplot2::facet_wrap(vehicle_fuel_type~., strip.position = "bottom") +
  ggplot2::scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01), labels = function(x) format(x, big.mark = "'")) +
  ggplot2::scale_fill_manual(name = "Fahrzeugkategorie:", values = c("Personenwagen" = "cadetblue3", "leichte Nutzfahrzeuge" = "darkslategray")) +
  ggplot2::ggtitle(
    label = openair::quickText("Abgasmessungen von Stickoxiden im realen Fahrbetrieb"),
    subtitle = openair::quickText("NOx Emissionen, Mittelwert pro Fahrzeug-Modelljahr (g/kg Treibstoff)")
  ) +
  ggplot2::labs(caption = "Daten: Kanton Zürich/AWEL") +
  theme_ts +
  ggplot2::theme(
    strip.background = ggplot2::element_blank(),
    # strip.background = ggplot2::element_rect(color = "gray40"),
    strip.placement = "outside",
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom"
  )

# plots$emissions$rsd_yearmodel$NOx <- ggiraph::girafe(ggobj = plots$emissions$rsd_yearmodel$NOx, width_svg = 6, height_svg = 4)


# read & plot RSD NOx emission time series (year of measurement) by fuel type
data_rsd_per_yearmeas <- read_local_csv(ressources_plotting$emissions$rsd_yearmeas)

plots$emissions$rsd_yearmeas$NOx <-
  data_rsd_per_yearmeas |> 
  dplyr::mutate(vehicle_fuel_type = dplyr::recode_factor(vehicle_fuel_type, !!!c("all" = "Benzin & Diesel", "gasoline" = "Benzin", "diesel" = "Diesel"))) |> 
  ggplot2::ggplot(aes(x = year, y = emission)) +
  ggplot2::geom_smooth(mapping = aes(color = vehicle_fuel_type), se = TRUE, span = 0.6, level = 0.95) +
  # ggiraph::geom_smooth_interactive(mapping = aes(color = vehicle_fuel_type, data_id = vehicle_fuel_type, tooltip = round_off(nox_emission, 1)), se = TRUE, span = 0.6, level = 0.95) +
  # ggplot2::geom_point(color = "gray60") +
  ggplot2::scale_x_continuous(expand = c(0.01,0.01)) +
  ggplot2::scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01), labels = function(x) format(x, big.mark = "'")) +
  ggplot2::scale_color_manual(name = "Treibstoff:", values = c("Benzin & Diesel" = "gray40", "Benzin" = "gold3", "Diesel" = "red3")) +
  ggplot2::ggtitle(
    label = openair::quickText("Abgasmessungen von Stickoxiden im realen Fahrbetrieb"),
    subtitle = openair::quickText("NOx Emissionen, Trend der Mittelwerte (g/kg Treibstoff)") # ... "Trend der Mittelwerte Personenwagen und leichte Nutzfahrzeuge", but too long for subtitle
  ) +
  ggplot2::labs(caption = "Daten: Kanton Zürich/AWEL") +
  theme_ts +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom"
  )

# plots$emissions$rsd_yearmeas$NO <- ggiraph::girafe(ggobj = plots$emissions$rsd_yearmeas$NO, width_svg = 6, height_svg = 4) 





# plotting air pollutant monitoring data
# ---
# read airquality monitoring data
data_monitoring_aq <- 
  read_local_csv(ressources_plotting$monitoring$airquality, locale = readr::locale(encoding = "UTF-8")) |> 
  dplyr::mutate(siteclass = factor(siteclass, levels = siteclass_levels)) |> 
  dplyr::filter(year %in% years & parameter %in% parameters_timeseries & !is.na(siteclass) & !(siteclass %in% c("ländlich - verkehrsbelastet", "klein-/vorstädtisch - verkehrsbelastet"))) 


# plot timeseries of yearly values for selected pollutants
plots$monitoring$timeseries_siteclass <- plot_pars_monitoring_timeseries(data_monitoring_aq, parameters_timeseries)


# read pre-compiled Ostluft y1 monitoring data for nitrogen deposition to sensitive ecosystems into separate dataset
data_monitoring_ndep <- read_local_csv(ressources_plotting$monitoring$ndep, locale = readr::locale(encoding = "UTF-8"))
data_monitoring_ndep <- 
  data_monitoring_ndep |> 
  dplyr::mutate(
    component = factor(component, levels = rev(c("N-Deposition", "aus NH3-Quellen", "aus NOx-Quellen"))),
    ecosystem_category = factor(ecosystem_category, levels = rev(c("Hochmoor", "Flachmoor", "Trockenrasen", "Wald")))
  )


# plot relative comparison latest n_years of measurement data vs. LRV Immissionsgrenzwerte + Critical Loads of Nitrogen and WHO-Richtwerte
data_thrshlds <- dplyr::distinct(immission_threshold_values, source, col, lty, lsz)
data_temp <-
  data_monitoring_ndep |>
  dplyr::filter(year %in% seq(max(years) - n_years + 1, max(years), 1)) |>
  dplyr::filter(component == "N-Deposition") |>
  dplyr::mutate(
    value = deposition / critical_load_single,
    pollutant = factor(component),
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
  dplyr::filter(year %in% seq(max(years) - n_years + 1, max(years), 1)) |>
  dplyr::select(year, pollutant, metric, value_relative_lrv, value_relative_who, siteclass) |>
  tidyr::gather(reference, value, -year, -pollutant, -metric, -siteclass) |>
  dplyr::filter(!is.na(value) & !(siteclass %in% c("ländlich - verkehrsbelastet", "klein-/vorstädtisch - verkehrsbelastet"))) |>
  dplyr::bind_rows(data_temp) |>
  dplyr::mutate(
    pollutant = dplyr::recode_factor(pollutant, "N-Deposition" = "Stickstoffeintrag in empfindliche Ökosysteme"),
    metric = dplyr::recode_factor(metric, "Jahreseintrag" = ""),
    x = paste0(pollutant, " ", metric),
    x = factor(x, levels = rev(sort(unique(.data$x)))),
    reference = dplyr::recode(reference, !!!c("value_relative_lrv" = "relativ zu Immissionsgrenzwerten bzw. kritischen Eintragsraten:", 
                                              "value_relative_who" = "relativ zu Richtwerten der Weltgesundheitsorganisation:"))
  ) |>
  ggplot2::ggplot(aes(x = x, y = value, color = siteclass)) +
  ggplot2::geom_hline(yintercept = 1, linetype = data_thrshlds$lty, color = data_thrshlds$col, linewidth = data_thrshlds$lsz, show.legend = FALSE) +
  ggplot2::geom_jitter(shape = 21, size = pointsize, width = 0.2) +
  lemon::facet_rep_wrap(reference~., scales = "free_y", ncol = 1, repeat.tick.labels = TRUE) +
  # lemon::facet_rep_grid(reference~., scales = "free_y", space = "free_y", repeat.tick.labels = TRUE) +
  ggplot2::scale_y_continuous(breaks = seq(0,10,1), limits = c(0,NA), labels = scales::percent_format(), expand = c(0.01,0.01)) +
  ggplot2::coord_flip() +
  ggplot2::guides(color = ggplot2::guide_legend(nrow = 3)) +
  ggplot2::ggtitle(
    label = openair::quickText("Luftqualitätsmesswerte Schwellenwertvergleich"),
    subtitle = paste0("Jahre ", max(years) - n_years + 1, " bis ", max(years))
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
  data_monitoring_ndep |>
  dplyr::filter(site == "BA" & component != "N-Deposition") |>
  dplyr::group_by(year, site, site_long, siteclass, ecosystem_category, critical_load_min, critical_load_single, critical_load_max, component, unit) |>
  dplyr::summarise(deposition = sum(deposition)) |>
  dplyr::ungroup() |>
  plot_timeseries_ndep_bars(xlim = c(2000,NA), linewidth = temp$lsz, color = temp$col, title = "Luftqualitätsmesswerte Stickstoffeintrag in empfindliche Ökosysteme am Bachtel") +
  ggplot2::geom_text(data = dplyr::filter(data_monitoring_ndep, site == "BA" & component == "N-Deposition" & estimate == "geschätzt"), label = "*", color = "gray40") +
  ggplot2::labs(caption = "*: mind. NH3 gemessen, restlicher Eintrag geschätzt; Daten: Ostluft & FUB") +
  lemon::facet_rep_wrap(ecosystem_category~., ncol = 1, scales = "free_y", repeat.tick.labels = TRUE)


# plot timeseries of yearly nitrogen deposition across several monitoring sites (structured per ecosystem type)
plots$monitoring$timeseries_ndep_all$Ndep <-
  data_monitoring_ndep |>
  dplyr::filter(year >= 2000 & component == "N-Deposition") |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = deposition, color = ecosystem_category, shape = estimate)) +
  ggplot2::geom_jitter(size = pointsize * 2, width = 0) +
  ggplot2::scale_x_continuous(limits = c(2000,NA), expand = c(0.01,0.01)) +
  ggplot2::scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01)) +
  ggplot2::scale_color_viridis_d(direction = -1) +
  ggplot2::ggtitle(
    label = openair::quickText("Luftqualitätsmesswerte - Stickstoffeintrag in empfindliche Ökosysteme"),
    subtitle = expression("Stickstoffeintrag (kgN " * ha^-1 * Jahr^-1 * ")")
  ) +
  ggplot2::labs(caption = "geschätzt: mind. NH3 gemessen, restlicher Eintrag geschätzt; Daten: Ostluft") +
  theme_ts +
  ggplot2::theme(
    legend.title = ggplot2::element_blank()
  )


# plot timeseries of yearly nitrogen deposition vs. critical loads of nitrogen across several monitoring sites (structured per ecosystem type)
plots$monitoring$timeseries_ndep_all_vs_CLN$Ndep <-
  data_monitoring_ndep |>
  dplyr::filter(year >= 2000 & component == "N-Deposition") |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = deposition / critical_load_single, color = ecosystem_category, shape = estimate)) +
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), color = temp$col, linewidth = temp$lsz, show.legend = FALSE) +
  ggplot2::geom_jitter(size = pointsize * 2, width = 0) +
  ggplot2::scale_x_continuous(limits = c(2000,NA), expand = c(0.01,0.01)) +
  ggplot2::scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01), labels = scales::percent_format()) +
  ggplot2::scale_color_viridis_d(direction = -1) +
  ggplot2::ggtitle(
    label = openair::quickText("Luftqualitätsmesswerte Stickstoffeintrag in empfindliche Ökosysteme"),
    subtitle = expression("Stickstoffeintrag vs. kritische Eintragsrate (relativ)")
  ) +
  ggplot2::labs(caption = "geschätzt: mind. NH3 gemessen, restlicher Eintrag geschätzt; Daten: Ostluft") +
  theme_ts +
  ggplot2::theme(
    legend.title = ggplot2::element_blank()
  )


# plot mean contribution of source categories to nitrogen deposition
plots$monitoring$ndep_mean_sources_fractions$Ndep <-
  data_monitoring_ndep |>
  dplyr::filter(year >= 2019 & component != "N-Deposition") |> 
  dplyr::group_by(year, component) |> 
  dplyr::summarise(deposition = mean(deposition)) |> 
  dplyr::ungroup() |> 
  ggplot2::ggplot(ggplot2::aes(x = year, y = deposition, fill = component)) +
  geom_bar(stat = "identity", position = "fill") +
  ggplot2::scale_x_continuous(breaks = seq(2018,max(years),1), expand = c(0.01,0.01)) +
  ggplot2::scale_y_continuous(expand = c(0.01,0.01), labels = scales::percent_format()) +
  ggplot2::scale_fill_manual(values = c("aus NOx-Quellen" = "steelblue4", "aus NH3-Quellen" = "gold2")) +
  ggplot2::ggtitle(
    label = openair::quickText("Luftqualitätsmesswerte Stickstoffeintrag in empfindliche Ökosysteme seit dem Jahr 2019"),
    subtitle = expression("mittlerer Beitrag der Quellgruppen")
  ) +
  ggplot2::labs(caption = "Daten: Ostluft") +
  theme_ts +
  ggplot2::theme(
    legend.title = ggplot2::element_blank()
  )






# plotting air pollutant population and ecosystem exposition
# ---
# read exposition data and setup
data_expo_distr_pollutants <- read_local_csv(ressources_plotting$exposition$expo_distr_pollutants, locale = readr::locale(encoding = "UTF-8")) 
data_expo_distr_ndep <- read_local_csv(ressources_plotting$exposition$expo_distr_ndep, locale = readr::locale(encoding = "UTF-8")) 
data_expo_weighmean_canton <- read_local_csv(ressources_plotting$exposition$weightedmean_canton, locale = readr::locale(encoding = "UTF-8")) 
data_expo_weighmean_municip <- read_local_csv(ressources_plotting$exposition$weightedmean_municip, locale = readr::locale(encoding = "UTF-8")) 
parameters_exposition <- setNames(parameters_exposition, parameters_exposition)

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
  dplyr::full_join(map_municipalities, by = "geodb_oid") |> 
  sf::st_as_sf()

plots$exposition$population_weighted_mean_map <-
  lapply(parameters_exposition, function(parameter) {
    plot_all_popweighmean_maps(parameter, data_expo_weighmean_municip, data_expo_weighmean_canton)
  })


# plotting timeseries of population-weighted mean pollutant concentration for Canton Zürich
plots$exposition$population_weighted_mean <- plot_pars_popmean_timeseries(data_expo_weighmean_canton, parameters_timeseries)





# plotting selected health-outcomes due to population exposition by air pollutants
# ---
data_outcomes <- read_local_csv(file = ressources_plotting$outcomes$outcomes, locale = readr::locale(encoding = "UTF-8"))

# plotting timeseries of preliminary deaths for Canton Zürich
plots$outcomes$preliminary_deaths_abs <- plot_pars_prelim_deaths_timeseries(data_outcomes, c("PM2.5", "NO2", "O3_peakseason_mean_d1_max_mean_h8gl"), relative = FALSE)

# plotting timeseries of preliminary deaths per 100'000 inhabitants for Canton Zürich
plots$outcomes$preliminary_deaths_rel <- plot_pars_prelim_deaths_timeseries(data_outcomes, c("PM2.5", "NO2", "O3_peakseason_mean_d1_max_mean_h8gl"), relative = TRUE)

# plotting timeseries of years of life lost for Canton Zürich
# TODO ...








# construct final plot tibble instead of plot list for better use in *.qmd
# ---
plots <-
  plots$emissions$inventory_absolute |> 
  plotlist_to_tibble("emission", "inventory_absolute") |> 
  bind_rows(plotlist_to_tibble(plots$emissions$inventory_relative, "emission", "inventory_relative")) |> 
  bind_rows(plotlist_to_tibble(plots$emissions$rsd_norm, "emission", "rsd_norm")) |> 
  bind_rows(plotlist_to_tibble(plots$emissions$rsd_yearmodel, "emission", "rsd_yearmodel")) |> 
  bind_rows(plotlist_to_tibble(plots$emissions$rsd_yearmeas, "emission", "rsd_yearmeas")) |> 
  bind_rows(plotlist_to_tibble(plots$monitoring$threshold_comparison, "monitoring", "threshold_comparison")) |> 
  bind_rows(plotlist_to_tibble(plots$monitoring$timeseries_siteclass, "monitoring", "timeseries_siteclass")) |> 
  bind_rows(plotlist_to_tibble(plots$monitoring$timeseries_ndep_bachtel, "monitoring", "timeseries_ndep_bachtel")) |> 
  bind_rows(plotlist_to_tibble(plots$monitoring$timeseries_ndep_all, "monitoring", "timeseries_ndep_all")) |>
  bind_rows(plotlist_to_tibble(plots$monitoring$timeseries_ndep_all_vs_CLN, "monitoring", "timeseries_ndep_all_vs_CLN")) |>
  bind_rows(plotlist_to_tibble(plots$exposition$distribution_histogram, "exposition", "distribution_histogram")) |>
  bind_rows(plotlist_to_tibble(plots$exposition$distribution_cumulative, "exposition", "distribution_cumulative")) |>
  bind_rows(plotlist_to_tibble(plots$exposition$population_weighted_mean, "exposition", "population_weighted_mean")) |>
  bind_rows(plotlist_to_tibble(plots$exposition$population_weighted_mean_map, "exposition", "population_weighted_mean_map")) |> 
  bind_rows(plotlist_to_tibble(plots$outcomes$preliminary_deaths_abs, "outcomes", "preliminary_deaths_abs")) |> 
  bind_rows(plotlist_to_tibble(plots$outcomes$preliminary_deaths_rel, "outcomes", "preliminary_deaths_rel"))



# save for *.qmd & clean up
# ---
saveRDS(dplyr::filter(plots, type == "emission"), "docs/plots_emissions.rds")
saveRDS(dplyr::filter(plots, type == "monitoring"), "docs/plots_monitoring.rds")
saveRDS(dplyr::filter(plots, type == "exposition"), "docs/plots_exposition.rds")
saveRDS(dplyr::filter(plots, type == "outcomes"), "docs/plots_outcomes.rds")

rm(list = c("map_municipalities", "ressources_plotting", "scale_color_siteclass", "scale_fill_siteclass", "temp", "theme_map", "theme_ts", "threshold_ndep",
            "data_emikat", "data_expo_distr_ndep", "data_expo_distr_pollutants", "data_expo_weighmean_canton", "thresh",
            "data_monitoring_aq", "data_monitoring_ndep", "data_rsd_per_norm", "data_rsd_per_yearmodel", "data_rsd_per_yearmeas", "data_temp", "data_thrshlds",
            "data_expo_weighmean_municip", "immission_threshold_values", "map_canton", "basesize", "col_lrv", "col_who", "cols_emissions", 
            "crs", "lbsz", "linewidth", "lsz_lrv", "lsz_who", "lty_lrv", "lty_who", "n_years", "parameters_exposition", "parameters_timeseries",
            "pointsize", "pollutants", "siteclass_levels", "years", "map_municipalities", "crs", "ressources"))



