
# setup analysis: load libraries & functions & read map boundaries data
source("scripts/_setup.R", encoding = "UTF-8")

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
    )
  )

# data subsetting parameters
years <- 1995:(lubridate::year(Sys.Date()) - 1) # years to consider for plotting 
n_years <- 3 # consider last 3 years for plotting relative threshold comparison    
parameters_timeseries <- c("NO2", "PM10", "PM2.5", "O3_max_98p_m1", "O3_peakseason_mean_d1_max_mean_h8gl") # parameters to include for timeseries plotting
parameters_exposition <- c("NO2", "O3_max_98p_m1", "PM10", "PM2.5", "eBC") # parameters to include for exposition plotting
siteclass_levels <- rev(c("ländlich - Hintergrund", "klein-/vorstädtisch - Hintergrund",
                          "städtisch - Hintergrund", "städtisch - verkehrsbelastet"))

# plotting size parameters
basesize <- 12 # ggplot theme base_size
pointsize <- 2 # size of point markers
linewidth <- 1 # width of lines

# read LRV legal threshold limit values & WHO air quality guideline values
immission_threshold_values <- readr::read_delim(filter_ressources(ressources, 10), delim = ";",locale = readr::locale(encoding = "UTF-8"))
update_log(10)

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

cols_emissions <- c(wesanderson::wes_palette(name = "BottleRocket2", n = 5, type = "discrete"), "#003333")

# ggplot2 custom themes
theme_ts <-
  theme_minimal(base_size = basesize) +
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
  theme_void(base_size = basesize) +
  theme(
    plot.subtitle = element_text(size = ggplot2::rel(0.8)),
    plot.caption = element_text(hjust = 1, color = "gray40", face = "italic", size = ggplot2::rel(0.75)),
    panel.background = element_blank(),
    plot.background = element_blank()
  )

# empty list to collect all plots
plots <- list() 






# plotting air pollutant emissions

# read & plot details of Canton Zürich air pollutant emissions per pollutant, subsector and year (absolute and relative values)
data_emikat <- readr::read_delim(ressources_plotting$emissions$emikat, delim = ";")
cols_emissions <- setNames(as.character(cols_emissions), unique(data_emikat$sector))
pollutants <- setNames(unique(data_emikat$pollutant), unique(data_emikat$pollutant))

# absolute values
plots$emissions$absolute <- 
  lapply(pollutants, function(pollutant) {
    ggplot_emissions(data = dplyr::filter(data_emikat, pollutant == !!pollutant), cols = cols_emissions, theme = theme_ts) #FIXME: fill shading doesn't work right
  })
update_log(29)  

# relative values
plots$emissions$relative <- 
  lapply(pollutants, function(pollutant) {
    ggplot_emissions(data = dplyr::filter(data_emikat, pollutant == !!pollutant), relative = TRUE, pos = "fill", cols = cols_emissions, theme = theme_ts)
  })
update_log(29)

# read & plot RSD NOx emissions by vehicle type, fuel type and euronorm
data_rsd_per_norm <- readr::read_delim(ressources_plotting$emissions$rsd_norm, delim = ";")

plots$emissions$NOx$rsd_norm <-
  data_rsd_per_norm |> 
  tidyr::expand(vehicle_type, vehicle_fuel_type, vehicle_euronorm) |>
  dplyr::left_join(data_rsd_per_norm, by = c("vehicle_type", "vehicle_fuel_type", "vehicle_euronorm")) |>
  dplyr::mutate(
    vehicle_type = dplyr::recode_factor(vehicle_type, !!!c("passenger car" = "Personenwagen", "light duty vehicle" = "leichte Nutzfahrzeuge")),
    vehicle_fuel_type = dplyr::recode_factor(vehicle_fuel_type, !!!c("gasoline" = "Benzin", "diesel" = "Diesel")),
    vehicle_euronorm = factor(vehicle_euronorm)
  ) |> 
  ggplot2::ggplot(aes(x = vehicle_euronorm, y = NOx_emission, group = vehicle_type, fill = vehicle_type)) +
  ggplot2::geom_bar(stat = "identity", width = 0.75, position = ggplot2::position_dodge()) +
  ggplot2::geom_linerange(mapping = aes(ymin = NOx_emission - standarderror, ymax = NOx_emission + standarderror), color = "gray60", position = ggplot2::position_dodge(width = 0.75)) +
  ggplot2::geom_segment(mapping = aes(x = as.numeric(vehicle_euronorm) - 0.45, xend = as.numeric(vehicle_euronorm) + 0.45, y = NOx_emission_threshold_g_per_kg_fuel, yend = NOx_emission_threshold_g_per_kg_fuel), color = "red3", linewidth = 1) +
  ggplot2::facet_wrap(vehicle_fuel_type~., strip.position = "bottom") +
  ggplot2::scale_y_continuous(limits = c(0,25), breaks = seq(0,25,5), expand = c(0.01,0.01), labels = function(x) format(x, big.mark = "'")) +
  ggplot2::scale_fill_manual(name = "Fahrzeugkategorie:", values = c("Personenwagen" = "cadetblue3", "leichte Nutzfahrzeuge" = "darkslategray")) +
  # ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1)) +
  ggplot2::ggtitle(
    label = openair::quickText("Abgasmessungen von Stickoxiden (NOx) im realen Fahrbetrieb"),
    subtitle = openair::quickText("NOx Emissionen, Mittelwert pro Abgasnorm (g/kg Treibstoff)")
  ) +
  ggplot2::labs(caption = "Quelle: Kanton Zürich/AWEL") +
  theme_ts +
  ggplot2::theme(
    strip.background = ggplot2::element_blank(),
    # strip.background = ggplot2::element_rect(color = "gray40"),
    strip.placement = "outside",
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom"
  )
update_log(30)

# read & plot RSD NOx emissions by vehicle model year, vehicle type and fuel type
data_rsd_per_yearmodel <- readr::read_delim(ressources_plotting$emissions$rsd_yearmodel, delim = ";")

plots$emissions$NOx$rsd_yearmodel <-
  data_rsd_per_yearmodel |> 
  tidyr::expand(vehicle_type, vehicle_fuel_type, vehicle_model_year) |> 
  dplyr::left_join(data_rsd_per_yearmodel, by = c("vehicle_type", "vehicle_fuel_type", "vehicle_model_year")) |> 
  dplyr::mutate(
    vehicle_type = dplyr::recode_factor(vehicle_type, !!!c("passenger car" = "Personenwagen", "light duty vehicle" = "leichte Nutzfahrzeuge")),
    vehicle_fuel_type = dplyr::recode_factor(vehicle_fuel_type, !!!c("gasoline" = "Benzin", "diesel" = "Diesel"))
  ) |> 
  ggplot2::ggplot(aes(x = vehicle_model_year, y = NOx_emission, group = vehicle_type, fill = vehicle_type)) +
  ggplot2::geom_bar(stat = "identity", width = 0.75, position = ggplot2::position_dodge()) +
  ggplot2::geom_linerange(mapping = aes(ymin = NOx_emission - standarderror, ymax = NOx_emission + standarderror), color = "gray60", position = ggplot2::position_dodge(width = 0.75)) +
  ggplot2::geom_step(mapping = aes(x = vehicle_model_year + 0.475, y = NOx_emission_threshold_g_per_kg_fuel), color = "red3", linewidth = 1) +
  ggplot2::facet_wrap(vehicle_fuel_type~., strip.position = "bottom") +
  ggplot2::scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01), labels = function(x) format(x, big.mark = "'")) +
  ggplot2::scale_fill_manual(name = "Fahrzeugkategorie:", values = c("Personenwagen" = "cadetblue3", "leichte Nutzfahrzeuge" = "darkslategray")) +
  ggplot2::ggtitle(
    label = openair::quickText("Abgasmessungen von Stickoxiden (NOx) im realen Fahrbetrieb"),
    subtitle = openair::quickText("NOx Emissionen, Mittelwert pro Fahrzeug-Modelljahr (g/kg Treibstoff)")
  ) +
  ggplot2::labs(caption = "Quelle: Kanton Zürich/AWEL") +
  theme_ts +
  ggplot2::theme(
    strip.background = ggplot2::element_blank(),
    # strip.background = ggplot2::element_rect(color = "gray40"),
    strip.placement = "outside",
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom"
  )
update_log(30)

# read & plot RSD NOx emission time series (year of measurement) by fuel type
data_rsd_per_yearmeas <- readr::read_delim(ressources_plotting$emissions$rsd_yearmeas, delim = ";")

plots$emissions$NOx$rsd_yearmeas <-
  data_rsd_per_yearmeas |> 
  dplyr::mutate(vehicle_fuel_type = dplyr::recode_factor(vehicle_fuel_type, !!!c("all" = "Benzin & Diesel", "gasoline" = "Benzin", "diesel" = "Diesel"))) |> 
  ggplot2::ggplot(aes(x = year, y = NOx_emission)) +
  ggplot2::geom_smooth(mapping = aes(color = vehicle_fuel_type), se = TRUE, span = 0.6, level = 0.95) +
  # ggplot2::geom_point(color = "gray60") +
  ggplot2::scale_x_continuous(expand = c(0.01,0.01)) +
  ggplot2::scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01), labels = function(x) format(x, big.mark = "'")) +
  ggplot2::scale_color_manual(name = "Treibstoff:", values = c("Benzin & Diesel" = "gray40", "Benzin" = "gold3", "Diesel" = "red3")) +
  ggplot2::ggtitle(
    label = openair::quickText("Abgasmessungen von Stickoxiden (NOx) im realen Fahrbetrieb"),
    subtitle = openair::quickText("NOx Emissionen, Trend der Mittelwerte (g/kg Treibstoff)") # ... "Trend der Mittelwerte Personenwagen und leichte Nutzfahrzeuge", but too long for subtitle
  ) +
  ggplot2::labs(caption = "Quelle: Kanton Zürich/AWEL") +
  theme_ts +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom"
  )
update_log(30)




# plotting air pollutant monitoring data

# read airquality monitoring data
data_monitoring_aq <- 
  readr::read_delim(ressources_plotting$monitoring$airquality, delim = ";") |> 
  dplyr::mutate(siteclass = factor(siteclass, levels = siteclass_levels)) |> 
  dplyr::filter(year %in% years & parameter %in% parameters_timeseries & 
                  !is.na(siteclass) & !(siteclass %in% c("ländlich - verkehrsbelastet", "klein-/vorstädtisch - verkehrsbelastet"))) 

# plot timeseries of yearly values for selected pollutants
plots$airquality$monitoring$timeseries <- plot_pars_monitoring_timeseries(data_monitoring_aq, parameters_timeseries)
update_log(31)

# read pre-compiled OSTLUFT y1 monitoring data for nitrogen deposition to sensitive ecosystems into separate dataset
data_monitoring_ndep <- readr::read_delim(ressources_plotting$monitoring$ndep, delim = ";")
data_monitoring_ndep <- 
  data_monitoring_ndep |> 
  dplyr::mutate(
    parameter = factor(parameter, levels = rev(c("N-Deposition", "aus NH3-Quellen", "aus NOx-Quellen"))),
    ecosystem_category = factor(ecosystem_category, levels = rev(c("Hochmoor", "Flachmoor", "Trockenrasen", "Wald")))
  )

# plot relative comparison latest n_years of measurement data vs. LRV Immissionsgrenzwerte + Critical Loads of Nitrogen and WHO-Richtwerte
data_thrshlds <- dplyr::distinct(immission_threshold_values, source, col, lty, lsz)
data_temp <-
  data_monitoring_ndep |>
  dplyr::filter(year %in% seq(max(years) - n_years + 1, max(years), 1)) |>
  dplyr::filter(parameter == "N-Deposition") |>
  dplyr::mutate(
    value = value / critical_load_single,
    parameter = factor(parameter),
    reference = factor("value_relative_lrv"),
    siteclass = "empf. Ökosystem"
  ) |>
  dplyr::select(year, parameter, reference, value, siteclass)

plots$airquality$monitoring$threshold_comparison <-
  data_monitoring_aq |>
  combine_thresholds(immission_threshold_values) |>
  dplyr::mutate(
    value_relative_lrv = value / `LRV Grenzwert`,
    value_relative_who = value / `WHO Richtwert`
  ) |>
  dplyr::filter(year %in% seq(max(years) - n_years + 1, max(years), 1)) |>
  dplyr::select(year, parameter, value_relative_lrv, value_relative_who, siteclass) |>
  tidyr::gather(reference, value, -year, -parameter, -siteclass) |>
  dplyr::filter(!is.na(value) & !(siteclass %in% c("ländlich - verkehrsbelastet", "klein-/vorstädtisch - verkehrsbelastet"))) |>
  dplyr::bind_rows(data_temp) |>
  dplyr::mutate(
    parameter = dplyr::recode_factor(parameter, !!!c("NO2" = "NO2 Jahresmittel", "PM2.5" = "Feinstaub PM2.5 Jahresmittel", "PM10" = "Feinstaub PM10 Jahresmittel", 
                                                     "O3_max_98p_m1" = "O3 max. 98% der monatl. ½-Stundenwerte", "O3_peakseason_mean_d1_max_mean_h8gl" = "O3 mittlere Sommersaison-Tagesbelastung", 
                                                     "N-Deposition" = "Stickstoffeintrag in empfindliche Ökosysteme")),
    parameter = factor(parameter, levels = rev(c("Feinstaub PM2.5 Jahresmittel", "Feinstaub PM10 Jahresmittel", "NO2 Jahresmittel", 
                                                 "O3 max. 98% der monatl. ½-Stundenwerte", "O3 mittlere Sommersaison-Tagesbelastung", "Stickstoffeintrag in empfindliche Ökosysteme"))),
    reference = dplyr::recode(reference, !!!c("value_relative_lrv" = "relativ zu Immissionsgrenzwerten bzw. kritischen Eintragsraten:", 
                                              "value_relative_who" = "relativ zu Richtwerten der Weltgesundheitsorganisation:"))
  ) |>
  ggplot2::ggplot(aes(x = parameter, y = value, color = siteclass)) +
  ggplot2::geom_hline(yintercept = 1, linetype = data_thrshlds$lty, color = data_thrshlds$col, linewidth = data_thrshlds$lsz, show.legend = FALSE) +
  ggplot2::geom_jitter(shape = 21, size = pointsize, width = 0.2) +
  lemon::facet_rep_wrap(reference~., scales = "free_y", ncol = 1, repeat.tick.labels = TRUE) +
  # lemon::facet_rep_grid(reference~., scales = "free_y", space = "free_y", repeat.tick.labels = TRUE) +
  ggplot2::scale_y_continuous(breaks = seq(0,10,1), limits = c(0,NA), labels = scales::percent_format(), expand = c(0.01,0.01)) +
  ggplot2::coord_flip() +
  ggplot2::guides(color = ggplot2::guide_legend(nrow = 2)) +
  ggplot2::ggtitle(
    label = openair::quickText("Luftqualitätsmesswerte - Schwellenwertvergleich Langzeitbelastung"),
    subtitle = paste0("Jahre ", max(years) - n_years + 1, " bis ", max(years))
  ) +
  ggplot2::labs(caption = "Quelle: OSTLUFT & NABEL (BAFU & Empa)") +
  theme_ts +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.major.x = element_line(),
    strip.text = ggplot2::element_text(hjust = 0)
  ) +
  scale_color_siteclass
update_log(31)
update_log(32)

# plot long-standing timeseries of yearly nitrogen deposition at Bachtel site (since 2001)
temp <- dplyr::filter(immission_threshold_values, source == "LRV Grenzwert" & pollutant == "NO2")
plots$airquality$monitoring$Ndep$timeseries_Bachtel <-
  data_monitoring_ndep |>
  dplyr::filter(site == "BA" & parameter != "N-Deposition") |>
  dplyr::group_by(year, site, site_long, siteclass, ecosystem_category, critical_load_min, critical_load_single, critical_load_max, parameter, unit) |>
  dplyr::summarise(value = sum(value)) |>
  dplyr::ungroup() |>
  plot_timeseries_ndep_bars(xlim = c(2000,NA), linewidth = temp$lsz, color = temp$col, title = "Luftqualitätsmesswerte - Stickstoffeintrag in empfindliche Ökosysteme am Bachtel") +
  ggplot2::geom_text(data = dplyr::filter(data_monitoring_ndep, site == "BA" & parameter == "N-Deposition" & estimate == "geschätzt"), label = "*", color = "gray40") +
  ggplot2::labs(caption = "*: mind. NH3 gemessen, restlicher Eintrag geschätzt; Quelle: OSTLUFT & FUB") +
  lemon::facet_rep_wrap(ecosystem_category~., ncol = 1, scales = "free_y", repeat.tick.labels = TRUE)
update_log(32)

# plot timeseries of yearly nitrogen deposition across several monitoring sites since 2019 (structured per ecosystem type)
plots$airquality$monitoring$Ndep$all_timeseries <-
  data_monitoring_ndep |>
  dplyr::filter(year >= 2019 & parameter == "N-Deposition") |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = value, color = ecosystem_category, shape = estimate)) +
  ggplot2::geom_jitter(size = pointsize * 2, width = 0.1) +
  ggplot2::scale_x_continuous(expand = c(0.01,0.01)) +
  ggplot2::scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01)) +
  ggplot2::scale_color_viridis_d(direction = -1) +
  ggplot2::ggtitle(
    label = openair::quickText("Luftqualitätsmesswerte - Stickstoffeintrag in empfindliche Ökosysteme seit dem Jahr 2019"),
    subtitle = expression("Stickstoffeintrag (kgN " * ha^-1 * Jahr^-1 * ")")
  ) +
  ggplot2::labs(caption = "geschätzt: mind. NH3 gemessen, restlicher Eintrag geschätzt; Quelle: OSTLUFT") +
  theme_ts +
  ggplot2::theme(
    legend.title = ggplot2::element_blank()
  )
update_log(32)

# plot timeseries of yearly nitrogen deposition vs. critical loads of nitrogen across several monitoring sites since 2019 (structured per ecosystem type)
plots$airquality$monitoring$Ndep$all_timeseries_vs_CLN <-
  data_monitoring_ndep |>
  dplyr::filter(year >= 2019 & parameter == "N-Deposition") |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = value / critical_load_single, color = ecosystem_category, shape = estimate)) +
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), color = temp$col, linewidth = temp$lsz, show.legend = FALSE) +
  ggplot2::geom_jitter(size = pointsize * 2, width = 0.1) +
  ggplot2::scale_x_continuous(expand = c(0.01,0.01)) +
  ggplot2::scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01), labels = scales::percent_format()) +
  ggplot2::scale_color_viridis_d(direction = -1) +
  ggplot2::ggtitle(
    label = openair::quickText("Luftqualitätsmesswerte - Stickstoffeintrag in empfindliche Ökosysteme seit dem Jahr 2019"),
    subtitle = expression("Stickstoffeintrag vs. kritische Eintragsrate (relativ)")
  ) +
  ggplot2::labs(caption = "geschätzt: mind. NH3 gemessen, restlicher Eintrag geschätzt; Quelle: OSTLUFT") +
  theme_ts +
  ggplot2::theme(
    legend.title = ggplot2::element_blank()
  )
update_log(32)

# plot mean contribution of source categories to nitrogen deposition
plots$airquality$monitoring$Ndep$mean_sources_fractions <-
  data_monitoring_ndep |>
  dplyr::filter(year >= 2019 & parameter != "N-Deposition") |> 
  dplyr::group_by(year, parameter) |> 
  dplyr::summarise(value = mean(value)) |> 
  dplyr::ungroup() |> 
  ggplot2::ggplot(ggplot2::aes(x = year, y = value, fill = parameter)) +
  geom_bar(stat = "identity", position = "fill") +
  ggplot2::scale_x_continuous(breaks = seq(2018,max(years),1), expand = c(0.01,0.01)) +
  ggplot2::scale_y_continuous(expand = c(0.01,0.01), labels = scales::percent_format()) +
  ggplot2::scale_fill_manual(values = c("aus NOx-Quellen" = "steelblue4", "aus NH3-Quellen" = "gold2")) +
  ggplot2::ggtitle(
    label = openair::quickText("Luftqualitätsmesswerte - Stickstoffeintrag in empfindliche Ökosysteme seit dem Jahr 2019"),
    subtitle = expression("mittlerer Beitrag der Quellgruppen zum Stickstoffeintrag")
  ) +
  ggplot2::labs(caption = "Quelle: OSTLUFT") +
  theme_ts +
  ggplot2::theme(
    legend.title = ggplot2::element_blank()
  )
update_log(32)





# plotting air pollutant population and ecosystem exposition

# read exposition data and setup
data_expo_distr_pollutants <- readr::read_delim(ressources_plotting$exposition$expo_distr_pollutants, delim = ";") 
data_expo_distr_ndep <- readr::read_delim(ressources_plotting$exposition$expo_distr_ndep, delim = ";") 
data_expo_weighmean_canton <- readr::read_delim(ressources_plotting$exposition$weightedmean_canton, delim = ";") 
data_expo_weighmean_municip <- readr::read_delim(ressources_plotting$exposition$weightedmean_municip, delim = ";") 
data_expo_weighmean_municip <- dplyr::full_join(map_municipalities, dplyr::select(data_expo_weighmean_municip, geodb_oid, year, parameter, pop_weighted_mean, source), by = "geodb_oid")
parameters_exposition <- setNames(parameters_exposition, parameters_exposition)

# plotting histograms for air pollutants
plots$exposition$hist <-
  lapply(parameters_exposition, function(parameter) {
    plot_all_expo_hist(parameter, data_expo_distr_pollutants)
  })
update_log(34)

# plotting histograms for sensitive ecosystems nitrogen deposition exceedance
plots$exposition$hist$Ndep <- plot_all_expo_hist_ndep(data_expo_distr_ndep, threshold_ndep)
update_log(35)

# plotting cumulative distributions for air pollutants
plots$exposition$cumul <-
  lapply(parameters_exposition, function(parameter) {
    plot_all_expo_cumul(parameter, data_expo_distr_pollutants)
  })
update_log(34)

# plotting cumulative distributions for sensitive ecosystems nitrogen deposition exceedance
plots$exposition$cumul$Ndep <- plot_all_expo_cumul_ndep(data_expo_distr_ndep, threshold_ndep)
update_log(35)

# plotting maps of population-weighted mean pollutant concentration (single value for Kanton Zürich & per municipality)
plots$exposition$population_weighted_mean <-
  lapply(parameters_exposition, function(parameter) {
    plot_all_popweighmean_maps(parameter, data_expo_weighmean_municip, data_expo_weighmean_canton)
  })
update_log(33)

# plotting timeseries of population-weighted mean pollutant concentration for Kanton Zürich
thresh <-  
  immission_threshold_values |> 
  dplyr::mutate(parameter = dplyr::case_when(metric == "monthly 98%-percentile of ½ hour mean values ≤ 100 µg/m3" ~ "O3_max_98p_m1", TRUE ~ pollutant)) |> 
  dplyr::filter(parameter %in% c("NO2", "O3_max_98p_m1", "PM10", "PM2.5")) |> 
  dplyr::mutate(parameter = paste0(longtitle(parameter), " ", longparameter(parameter)," (",shorttitle(parameter),")"))

plots$exposition$population_weighted_mean$overview <-
  data_expo_weighmean_canton |> 
  dplyr::filter(parameter != "eBC") |> 
  dplyr::mutate(parameter = paste0(longtitle(parameter), " ", longparameter(parameter)," (",shorttitle(parameter),")")) |> 
  ggplot2::ggplot(ggplot2::aes(x = year, y = pop_weighted_mean)) + 
  ggplot2::geom_bar(stat = "identity", fill = "gray40") +
  ggplot2::geom_hline(data = thresh, mapping = ggplot2::aes(yintercept = threshold), linewidth = thresh$lsz, color = thresh$col, linetype = thresh$lty) +
  lemon::facet_rep_wrap(parameter~., scales = "free_y", ncol = 1, repeat.tick.labels = TRUE) +
  ggplot2::scale_x_continuous(breaks = 2015:max(years), expand = c(0.01,0.01)) + 
  ggplot2::scale_y_continuous(expand = c(0.01,0.01)) + 
  theme_ts +
  ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0)) +
  ggplot2::ylab("bevölkerungsgewichtete mittlere Belastung (μg/m3)") +
  ggplot2::ggtitle(
    label = "Bevölkerungsgewichtete Schadstoffbelastung",
    subtitle = "Mittlere Schadstoffbelastung pro Einwohner/in"
  ) +
  ggplot2::labs(caption = "Datengrundlage: BAFU & BFS")


# clean up
rm(list = c("map_municipalities", "ressources_plotting", "scale_color_siteclass", "scale_fill_siteclass", "temp", "theme_map", "theme_ts", "threshold_ndep",
            "data_emikat", "data_expo_distr_ndep", "data_expo_distr_pollutants", "data_expo_weighmean_canton", "data_expo_weighmean_municipalities",
            "data_monitoring_aq", "data_monitoring_ndep", "data_rsd_per_norm", "data_rsd_per_yearmodel", "data_rsd_per_yearmeas", "data_temp", "data_thrshlds",
            "data_expo_weighmean_municip", "immission_threshold_values", "map_canton", "basesize", "col_lrv", "col_who", "cols_emissions", 
            "crs", "lbsz", "linewidth", "lsz_lrv", "lsz_who", "lty_lrv", "lty_who", "n_years", "parameters_exposition", "parameters_timeseries",
            "pointsize", "pollutants", "siteclass_levels", "years"))



