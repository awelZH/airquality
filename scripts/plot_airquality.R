
# get links to data sources
source("scripts/__ressources.R", encoding = "UTF-8") #FIXME: see issue 11

# setup analysis: load libraries & functions & read map boundaries data
source("scripts/_setup.R", encoding = "UTF-8")

# list of output data sources for plotting
ressources_plotting <-
  list(
    emissions = list(
      emikat = "inst/extdata/output_data_emissions.csv",
      rsd_norm = "inst/extdata/output_data_nox_vehicle_emissions_rsd_per_norm.csv",
      rsd_yearmodel = "inst/extdata/output_data_nox_emissions_rsd_per_yearmodel.csv",
      rsd_yearmeas = "inst/extdata/output_data_nox_emissions_rsd_per_yearmeas.csv"
    ),
    monitoring = list(
      airquality = "inst/extdata/output_data_airquality_monitoring_y1.csv",
      ndep = "inst/extdata/output_data_ndep_monitoring_y1.csv"
    ),
    exposition = list(
      weightedmean_canton = "inst/extdata/output_data_exposition_weighted_means_canton.csv",
      weightedmean_municip = "inst/extdata/output_data_exposition_weighted_means_municipalities.csv",
      expo_distr_pollutants = "inst/extdata/output_data_exposition_distribution_pollutants.csv",
      expo_distr_ndep ="inst/extdata/output_data_exposition_distribution_ndep.csv"
    )
  )

# data subsetting parameters
years <- 1995:(lubridate::year(Sys.Date()) - 1) # years to consider for plotting 
n_years <- 3 # consider last 3 years for plotting relative threshold comparison    
parameters_timeseries <- c("NO2", "PM10", "PM2.5", "O3_max_98%_m1", "O3_peakseason_mean_d1_max_mean_h8gl") # parameters to include for timeseries plotting
parameters_exposition <- c("NO2", "PM10", "PM2.5", "eBC") # parameters to include for exposition plotting
siteclass_levels <- rev(c("ländlich - Hintergrund", "ländlich - verkehrsbelastet", "klein-/vorstädtisch - Hintergrund",
                          "klein-/vorstädtisch - verkehrsbelastet", "städtisch - Hintergrund", "städtisch - verkehrsbelastet"))

# plotting size parameters
basesize <- 12 # ggplot theme base_size
pointsize <- 2 # size of point markers
linewidth <- 1 # width of lines

# read LRV legal threshold limit values & WHO air quality guideline values
immission_threshold_values <- readr::read_delim(paste("inst/extdata", files$airquality$thresh, sep = "/"), delim = ";",locale = readr::locale(encoding = "UTF-8"))

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
  ) %>% 
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
# ------------------------------------------------------------

# plot details of Canton Zürich air pollutant emissions per pollutant, subsector and year (absolute values)
data_emikat <- readr::read_delim(ressources_plotting$emissions$emikat, delim = ";")
cols_emissions <- setNames(as.character(cols_emissions), unique(data_emikat$sector))
plots$emissions <- 
  setNames(unique(data_emikat$pollutant), unique(data_emikat$pollutant)) %>% 
  lapply(function(x) {
    
    list(
      
      absolute =
        data_emikat %>% 
        dplyr::filter(pollutant == x) %>% 
        ggplot_emissions(cols = cols_emissions, theme = theme_ts) + 
        ggplot2::ggtitle(
          label = openair::quickText(paste0("Luftschadstoff-Emissionen ", longtitle(x)," (",x,")")),
          subtitle = as.expression(substitute(a * ", Jahresmenge nach Quellgruppen (t " * Jahr^-1 * ")", env = list(a = x)))
        ) +
        ggplot2::labs(caption = "Quelle: OSTLUFT, Grundlage: EMIS Schweiz"), 
      
      relative = 
        data_emikat %>% 
        dplyr::filter(pollutant == x) %>% 
        ggplot_emissions(cols_emissions, pos = "fill", width = 0.75, theme = theme_ts) + 
        ggplot2::scale_y_continuous(labels = scales::percent_format(), expand = c(0.01,0.01)) +
        ggplot2::ggtitle(
          label = openair::quickText(paste0("Luftschadstoff-Emissionen ", longtitle(x)," (",x,")")),
          subtitle = as.expression(substitute(a * ", Jahresmengen-Anteile nach Quellgruppen (relativ)", env = list(a = x)))
        ) +
        ggplot2::labs(caption = "Quelle: OSTLUFT, Grundlage: EMIS Schweiz")
    )
    
  })



# plot RSD NOx emissions by vehicle type, fuel type and euronorm
data_rsd_per_norm <- readr::read_delim(ressources_plotting$emissions$rsd_norm, delim = ";")
plots$emissions$NOx$rsd_norm <-
  data_rsd_per_norm %>% 
  tidyr::expand(vehicle_type, vehicle_fuel_type, vehicle_euronorm) %>%
  dplyr::left_join(data_rsd_per_norm, by = c("vehicle_type", "vehicle_fuel_type", "vehicle_euronorm")) %>%
  dplyr::mutate(
    vehicle_type = dplyr::recode_factor(vehicle_type, !!!c("passenger car" = "Personenwagen", "light duty vehicle" = "leichte Nutzfahrzeuge")),
    vehicle_fuel_type = dplyr::recode_factor(vehicle_fuel_type, !!!c("gasoline" = "Benzin", "diesel" = "Diesel")),
    vehicle_euronorm = factor(vehicle_euronorm)
  ) %>% 
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

# plot RSD NOx emissions by vehicle model year, vehicle type and fuel type
data_rsd_per_yearmodel <- readr::read_delim(ressources_plotting$emissions$rsd_yearmodel, delim = ";")
plots$emissions$NOx$rsd_yearmodel <-
  data_rsd_per_yearmodel %>% 
  tidyr::expand(vehicle_type, vehicle_fuel_type, vehicle_model_year) %>% 
  dplyr::left_join(data_rsd_per_yearmodel, by = c("vehicle_type", "vehicle_fuel_type", "vehicle_model_year")) %>% 
  dplyr::mutate(
    vehicle_type = dplyr::recode_factor(vehicle_type, !!!c("passenger car" = "Personenwagen", "light duty vehicle" = "leichte Nutzfahrzeuge")),
    vehicle_fuel_type = dplyr::recode_factor(vehicle_fuel_type, !!!c("gasoline" = "Benzin", "diesel" = "Diesel"))
  ) %>% 
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

# plot RSD NOx emission time series (year of measurement) by fuel type
data_rsd_per_yearmeas <- readr::read_delim(ressources_plotting$emissions$rsd_yearmeas, delim = ";")
plots$emissions$NOx$rsd_yearmeas <-
  data_rsd_per_yearmeas %>% 
  dplyr::mutate(vehicle_fuel_type = dplyr::recode_factor(vehicle_fuel_type, !!!c("all" = "Benzin & Diesel", "gasoline" = "Benzin", "diesel" = "Diesel"))) %>% 
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





# plotting air pollutant monitoring data
# ------------------------------------------------------------

# read airquality monitring data
data_monitoring_aq <- 
  readr::read_delim(ressources_plotting$monitoring$airquality, delim = ";") %>% 
  dplyr::mutate(siteclass = factor(siteclass, levels = siteclass_levels)) %>% 
  dplyr::filter(year %in% years & parameter %in% parameters_timeseries & !(siteclass %in% c("ländlich - verkehrsbelastet", "klein-/vorstädtisch - verkehrsbelastet"))) 

# plot timeseries of yearly values for selected pollutants
plots$airquality$monitoring$timeseries <- plot_pars_timeseries(data_monitoring_aq, parameters_timeseries)

# read pre-compiled OSTLUFT y1 monitoring data for nitrogen deposition to sensitive ecosystems into separate dataset
data_monitoring_ndep <- readr::read_delim(ressources_plotting$monitoring$ndep, delim = ";")
data_monitoring_ndep <- 
  data_monitoring_ndep %>% 
  dplyr::mutate(
    parameter = factor(parameter, levels = rev(c("N-Deposition", "aus NH3-Quellen", "aus NOx-Quellen"))),
    ecosystem_category = factor(ecosystem_category, levels = rev(c("Hochmoor", "Flachmoor", "Trockenrasen", "Wald")))
  )

# plot timeseries of yearly nitrogen deposition at Bachtel site
temp <- dplyr::filter(immission_threshold_values, source == "LRV Grenzwert" & pollutant == "NO2")
cln <-
  data_monitoring_ndep %>%
  dplyr::filter(site == c("BA")) %>%
  dplyr::distinct(site_long, ecosystem_category, critical_load_min, critical_load_single, critical_load_max) %>%
  tidyr::gather(cln, value, -site_long, -ecosystem_category) %>%
  dplyr::filter(cln == "critical_load_single") # decided to show only single value, but still keep option for range display

plots$airquality$monitoring$Ndep$Bachtel_timeseries <-
  data_monitoring_ndep %>%
  dplyr::filter(site == "BA" & parameter != "N-Deposition") %>%
  dplyr::group_by(year, site, site_long, siteclass, ecosystem_category, critical_load_min, critical_load_single, critical_load_max, parameter, unit) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup() %>%
  ggplot2::ggplot(aes(x = year, y = value, fill = parameter)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::geom_text(data = dplyr::filter(data_monitoring_ndep, site == "BA" & parameter == "N-Deposition" & estimate == "geschätzt"), label = "*", color = "gray40") +
  ggplot2::geom_hline(data = cln, mapping = aes(yintercept = value, linetype = cln), color = temp$col, linewidth = temp$lsz, show.legend = FALSE) +
  ggplot2::scale_linetype_manual(values = c("critical_load_single" = 1, "critical_load_min" = 2, "critical_load_max" = 2)) +
  ggplot2::scale_x_continuous(limits = c(2000, max(years)), expand = c(0.01,0.01)) +
  ggplot2::scale_y_continuous(expand = c(0.01,0.01)) +
  ggplot2::scale_fill_manual(values = c("aus NH3-Quellen" = "gold2", "aus NOx-Quellen" = "steelblue4")) +
  lemon::facet_rep_wrap(ecosystem_category~., ncol = 1, scales = "free_y", repeat.tick.labels = TRUE) +
  ggplot2::ggtitle(
    label = openair::quickText("Luftqualitätsmesswerte - Stickstoffeintrag in empfindliche Ökosysteme am Standort Bachtel"),
    subtitle = expression("Stickstoffeintrag (kgN " * ha^-1 * Jahr^-1 * ")")
  ) +
  ggplot2::labs(caption = "*: mind. NH3 gemessen, restlicher Eintrag geschätzt; Quelle: OSTLUFT & FUB") +
  theme_ts +
  ggplot2::theme(
    strip.text = ggplot2::element_text(hjust = 0),
    legend.title = ggplot2::element_blank(),
    legend.position = "right"
  )

# plot distribution of yearly nitrogen deposition across several monitoring sites since 2019 (structured per year and ecosystem type)

plots$airquality$monitoring$Ndep$all <-
  data_monitoring_ndep %>%
  dplyr::filter(year >= 2019 & parameter != "N-Deposition") %>%
  dplyr::group_by(year, site, site_long, siteclass, ecosystem_category, critical_load_min, critical_load_single, critical_load_max, parameter, unit) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup() %>%
  ggplot2::ggplot(aes(x = year, y = value, fill = parameter)) +
  ggplot2::geom_bar(stat = "identity") + 
  # ggplot2::geom_hline(mapping = aes(yintercept = critical_load_min), linetype = 2, color = temp$col, linewidth = temp$lsz, show.legend = FALSE) +
  ggplot2::geom_hline(mapping = aes(yintercept = critical_load_single), color = temp$col, linewidth = temp$lsz, show.legend = FALSE) +
  # ggplot2::geom_hline(mapping = aes(yintercept = critical_load_max), linetype = 2, color = temp$col, linewidth = temp$lsz, show.legend = FALSE) +
  ggplot2::geom_text(data = dplyr::filter(data_monitoring_ndep, year >= 2019 & parameter == "N-Deposition" & estimate == "geschätzt"), label = "*", color = "gray40") +
  # ggplot2::scale_x_date(expand = c(0.01,0.01), date_labels = "%Y", date_breaks = "2 years") +
  ggplot2::scale_y_continuous(expand = c(0.01,0.01)) +
  ggplot2::scale_fill_manual(values = c("aus NOx-Quellen" = "steelblue4", "aus NH3-Quellen" = "gold2")) +
  ggh4x::facet_nested_wrap(.~ecosystem_category*site, nrow = 2, strip.position = "top", axes = "x", solo_line = TRUE) +
  ggplot2::ggtitle(
    label = openair::quickText("Luftqualitätsmesswerte - Stickstoffeintrag in empfindliche Ökosysteme"),
    subtitle = expression("Stickstoffeintrag (kgN " * ha^-1 * Jahr^-1 * ")")
  ) +
  ggplot2::labs(caption = "*: mind. NH3 gemessen, restlicher Eintrag geschätzt; Quelle: OSTLUFT") +
  theme_ts +
  ggplot2::theme(
    strip.text = ggplot2::element_text(size = ggplot2::rel(0.66)),
    ggh4x.facet.nestline = ggplot2::element_line(colour = "gray40"),
    panel.spacing = unit(2, "lines"),
    legend.position = "bottom",
    legend.title = ggplot2::element_blank()
  )

# plot timeseries of yearly nitrogen deposition across several monitoring sites since 2019 (structured per ecosystem type)

plots$airquality$monitoring$Ndep$all_timeseries <-
  data_monitoring_ndep %>%
  dplyr::filter(year >= 2019 & parameter == "N-Deposition") %>%
  ggplot2::ggplot(ggplot2::aes(x = year, y = value, color = ecosystem_category, shape = estimate)) +
  ggplot2::geom_jitter(size = pointsize * 2, width = 0.1) +
  ggplot2::scale_x_continuous(expand = c(0.01,0.01)) +
  ggplot2::scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01)) +
  ggplot2::scale_color_viridis_d(direction = -1) +
  # ggplot2::scale_color_manual(values = c("Hochmoor" , "Flachmoor", "Trockenrasen", "Wald")) +
  ggplot2::ggtitle(
    label = openair::quickText("Luftqualitätsmesswerte - Stickstoffeintrag in empfindliche Ökosysteme seit dem Jahr 2019"),
    subtitle = expression("Stickstoffeintrag (kgN " * ha^-1 * Jahr^-1 * ")")
  ) +
  ggplot2::labs(caption = "geschätzt: mind. NH3 gemessen, restlicher Eintrag geschätzt; Quelle: OSTLUFT") +
  theme_ts +
  ggplot2::theme(
    legend.title = ggplot2::element_blank()
  )

# plot timeseries of yearly nitrogen deposition vs. critical loads of nitrogen across several monitoring sites since 2019 (structured per ecosystem type)

plots$airquality$monitoring$Ndep$all_timeseries_vs_CLN <-
  data_monitoring_ndep %>%
  dplyr::filter(year >= 2019 & parameter == "N-Deposition") %>%
  ggplot2::ggplot(ggplot2::aes(x = year, y = value / critical_load_single, color = ecosystem_category, shape = estimate)) +
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), color = temp$col, linewidth = temp$lsz, show.legend = FALSE) +
  ggplot2::geom_jitter(size = pointsize * 2, width = 0.1) +
  ggplot2::scale_x_continuous(expand = c(0.01,0.01)) +
  ggplot2::scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01), labels = scales::percent_format()) +
  ggplot2::scale_color_viridis_d(direction = -1) +
  # ggplot2::scale_color_manual(values = c("Hochmoor" , "Flachmoor", "Trockenrasen", "Wald")) +
  ggplot2::ggtitle(
    label = openair::quickText("Luftqualitätsmesswerte - Stickstoffeintrag in empfindliche Ökosysteme seit dem Jahr 2019"),
    subtitle = expression("Stickstoffeintrag vs. kritische Eintragsrate (relativ)")
  ) +
  ggplot2::labs(caption = "geschätzt: mind. NH3 gemessen, restlicher Eintrag geschätzt; Quelle: OSTLUFT") +
  theme_ts +
  ggplot2::theme(
    legend.title = ggplot2::element_blank()
  )

# plot mean contribution of source categories to nitrogen deposition

plots$airquality$monitoring$Ndep$all_pie_sources <-
  data_monitoring_ndep %>%
  dplyr::filter(year >= 2019 & parameter != "N-Deposition") %>% 
  dplyr::group_by(parameter) %>% 
  dplyr::summarise(value = mean(value)) %>% 
  dplyr::ungroup() %>% 
  ggplot2::ggplot(ggplot2::aes(x = 1, y = value, fill = parameter)) +
  geom_bar(stat = "identity", position = "fill") +
  coord_polar(theta = "y") +
  ggplot2::scale_fill_manual(values = c("aus NOx-Quellen" = "steelblue4", "aus NH3-Quellen" = "gold2")) +
  ggplot2::ggtitle(
    label = openair::quickText("Luftqualitätsmesswerte - Stickstoffeintrag in empfindliche Ökosysteme seit dem Jahr 2019"),
    subtitle = expression("mittlerer Beitrag Quellgruppen zum Stickstoffeintrag")
  ) +
  ggplot2::labs(caption = "Quelle: OSTLUFT") +
  theme_map +
  ggplot2::theme(
    legend.title = ggplot2::element_blank()
  )

# plot relative comparison latest n_years of measurement data vs. LRV Immissionsgrenzwerte + Critical Loads of Nitrogen and WHO-Richtwerte

data_thrshlds <- dplyr::distinct(immission_threshold_values, source, col, lty, lsz)
data_temp <-
  data_monitoring_ndep %>%
  dplyr::filter(year %in% seq(max(years) - n_years + 1, max(years), 1)) %>%
  dplyr::filter(parameter == "N-Deposition") %>%
  dplyr::mutate(
    value = value / critical_load_single,
    parameter = factor(parameter),
    reference = factor("value_relative_lrv"),
    siteclass = "empf. Ökosystem"
  ) %>%
  dplyr::select(year, parameter, reference, value, siteclass)

plots$airquality$monitoring$threshold_comparison <-
  data_monitoring_aq %>%
  combine_thresholds(immission_threshold_values) %>%
  dplyr::mutate(
    value_relative_lrv = value / `LRV Grenzwert`,
    value_relative_who = value / `WHO Richtwert`
  ) %>%
  dplyr::filter(year %in% seq(max(years) - n_years + 1, max(years), 1)) %>%
  dplyr::select(year, parameter, value_relative_lrv, value_relative_who, siteclass) %>%
  tidyr::gather(reference, value, -year, -parameter, -siteclass) %>%
  dplyr::filter(!is.na(value) & !(siteclass %in% c("ländlich - verkehrsbelastet", "klein-/vorstädtisch - verkehrsbelastet"))) %>%
  dplyr::bind_rows(data_temp) %>%
  dplyr::mutate(
    parameter = dplyr::recode_factor(parameter, !!!c("NO2" = "NO2 Jahresmittel", "PM2.5" = "Feinstaub PM2.5 Jahresmittel", "PM10" = "Feinstaub PM10 Jahresmittel", 
                                                     "O3_max_98%_m1" = "O3 max. 98% der monatl. ½-Stundenwerte", "O3_peakseason_mean_d1_max_mean_h8gl" = "O3 mittlere Sommersaison-Tagesbelastung", 
                                                     "N-Deposition" = "Stickstoffeintrag in empfindliche Ökosysteme")),
    parameter = factor(parameter, levels = rev(c("Feinstaub PM2.5 Jahresmittel", "Feinstaub PM10 Jahresmittel", "NO2 Jahresmittel", 
                                                 "O3 max. 98% der monatl. ½-Stundenwerte", "O3 mittlere Sommersaison-Tagesbelastung", "Stickstoffeintrag in empfindliche Ökosysteme"))),
    reference = dplyr::recode(reference, !!!c("value_relative_lrv" = "relativ zu Immissionsgrenzwerten bzw. kritischen Eintragsraten:", 
                                              "value_relative_who" = "relativ zu Richtwerten der Weltgesundheitsorganisation:"))
  ) %>%
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





# plotting air pollutant population and ecosystem exposition
# ------------------------------------------------------------

# read exposition data
data_expo_distr_pollutants <- readr::read_delim(ressources_plotting$exposition$expo_distr_pollutants, delim = ";") 
data_expo_distr_ndep <- readr::read_delim(ressources_plotting$exposition$expo_distr_ndep, delim = ";") 
data_expo_weighmean_canton <- readr::read_delim(ressources_plotting$exposition$weightedmean_canton, delim = ";") 
data_expo_weighmean_municip <- readr::read_delim(ressources_plotting$exposition$weightedmean_municip, delim = ";") 
data_expo_weighmean_municip <- 
  map_municipalities %>% 
  dplyr::full_join(dplyr::select(data_expo_weighmean_municip, geodb_oid, year, parameter, pop_weighted_mean, source), by = "geodb_oid")



# plot inhabitant exposure distribution
# -----------------------------------------------
years_exposition <- setNames(unique(data_expo_distr_pollutants$year), as.character(unique(data_expo_distr_pollutants$year)))
parameters_exposition <- setNames(parameters_exposition, parameters_exposition)
years_exposition_ndep <- setNames(unique(data_expo_distr_ndep$year), as.character(unique(data_expo_distr_ndep$year)))



# ... histograms for air pollutants
plots$exposition$hist <-
  lapply(parameters_exposition, function(parameter) {
    lapply(years_exposition, function(year) {
      ggplot_expo_hist(
        data = dplyr::filter(data_expo_distr_pollutants, year == year & parameter == !!parameter), x = "concentration", y = "population", barwidth = expositionpars(parameter)$barwidth,
        xlims = range(expositionpars(parameter)$xbreaks), xbreaks = expositionpars(parameter)$xbreaks, threshold = extract_threshold(immission_threshold_values, parameter),
        xlabel = ggplot2::xlab(openair::quickText(paste0("Jahresmittel-Belastung ",parameter," (µg/m3)"))),
        titlelab = ggplot2::ggtitle(
          label = openair::quickText(paste0("Bevölkerungsexposition - ",longtitle(parameter)," (",parameter,")")),
          subtitle = openair::quickText(paste0("Anzahl Personen, Wohnbevölkerung im Kanton Zürich im Jahr ",year))
        ), 
        captionlab = ggplot2::labs(caption = "Datengrundlage: BAFU & BFS"),
        fill_scale = immissionscale(parameter), theme = theme_ts
      ) 
    })
  })

# ... histograms for sensitive ecosystems nitrogen deposition exceedance
plots$exposition$hist$Ndep <- 
  lapply(years_exposition_ndep, function(year) {
    ggplot_expo_hist(
      data = dplyr::filter(data_expo_distr_ndep, year == year & parameter == "max Ndep > CLO"), x = "EXNMAX", y = "n_ecosys", barwidth = expositionpars("Ndep")$barwidth,
      xlims = range(expositionpars("Ndep")$xbreaks), xbreaks = expositionpars("Ndep")$xbreaks, threshold = threshold_ndep,
      xlabel = ggplot2::xlab(expression("max. Stickstoff-Überschuss im Vergleich zu den kritischen Eintragsraten (kgN " * ha^-1 * Jahr^-1 * ")")),
      titlelab =   ggplot2::ggtitle(
        label = openair::quickText("Exposition empfindlicher Ökosysteme durch Stickstoffeinträge"),
        subtitle = paste0("Anzahl empfindlicher Ökosysteme (kumuliert) im Kanton Zürich im Jahr ", year) 
      ), 
      captionlab = ggplot2::labs(caption = "Quelle: BAFU"),
      fill_scale = immissionscale("Ndep"), theme = theme_ts
    )
  })

# ... cumulative distributions for air pollutants
plots$exposition$cumul <-
  lapply(parameters_exposition, function(parameter) {
    lapply(years_exposition, function(year) {
      ggplot_expo_cumulative(
        data = dplyr::filter(data_expo_distr_pollutants, year == year & parameter == !!parameter), x = "concentration", y = "population_cum_relative", linewidth = 1,
        xlims = range(expositionpars(parameter)$xbreaks), xbreaks = expositionpars(parameter)$xbreaks, threshold = extract_threshold(immission_threshold_values, parameter),
        xlabel = ggplot2::xlab(openair::quickText(paste0("Jahresmittel-Belastung ",parameter," (µg/m3)"))),
        titlelab = ggplot2::ggtitle(
          label = openair::quickText(paste0("Bevölkerungsexposition - ",longtitle(parameter)," (",parameter,")")),
          subtitle = openair::quickText(paste0("relativer Anteil (kumuliert), Wohnbevölkerung im Kanton Zürich im Jahr ",year))
        ), 
        captionlab = ggplot2::labs(caption = "Datengrundlage: BAFU & BFS"),
        theme = theme_ts
      ) 
    })
  })

# ... cumulative distributions for sensitive ecosystems nitrogen deposition exceedance
plots$exposition$cum$Ndep <- 
  lapply(years_exposition_ndep, function(year) {
    ggplot_expo_cumulative(
      data = dplyr::filter(data_expo_distr_ndep, year == year & parameter == "max Ndep > CLO"), x = "EXNMAX", y = "n_ecosys_cum_relative", linewidth = 1,
      xlims = range(expositionpars("Ndep")$xbreaks), xbreaks = expositionpars("Ndep")$xbreaks, threshold = threshold_ndep,
      xlabel = ggplot2::xlab(expression("max. Stickstoff-Überschuss im Vergleich zu den kritischen Eintragsraten (kgN " * ha^-1 * Jahr^-1 * ")")),
      titlelab =   ggplot2::ggtitle(
        label = openair::quickText("Exposition empfindlicher Ökosysteme durch Stickstoffeinträge"),
        subtitle = paste0("Anzahl empfindlicher Ökosysteme (kumuliert) im Kanton Zürich im Jahr ", year) 
      ), 
      captionlab = ggplot2::labs(caption = "Quelle: BAFU"),
      theme = theme_ts
    )
  })





# -----------------------------------------------
# plot population-weighted mean values (single value for Kanton Zürich & per municipality)
# -----------------------------------------------

plots$exposition$population_weighted_mean <-
  lapply(parameters_exposition, function(parameter) {
    
    lapply(years_exposition, function(year) {
      
      dplyr::filter(data_expo_weighmean_municip, year == year & parameter == !!parameter) %>% 
        ggplot2::ggplot(ggplot2::aes(fill = pop_weighted_mean)) +
        ggplot2::geom_sf() + 
        ggplot2::coord_sf(datum = sf::st_crs(crs)) +
        immissionscale(parameter) +
        theme_map +
        ggplot2::ggtitle(
          label = openair::quickText(paste0("Bevölkerungsgewichtete Schadstoffbelastung - ",longtitle(parameter)," (",parameter,")")),
          subtitle = openair::quickText(paste0("Mittlere ",parameter,"-Belastung pro Einwohner/in nach Gemeinde im Jahr ",year,"; Kanton = ",dplyr::pull(dplyr::filter(data_expo_weighmean_canton, year == year & parameter == !!parameter), "pop_weighted_mean")," µg/m3"))
        ) +
        ggplot2::labs(caption = "Datengrundlage: BAFU & BFS")
      
    })
  })





# clean up
# ------------------------------------------------------------

rm(list = c("basesize", "col_lrv", "col_who", "lty_lrv", "lty_who", "lsz_lrv", "lsz_who", "lbsz", "url", "request"))
rm(list = c("req", "rsd", "rsd_filters", "data_temp", "data_temp2", "data_rsd", "data_rsd_per_norm", "data_rsd_per_yearmodel", "data_rsd_per_yearmeas", "rsd_meta", "emikat", "data_emikat", "cols_emissions"))
rm(list = c("parameters", "pointsize", "n_years", "temp", "cln", "data_temp", "data_monitoring_ndep", "data_thrshlds", "scale_color_siteclass", "scale_fill_siteclass", "site_meta", "data", "files"))
rm(list = c("data_expo", "data_raster", "threshold", "boundaries", "boundaries_hull", "threshold_values", "crs", "years", "theme_map", "theme_ts", "immission_colorscale_no2", "immission_colorscale_pm10",
            "immission_colorscale_pm2_5", "immission_colorscale_ebc", "immission_colorscale_nh3", "immission_colorscale_ndep", "immission_colorscale_ndep_exc"))



