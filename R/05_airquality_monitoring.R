### -----------------------------------------------
### -----------------------------------------------
### pollutant monitoring data 
### (time series and threshold comparisons)
### -----------------------------------------------
### -----------------------------------------------


### -----------------------------------------------
### air quality monitoring site metadata
### -----------------------------------------------

### read and restructure NABEL site metadata
site_meta <-
  fs::path("data/input", files$airquality$monitoring$nabel_y1) %>%
  get_nabel_meta_arias() %>%
  dplyr::mutate(
    site_long = site,
    zone = ifelse(zone == "vorstädtisch", "klein-/vorstädtisch", zone),
    source = "NABEL (BAFU & Empa)"
  )

### read and restructure OSTLUFT site metadata
site_meta <-
  fs::path("data/input", files$airquality$monitoring$ostluft_meta)  %>%
  read_ostluft_meta() %>%
  dplyr::mutate(
    zone = aggregate_ostluft_meta_zone(zone),
    type = aggregate_ostluft_meta_type(type),
    source = "OSTLUFT"
  ) %>%
  dplyr::bind_rows(site_meta) %>%
  dplyr::mutate(siteclass = factor(paste(zone, type, sep = " - "),
                                   levels = rev(c("ländlich - Hintergrund", "ländlich - verkehrsbelastet", "klein-/vorstädtisch - Hintergrund",
                                                  "klein-/vorstädtisch - verkehrsbelastet", "städtisch - Hintergrund", "städtisch - verkehrsbelastet")))) %>%
  dplyr::select(-zone, -type)



### -----------------------------------------------
### read, restructure and combine monitoring data
### prepare data for plotting and write dataset
### -----------------------------------------------

### empty dataset to be consecutively filled with monitoring data
data <- tibble::tibble()

### read and restructure NABEL y1 data
data <-
  fs::path("data/input", files$airquality$monitoring$nabel_y1) %>%
  read_arias() %>%
  dplyr::mutate(source = factor("NABEL (BAFU & Empa)")) %>%
  dplyr::bind_rows(data)

### read and restructure OSTLUFT y1 data
data <-
  fs::path("data/input", files$airquality$monitoring$ostluft_y1) %>%
  read_airmo_csv2() %>%
  remove_duplicate_y1() %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(
    parameter = dplyr::recode_factor(parameter, !!!c("NO2_PS" = "NO2", "PM10h" = "PM10", "PM2.5h" = "PM2.5")),
    source = factor("OSTLUFT")
  ) %>%
  dplyr::bind_rows(data)

### read pre-compiled OSTLUFT & NABEL O3 peak-season y1 data
data <-
  fs::path("data/input", files$airquality$monitoring$ostluft_nabel_peakseason_y1) %>% 
  readr::read_delim(delim = ";", locale = readr::locale(tz = "Etc/GMT-1")) %>%
  dplyr::bind_rows(data)

### read OSTLUFT y1 data for nitrogen deposition to sensitive ecosystems
data_ndep <-
  fs::path("data/input", files$airquality$monitoring$ostluft_ndep_y1) %>%
  readr::read_delim(delim = ";")

### finalise dataset and join with site metadata
data <-
  data %>%
  # dplyr::bind_rows(calc_lbi(data, threshold_values)) %>% # add Langzeitbelastungsindex LBI ... currently, LBI needs to be revised by Cercl'Air => prepared for later inclusion
  dplyr::filter(lubridate::year(starttime) %in% years & parameter %in% c(parameters, "LBI")) %>% # filter for target years and parameter
  rOstluft::pad() %>%  # pad to complete timeseries for better plotting
  dplyr::select(-source) %>%
  dplyr::left_join(site_meta, by = "site") %>%
  dplyr::filter(!is.na(siteclass))

### write dataset
data %>%
  dplyr::arrange(site, parameter, starttime) %>%
  dplyr::mutate(starttime = format(starttime, "%Y-%m-%d %H:%M:%S")) %>%
  readr::write_delim(file = "data/output/data_airquality_monitoring_y1.csv", delim = ";", na = "NA")




### -----------------------------------------------
### plot data
### -----------------------------------------------

### plot timeseries yearly mean values NO2
plots$airquality$monitoring$NO2$y1_timeseries <-
  data %>%
  dplyr::filter(parameter == "NO2" & !(siteclass %in% c("ländlich - verkehrsbelastet", "klein-/vorstädtisch - verkehrsbelastet"))) %>%
  ggplot_timeseries(
    lims = c(0,65),
    titlelab = ggplot2::ggtitle(
      label = openair::quickText("Luftqualitätsmesswerte - Stickstoffdioxid (NO2)"),
      subtitle = openair::quickText("NO2, Jahresmittel (µg/m3)")
    ),
    captionlab = ggplot2::labs(caption = "Datenabdeckung: Kanton Zürich, Quelle: OSTLUFT & NABEL (BAFU & Empa)"),
    pointsize = pointsize, theme = theme_ts, threshold = get_threshold(threshold_values, "NO2")
  ) +
  scale_color_siteclass

### plot timeseries yearly mean values particulate matter PM10
plots$airquality$monitoring$PM10$y1_timeseries <-
  data %>%
  dplyr::filter(parameter == "PM10" & !(siteclass %in% c("ländlich - verkehrsbelastet", "klein-/vorstädtisch - verkehrsbelastet"))) %>%
  ggplot_timeseries(
    lims = c(0,35),
    titlelab = ggplot2::ggtitle(
      label = openair::quickText("Luftqualitätsmesswerte - Feinstaub (PM10)"),
      subtitle = openair::quickText("PM10, Jahresmittel (µg/m3)")
    ),
    captionlab = ggplot2::labs(caption = "Datenabdeckung: Kanton Zürich, Quelle: OSTLUFT & NABEL (BAFU & Empa)"),
    pointsize = pointsize, theme = theme_ts, threshold = get_threshold(threshold_values, "PM10")
  ) +
  scale_color_siteclass

### plot timeseries yearly mean values particulate matter PM2.5
plots$airquality$monitoring$`PM2.5`$y1_timeseries <-
  data %>%
  dplyr::filter(parameter == "PM2.5" & !(siteclass %in% c("ländlich - verkehrsbelastet", "klein-/vorstädtisch - verkehrsbelastet"))) %>%
  ggplot_timeseries(
    lims = c(0,20),
    titlelab = ggplot2::ggtitle(
      label = openair::quickText("Luftqualitätsmesswerte - Feinstaub (PM2.5)"),
      subtitle = openair::quickText("PM2.5, Jahresmittel (µg/m3)")
    ),
    captionlab = ggplot2::labs(caption = "Datenabdeckung: Kanton Zürich, Quelle: OSTLUFT & NABEL (BAFU & Empa)"),
    pointsize = pointsize, theme = theme_ts, threshold = get_threshold(threshold_values, "PM2.5")
  ) +
  scale_color_siteclass

### plot timeseries yearly values of maximum O3 monthly 98%-percentile of 1/2 hour mean values
plots$airquality$monitoring$O3$`max_98%_m1_timeseries` <-
  data %>%
  dplyr::filter(parameter == "O3_max_98%_m1" & !(siteclass %in% c("ländlich - verkehrsbelastet", "klein-/vorstädtisch - verkehrsbelastet"))) %>%
  ggplot_timeseries(
    lims = c(0,210),
    titlelab = ggplot2::ggtitle(
      label = openair::quickText("Luftqualitätsmesswerte - Ozon (O3)"),
      subtitle = openair::quickText("O3, höchstes 98%-Perzentil der Halbstundenmittel eines Monats (µg/m3)")
    ),
    captionlab = ggplot2::labs(caption = "Datenabdeckung: Kanton Zürich, Quelle: OSTLUFT & NABEL (BAFU & Empa)"),
    pointsize = pointsize, theme = theme_ts,
    threshold = get_threshold(threshold_values, pollutant = "O3", metric = "monthly 98%-percentile of ½ hour mean values ≤ 100 µg/m3", source = "LRV Grenzwert", aggregation = "m1")
  ) +
  scale_color_siteclass

### plot timeseries yearly values O3 mean peak-season
plots$airquality$monitoring$O3$`peak-season_timeseries` <-
  data %>%
  dplyr::filter(parameter == "O3_peakseason_mean_d1_max_mean_h8gl" & !(siteclass %in% c("ländlich - verkehrsbelastet", "klein-/vorstädtisch - verkehrsbelastet"))) %>%
  ggplot_timeseries(
    lims = c(0,130),
    titlelab = ggplot2::ggtitle(
      label = openair::quickText("Luftqualitätsmesswerte - Ozon (O3)"),
      subtitle = openair::quickText("O3, mittlere tägliche max. 8-Stundenmittel während der Sommersaison (µg/m3)")
    ),
    captionlab = ggplot2::labs(caption = "Datenabdeckung: Kanton Zürich, Quelle: OSTLUFT & NABEL (BAFU & Empa)"),
    pointsize = pointsize, theme = theme_ts,
    threshold = get_threshold(threshold_values, pollutant = "O3", metric = "mean of daily maximum 8-hour mean concentration in the six consecutive months with the highest six-month running-mean concentration", source = "WHO Richtwert", aggregation = "peak-season")
  ) +
  scale_color_siteclass

# ### plot timeseries Langzeitbelastungsindex LBI, see https://cerclair.ch/assets/pdf/27b_2015_06_10_D_Langzeit_Luftbelastungs_Index.pdf
# sites <-
#   data %>%
#   dplyr::filter(parameter == "LBI") %>%
#   dplyr::group_by(site) %>%
#   dplyr::summarise(n = sum(!is.na(value))) %>%
#   dplyr::ungroup() %>%
#   dplyr::filter(n > 3) %>%
#   dplyr::pull(site) %>%
#   as.character()
#
# plots$airquality$monitoring$LBI$timeseries <-
#   data %>%
#   dplyr::filter(parameter == "LBI" & !is.na(value) & site %in% sites) %>%
#   dplyr::arrange(site, starttime) %>%
#   ggplot2::ggplot(aes(x = lubridate::year(starttime) + 0.5, y = site, fill = recode_lbi(value))) +
#   ggplot2::geom_raster() +
#   ggplot2::scale_x_continuous(expand = c(0,0)) +
#   ggplot2::scale_fill_manual(name = "LBI", values = c("gering" = "#00FFFF", "mässig" = "#80FF00", "deutlich" = "#FFFF00",
#                                                       "erheblich" = "#FFBF00", "hoch"= "#FF8000", "sehr hoch" = "#BF00FF")) +
#   ggplot2::ggtitle(
#     label = openair::quickText("Luftqualitätsmesswerte - Langzeitbelastungsindex (LBI)"),
#     subtitle = openair::quickText("Definition siehe www.cerclair.ch/empfehlungen")
#   ) +
#   ggplot2::labs(caption = "Datenabdeckung: Kanton Zürich, Quelle: OSTLUFT & NABEL (BAFU & Empa)") +
#   theme_ts

### plot timeseries of yearly nitrogen deposition at Bachtel site
temp <- dplyr::filter(threshold_values, source == "LRV Grenzwert" & pollutant == "NO2")
cln <-
  data_ndep %>%
  dplyr::filter(site_short %in% c("BA", "BA_Wald")) %>%
  dplyr::distinct(site, ecosystem_category, critical_load_min, critical_load_single, critical_load_max) %>%
  tidyr::gather(cln, value, -site, -ecosystem_category) %>%
  # dplyr::mutate(ecosystem_category = paste0("empfindliches Ökosystem: ", ecosystem_category)) %>%
  dplyr::filter(cln == "critical_load_single") # decided to show only single value, but still keep option for range display
data_temp <-
  data_ndep %>%
  dplyr::mutate(
    parameter = dplyr::case_when(
      stringr::str_detect(parameter, "NO3") | stringr::str_detect(parameter, "NO2") ~ "aus NOx-Quellen",
      stringr::str_detect(parameter, "NH3") | stringr::str_detect(parameter, "NH4") ~ "aus NH3-Quellen",
      TRUE ~ parameter
    ),
    parameter =  factor(parameter, levels = c("aus NOx-Quellen", "aus NH3-Quellen", "N-Deposition")),
    # ecosystem_category = paste0("empfindliches Ökosystem: ", ecosystem_category),
    site_short = stringr::str_remove(site_short, "_Wald"),
    site = stringr::str_remove(site, "_Wald"),
    site = stringr::str_replace(site, "_", "-")
  )

plots$airquality$monitoring$Ndep$Bachtel_timeseries <-
  data_temp %>%
  dplyr::filter(site_short == "BA" & parameter != "N-Deposition") %>%
  dplyr::group_by(year, site_short, site, siteclass, ecosystem_category, critical_load_min, critical_load_single, critical_load_max, parameter, unit) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup() %>%
  ggplot2::ggplot(aes(x = year, y = value, fill = parameter)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::geom_text(data = dplyr::filter(data_temp, site_short == "BA" & parameter == "N-Deposition" & estimate == "geschätzt"), label = "*", color = "gray40") +
  ggplot2::geom_hline(data = cln, mapping = aes(yintercept = value, linetype = cln), color = temp$col, linewidth = temp$lsz, show.legend = FALSE) +
  ggplot2::scale_linetype_manual(values = c("critical_load_single" = 1, "critical_load_min" = 2, "critical_load_max" = 2)) +
  ggplot2::scale_x_continuous(limits = range(years), expand = c(0.01,0.01)) +
  ggplot2::scale_y_continuous(expand = c(0.01,0.01)) +
  ggplot2::scale_fill_manual(values = c("aus NOx-Quellen" = "steelblue4", "aus NH3-Quellen" = "gold2")) +
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

### plot distribution of yearly nitrogen deposition across several monitoring sites since 2019 (structured per year and ecosystem type)
plots$airquality$monitoring$Ndep$all <-
  data_temp %>%
  dplyr::filter(year >= 2019 & parameter != "N-Deposition") %>%
  dplyr::group_by(year, site_short, site, siteclass, ecosystem_category, critical_load_min, critical_load_single, critical_load_max, parameter, unit) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup() %>%
  ggplot2::ggplot(aes(x = lubridate::as_date(paste0(year,"-01-01")), y = value, fill = parameter)) +
  ggplot2::geom_bar(stat = "identity") +
  # ggplot2::geom_hline(mapping = aes(yintercept = critical_load_min), linetype = 2, color = temp$col, linewidth = temp$lsz, show.legend = FALSE) +
  ggplot2::geom_hline(mapping = aes(yintercept = critical_load_single), color = temp$col, linewidth = temp$lsz, show.legend = FALSE) +
  # ggplot2::geom_hline(mapping = aes(yintercept = critical_load_max), linetype = 2, color = temp$col, linewidth = temp$lsz, show.legend = FALSE) +
  ggplot2::geom_text(data = dplyr::filter(data_temp, year >= 2019 & parameter == "N-Deposition" & estimate == "geschätzt"), label = "*", color = "gray40") +
  ggplot2::scale_x_date(expand = c(0.01,0.01), date_labels = "%Y", date_breaks = "2 years") +
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

### plot relative comparison latest n_years of measurement data vs. LRV Immissionsgrenzwerte + Critical Loads of Nitrogen and WHO-Richtwerte
data_thrshlds <- dplyr::distinct(threshold_values, source, col, lty, lsz)
data_temp <-
  data_ndep %>%
  dplyr::filter(year %in% seq(max(years) - n_years + 1, max(years), 1)) %>%
  dplyr::filter(parameter == "N-Deposition") %>%
  dplyr::mutate(
    value = value / critical_load_single,
    parameter = factor(parameter),
    reference = factor("value_relative_lrv")
  ) %>%
  dplyr::select(year, parameter, reference, value)

plots$airquality$monitoring$threshold_comparison <-
  data %>%
  combine_thresholds(threshold_values) %>%
  dplyr::mutate(
    year = lubridate::year(starttime),
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
    label = openair::quickText("Luftqualitätsmesswerte - Schwellenwertvergleich"),
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


### clean up
# rm(list = c("parameters", "pointsize", "n_years", "temp", "cln", "data_temp", "data_ndep", "data_thrshlds", "scale_color_siteclass", "scale_fill_siteclass", "site_meta", "data", "files"))

