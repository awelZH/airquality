# preparing plotting
# ------------------------------------------------------------

# add plotting parameter to LRV threshold limit values & WHO air quality guideline values

col_lrv <- "red3" # color of LRV threshold value
col_who <- "gray30" # color of WHO guideline threshold value
lty_lrv <- 1 # line type of LRV threshold value
lty_who <- 2 # line type WHO guideline threshold value
lsz_lrv <- 1 # line width of LRV threshold value
lsz_who <- 1 # line width of WHO guideline threshold value
lbsz <- 4 # label size of threshold value line text

threshold_values <-
  tibble::tibble(
    source = c("LRV Grenzwert", "WHO Richtwert"),
    col = c(col_lrv, col_who),
    lty = c(lty_lrv, lty_who),
    lsz = c(lsz_lrv, lsz_who),
    lbsz = lbsz
  ) %>% 
  dplyr::right_join(threshold_values, by = "source")

# subsetting parameters

years <- 2000:(lubridate::year(Sys.Date()) - 2) # years to consider for plotting 
n_years <- 3 # consider last 3 years for plotting relative threshold comparison    
parameters <- c("NO2", "NO2_PS", "PM10", "PM2.5", "O3_max_98%_m1", "O3_peakseason_mean_d1_max_mean_h8gl") # parameters to include for timeseries plotting

# plotting size parameters

basesize <- 12 # ggplot theme base_size
pointsize <- 2 # size of point markers
linewidth <- 1 # width of lines

# colors and color scales

scale_fill_siteclass <- 
  ggplot2::scale_fill_manual(name = "Standortklasse", values = c(
    "ländlich - Hintergrund" = viridis_pal(option = "D", begin = 0.2, end = 0.97)(4)[4],
    "klein-/vorstädtisch - Hintergrund" = viridis_pal(option = "D", begin = 0.2, end = 0.97)(4)[3],
    "städtisch - Hintergrund" = viridis_pal(option = "D", begin = 0.2, end = 0.97)(4)[2],
    "städtisch - verkehrsbelastet" = viridis_pal(option = "D", begin = 0.2, end = 0.97)(4)[1]
  ))

scale_color_siteclass <- 
  ggplot2::scale_color_manual(name = "Standortklasse", values = c(
    "ländlich - Hintergrund" = viridis_pal(option = "D", begin = 0.2, end = 0.97)(4)[4],
    "klein-/vorstädtisch - Hintergrund" = viridis_pal(option = "D", begin = 0.2, end = 0.97)(4)[3],
    "städtisch - Hintergrund" = viridis_pal(option = "D", begin = 0.2, end = 0.97)(4)[2],
    "städtisch - verkehrsbelastet" = viridis_pal(option = "D", begin = 0.2, end = 0.97)(4)[1]
  ))

cols_emissions <- c(wesanderson::wes_palette(name = "BottleRocket2", n = 5, type = "discrete"), "#003333")
immission_colorscale <- function(...) {
  cols <- c("#004DA8", "#005ce6", "#0070ff", "#00c5ff", "#47d9fa", "#56f9fb", "#2e9c6b", "#38bd00", "#56d900", 
            "#51f551", "#ffff00", "#ffd400", "#ffa300", "#ff5200", "#ff0000", "#ff0094", "#de00a1", "#c500ba")
  return(rOstluft.plot::scale_fill_gradientn_squished(..., colors = cols, na.value = NA))
}
immission_colorscale_no2 <- immission_colorscale(limits = c(0,50), breaks = seq(0,50,10), name = "NO2\n(µg/m3)")
immission_colorscale_pm10 <- immission_colorscale(limits = c(0,34), breaks = c(seq(0,30,10), 34), name = "PM10\n(µg/m3)")
immission_colorscale_pm2_5 <- immission_colorscale(limits = c(0,17), breaks = c(seq(0,15,2.5), 17), name = "PM2.5\n(µg/m3)")
immission_colorscale_ebc <- immission_colorscale(limits = c(0,1.5), breaks = seq(0,1.5,0.3), name = "eBC\n(µg/m3)")
immission_colorscale_nh3 <- rOstluft.plot::scale_fill_viridis_squished(name = "NH3\n(µg/m3)", limits = c(1, 7), breaks = seq(1, 7, 2), direction = -1,  option = "A", na.value = NA)
immission_colorscale_ndep <- rOstluft.plot::scale_fill_viridis_squished(name = "Ndep\n(kgN/ha/Jahr)", limits = c(15, 30), breaks = seq(15, 30, 5), direction = -1, option = "A", na.value = NA)
immission_colorscale_ndep_exc <- rOstluft.plot::scale_fill_viridis_squished(name = "Ndep > CLN\n(kgN/ha/Jahr)", limits = c(0, 30), breaks = seq(0, 30, 5), direction = -1, option = "A", na.value = NA)

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



# FIXME: in scripts: use output-datasets directly from github


# plotting air pollutant emissions
# ------------------------------------------------------------

# plot details of Canton Zürich air pollutant emissions per pollutant, subsector and year (absolute values)
# and plot maps of air pollutant emissions per pollutant, municipality and year

data_temp2 <- 
  data_emikat %>% 
  dplyr::group_by(municipality, year, pollutant, unit) %>% 
  dplyr::summarise(emission = sum(emission)) %>% 
  dplyr::ungroup()

data_temp2 <-
  boundaries %>% 
  dplyr::filter(art_code == 1) %>% 
  dplyr::rename(municipality = gemeindename) %>% 
  dplyr::right_join(data_temp2, by = "municipality") %>% 
  dplyr::select(year, pollutant, emission)

cols_emissions <- setNames(as.character(cols_emissions), unique(data_temp$sector))

plots$emissions <- 
  setNames(unique(data_temp$pollutant), unique(data_temp$pollutant)) %>% 
  lapply(function(x) {
    
    list(
      
      absolute =
        data_temp %>% 
        dplyr::filter(pollutant == x) %>% 
        ggplot_emissions(cols_emissions, theme_emissions = theme_ts) + 
        ggplot2::ggtitle(
          label = openair::quickText(paste0("Luftschadstoff-Emissionen ", longtitle(x)," (",x,")")),
          subtitle = as.expression(substitute(a * ", Jahresmenge nach Quellgruppen (t " * Jahr^-1 * ")", env = list(a = x)))
        ) +
        ggplot2::labs(caption = "Quelle: OSTLUFT, Grundlage: EMIS Schweiz"), 
      
      relative = 
        data_temp %>% 
        dplyr::filter(pollutant == x) %>% 
        ggplot_emissions(cols_emissions, pos = "fill", width = 0.75, theme_emissions = theme_ts) + 
        ggplot2::scale_y_continuous(labels = scales::percent_format(), expand = c(0.01,0.01)) +
        ggplot2::ggtitle(
          label = openair::quickText(paste0("Luftschadstoff-Emissionen ", longtitle(x)," (",x,")")),
          subtitle = as.expression(substitute(a * ", Jahresmengen-Anteile nach Quellgruppen (relativ)", env = list(a = x)))
        ) +
        ggplot2::labs(caption = "Quelle: OSTLUFT, Grundlage: EMIS Schweiz"), 
      
      map = 
        data_temp2 %>% 
        dplyr::filter(pollutant == x) %>% 
        ggplot2::ggplot(aes(fill = emission)) +
        ggplot2::geom_sf() + 
        ggplot2::coord_sf(datum = sf::st_crs(crs)) +
        ggplot2::facet_wrap(year~.) +
        ggplot2::scale_fill_viridis_c(name = paste0(x,"\nEmission"), option = "A", direction = -1, na.value = NA) +
        theme_map +
        ggplot2::ggtitle(
          label = openair::quickText(paste0("Luftschadstoff-Emissionen ", longtitle(x)," (",x,")")),
          subtitle = as.expression(substitute(a * ", Jahresmenge pro Gemeinde (t " * Jahr^-1 * ")", env = list(a = x)))
        ) +
        ggplot2::labs(caption = "Quelle: OSTLUFT, Grundlage: EMIS Schweiz")
      
      # ... possibly later: maps with emission per inhabitant per municipality? 
    )
    
  })

# plot RSD NOx emissions by vehicle type, fuel type and euronorm

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

plots$emissions$NOx$rsd_timeseries <-
  data_rsd_per_yearmeas %>% 
  dplyr::mutate(vehicle_fuel_type = dplyr::recode_factor(vehicle_fuel_type, !!!c("all" = "Benzin & Diesel", "gasoline" = "Benzin", "diesel" = "Diesel"))) %>% 
  ggplot2::ggplot(aes(x = year, y = NOx_emission)) +
  ggplot2::geom_smooth(mapping = aes(color = vehicle_fuel_type), span = 0.6, level = 0.95) +
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

# read LRV legal threshold limit values & WHO air quality guideline values

immission_threshold_values <- readr::read_delim(paste(path_data_input, files$airquality$thresh, sep = "/"), delim = ";",locale = readr::locale(encoding = "UTF-8"))

# filter for target years and parameters

data_monitoring_aq <- 
  data_monitoring_aq %>% 
  dplyr::filter(lubridate::year(starttime) %in% years & parameter %in% c(parameters, "LBI")) 

# read pre-compiled OSTLUFT y1 monitoring data for nitrogen deposition to sensitive ecosystems into separate dataset

data_ndep <- readr::read_delim(paste(path_data_input, files$airquality$monitoring$ostluft_ndep_y1, sep = "/"), delim = ";")

# plot timeseries yearly mean values NO2

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

# plot timeseries yearly mean values particulate matter PM10

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

# plot timeseries yearly mean values particulate matter PM2.5

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

# plot timeseries yearly values of maximum O3 monthly 98%-percentile of 1/2 hour mean values

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

# plot timeseries yearly values O3 mean peak-season

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

# # plot timeseries Langzeitbelastungsindex LBI, see https://cerclair.ch/assets/pdf/27b_2015_06_10_D_Langzeit_Luftbelastungs_Index.pdf

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

# plot timeseries of yearly nitrogen deposition at Bachtel site

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

# plot distribution of yearly nitrogen deposition across several monitoring sites since 2019 (structured per year and ecosystem type)

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

# plot relative comparison latest n_years of measurement data vs. LRV Immissionsgrenzwerte + Critical Loads of Nitrogen and WHO-Richtwerte

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




# plotting air pollutant maps
# ------------------------------------------------------------


# maps of mean air pollutant concentrations

# ... for NO2

plots$airquality$maps$NO2 <-
  setNames(names(data_raster$NO2), names(data_raster$NO2)) %>% 
  lapply(function(year) {
    ggplot2::ggplot() +
      stars::geom_stars(data = dplyr::select(data_raster$NO2[[year]], NO2)) +
      ggplot2::geom_sf(data = boundaries_hull, fill = NA) +
      ggplot2::coord_sf(datum = sf::st_crs(crs)) +
      immission_colorscale_no2 +
      ggplot2::ggtitle(
        label = openair::quickText("Belastungskarte Stickstoffdioxid (NO2)"),
        subtitle = openair::quickText(paste0("NO2, Jahresmittel-Konzentration ", year))
      ) +
      ggplot2::labs(caption = "Quelle: BAFU") +
      theme_map
  })

# ... for PM10

plots$airquality$maps$PM10 <-
  setNames(names(data_raster$PM10), names(data_raster$PM10)) %>% 
  lapply(function(year) {
    ggplot2::ggplot() +
      stars::geom_stars(data = dplyr::select(data_raster$PM10[[year]], PM10)) +
      ggplot2::geom_sf(data = boundaries_hull, fill = NA) +
      ggplot2::coord_sf(datum = sf::st_crs(crs)) +
      immission_colorscale_pm10 +
      ggplot2::ggtitle(
        label = openair::quickText("Belastungskarte Feinstaub (PM10)"),
        subtitle = openair::quickText(paste0("PM10, Jahresmittel-Konzentration ", year))
      ) +
      ggplot2::labs(caption = "Quelle: BAFU") +
      theme_map
  })

# ... for PM2.5

plots$airquality$maps$PM2.5 <-
  setNames(names(data_raster$PM2.5), names(data_raster$PM2.5)) %>% 
  lapply(function(year) {
    ggplot2::ggplot() +
      stars::geom_stars(data = dplyr::select(data_raster$PM2.5[[year]], PM2.5)) +
      ggplot2::geom_sf(data = boundaries_hull, fill = NA) +
      ggplot2::coord_sf(datum = sf::st_crs(crs)) +
      immission_colorscale_pm2_5 +
      ggplot2::ggtitle(
        label = openair::quickText("Belastungskarte Feinstaub (PM2.5)"),
        subtitle = openair::quickText(paste0("PM2.5, Jahresmittel-Konzentration ", year))
      ) +
      ggplot2::labs(caption = "Quelle: BAFU") +
      theme_map
  })

# ... for eBC

plots$airquality$maps$eBC <-
  setNames(names(data_raster$eBC), names(data_raster$eBC)) %>% 
  lapply(function(year) {
    ggplot2::ggplot() +
      stars::geom_stars(data = dplyr::select(data_raster$eBC[[year]], eBC)) +
      ggplot2::geom_sf(data = boundaries_hull, fill = NA) +
      ggplot2::coord_sf(datum = sf::st_crs(crs)) +
      immission_colorscale_ebc +
      ggplot2::ggtitle(
        label = openair::quickText("Belastungskarte Russ im Feinstaub (eBC)"),
        subtitle = openair::quickText(paste0("eBC, Jahresmittel-Konzentration ", year))
      ) +
      ggplot2::labs(caption = "Quelle: BAFU") +
      theme_map
  })

# ... for O3
# ...

# map of mean NH3 concentration in 2020

plots$airquality$maps$NH3$`2020` <-
  ggplot2::ggplot() +
  geom_sf(data = data_raster$NH3$`2020`, mapping = aes(fill = CNH3), color = NA) +
  ggplot2::geom_sf(data = boundaries_hull, fill = NA) +
  ggplot2::coord_sf(datum = sf::st_crs(crs)) +
  immission_colorscale_nh3 +
  ggplot2::ggtitle(
    label = openair::quickText("Belastungskarte Ammoniak (NH3)"),
    subtitle = openair::quickText("NH3, Jahresmittel 2020")
  ) +
  ggplot2::labs(caption = "Quelle: BAFU") +
  theme_map

# map of total nitrogen deposition in 2020

plots$airquality$maps$nitrogen_deposition$`2020` <-
  ggplot2::ggplot() +
  geom_sf(data = data_raster$Ndep$`2020`, mapping = aes(fill = DNTOT), color = NA) +
  ggplot2::geom_sf(data = boundaries_hull, fill = NA) +
  ggplot2::coord_sf(datum = sf::st_crs(crs)) +
  immission_colorscale_ndep +
  ggplot2::ggtitle(label = "Belastungskarte Stickstoffdeposition (Ndep)",
                   subtitle = "Ndep, Jahressumme 2020") +
  ggplot2::labs(caption = "Quelle: BAFU") +
  theme_map

# map of total nitrogen deposition CLN exceedance in 2020

plots$airquality$maps$CLN_exceedance$`2020` <-
  ggplot2::ggplot() +
  geom_sf(data = data_raster$Ndep_exceedance$`2020`, mapping = aes(fill = EXNMAX), color = NA) +
  ggplot2::geom_sf(data = boundaries_hull, fill = NA) +
  ggplot2::coord_sf(datum = sf::st_crs(crs)) +
  immission_colorscale_ndep_exc +
  ggplot2::ggtitle(label = "Belastungskarte Überschreitung Stickstoffdeposition",
                   subtitle = "max. Ndep > CLN in empfindlichen Ökosystemen, Jahressumme 2020") +
  ggplot2::labs(caption = "Quelle: BAFU") +
  theme_map







# plotting air pollutant population and ecosystem exposition
# ------------------------------------------------------------

# plot inhabitant exposure distribution
# -----------------------------------------------

# ... for NO2
plots$exposition$NO2$distribution <-
  lapply(setNames(names(data_expo$NO2$exposition_distrib), names(data_expo$NO2$exposition_distrib)), function(year) {
    ggplot_expo_hist(
      data = data_expo$NO2$exposition_distrib[[year]], x = "NO2", y = "population", barwidth = 1,
      xlims = c(0,90), xbreaks = seq(0,90,10), threshold = get_threshold(threshold_values, "NO2"),
      xlabel = ggplot2::xlab(openair::quickText("Jahresmittel-Belastung NO2 (µg/m3)")),
      titlelab = ggplot2::ggtitle(
        label = openair::quickText("Bevölkerungsexposition - Stickstoffdioxid (NO2)"),
        subtitle = openair::quickText(paste0("Anzahl Personen, Wohnbevölkerung im Kanton Zürich im Jahr ",year))
      ), 
      captionlab = ggplot2::labs(caption = "Datengrundlage: BAFU & BFS"),
      fill_scale = immission_colorscale_no2, theme = theme_ts
    ) 
  })

plots$exposition$NO2$cumulative <-
  lapply(setNames(names(data_expo$NO2$exposition_distrib), names(data_expo$NO2$exposition_distrib)), function(year) {
    ggplot_expo_cumulative(
      data = data_expo$NO2$exposition_distrib[[year]], x = "NO2", y = "population_relative", linewidth = 1,
      xlims = c(0,91), xbreaks = seq(0,90,10), threshold = get_threshold(threshold_values, "NO2"),
      xlabel = ggplot2::xlab(openair::quickText("Jahresmittel-Belastung NO2 (µg/m3)")),
      titlelab = ggplot2::ggtitle(
        label = openair::quickText("Bevölkerungsexposition - Stickstoffdioxid (NO2)"),
        subtitle = openair::quickText(paste0("relativer Anteil (kumuliert), Wohnbevölkerung im Kanton Zürich im Jahr ",year))
      ), 
      captionlab = ggplot2::labs(caption = "Datengrundlage: BAFU & BFS"),
      theme = theme_ts
    ) 
  })

# ... for PM10
plots$exposition$PM10$distribution <-
  lapply(setNames(names(data_expo$PM10$exposition_distrib), names(data_expo$PM10$exposition_distrib)), function(year) {
    ggplot_expo_hist(
      data = data_expo$PM10$exposition_distrib[[year]], x = "PM10", y = "population", barwidth = 0.2,
      xlims = c(0,24), xbreaks = seq(0,24,2), threshold = get_threshold(threshold_values, "PM10"),
      xlabel = ggplot2::xlab(openair::quickText("Jahresmittel-Belastung PM10 (µg/m3)")),
      titlelab = ggplot2::ggtitle(
        label = openair::quickText("Bevölkerungsexposition - Feinstaub PM10"),
        subtitle = openair::quickText(paste0("Anzahl Personen, Wohnbevölkerung im Kanton Zürich im Jahr ",year))
      ), 
      captionlab = ggplot2::labs(caption = "Datengrundlage: BAFU & BFS"),
      fill_scale = immission_colorscale_pm10, theme = theme_ts
    )
  })

plots$exposition$PM10$cumulative <-
  lapply(setNames(names(data_expo$PM10$exposition_distrib), names(data_expo$PM10$exposition_distrib)), function(year) {
    ggplot_expo_cumulative(
      data = data_expo$PM10$exposition_distrib[[year]], x = "PM10", y = "population_relative", linewidth = 1,
      xlims = c(0,24), xbreaks = seq(0,24,2), threshold = get_threshold(threshold_values, "PM10"),
      xlabel = ggplot2::xlab(openair::quickText("Jahresmittel-Belastung PM10 (µg/m3)")),
      titlelab = ggplot2::ggtitle(
        label = openair::quickText("Bevölkerungsexposition - Feinstaub (PM10)"),
        subtitle = openair::quickText(paste0("relativer Anteil (kumuliert), Wohnbevölkerung im Kanton Zürich im Jahr ",year))
      ), 
      captionlab = ggplot2::labs(caption = "Datengrundlage: BAFU & BFS"),
      theme = theme_ts
    ) 
  })

# ... for PM2.5
plots$exposition$PM2.5$distribution <-
  lapply(setNames(names(data_expo$PM2.5$exposition_distrib), names(data_expo$PM2.5$exposition_distrib)), function(year) {
    ggplot_expo_hist(
      data = data_expo$PM2.5$exposition_distrib[[year]], x = "PM2.5", y = "population", barwidth = 0.2,
      xlims = c(0,16), xbreaks = seq(0,16,1), threshold = get_threshold(threshold_values, "PM2.5"),
      xlabel = ggplot2::xlab(openair::quickText("Jahresmittel-Belastung PM2.5 (µg/m3)")),
      titlelab = ggplot2::ggtitle(
        label = openair::quickText("Bevölkerungsexposition - Feinstaub PM2.5"),
        subtitle = openair::quickText(paste0("Anzahl Personen, Wohnbevölkerung im Kanton Zürich im Jahr ",year))
      ), 
      captionlab = ggplot2::labs(caption = "Datengrundlage: BAFU & BFS"),
      fill_scale = immission_colorscale_pm2_5, theme = theme_ts
    )
  })

plots$exposition$PM2.5$cumulative <-
  lapply(setNames(names(data_expo$PM2.5$exposition_distrib), names(data_expo$PM2.5$exposition_distrib)), function(year) {
    ggplot_expo_cumulative(
      data = data_expo$PM2.5$exposition_distrib[[year]], x = "PM2.5", y = "population_relative", linewidth = 1,
      xlims = c(0,16), xbreaks = seq(0,16,1), threshold = get_threshold(threshold_values, "PM2.5"),
      xlabel = ggplot2::xlab(openair::quickText("Jahresmittel-Belastung PM2.5 (µg/m3)")),
      titlelab = ggplot2::ggtitle(
        label = openair::quickText("Bevölkerungsexposition - Feinstaub (PM2.5)"),
        subtitle = openair::quickText(paste0("relativer Anteil (kumuliert), Wohnbevölkerung im Kanton Zürich im Jahr ",year))
      ), 
      captionlab = ggplot2::labs(caption = "Datengrundlage: BAFU & BFS"),
      theme = theme_ts
    )
  })

# ... for eBC
plots$exposition$eBC$distribution <-
  lapply(setNames(names(data_expo$eBC$exposition_distrib), names(data_expo$eBC$exposition_distrib)), function(year) {
    ggplot_expo_hist(
      data = data_expo$eBC$exposition_distrib[[year]], x = "eBC", y = "population", barwidth = 0.05,
      xlims = c(0,2.2), xbreaks = seq(0,2.2,0.2),
      xlabel = ggplot2::xlab(openair::quickText("Jahresmittel-Belastung eBC (µg/m3)")),
      titlelab = ggplot2::ggtitle(
        label = openair::quickText("Bevölkerungsexposition - Russ im Feinstaub (eBC)"),
        subtitle = openair::quickText(paste0("Anzahl Personen, Wohnbevölkerung im Kanton Zürich im Jahr ",year))
      ), 
      captionlab = ggplot2::labs(caption = "Datengrundlage: BAFU & BFS"),
      fill_scale = immission_colorscale_ebc, theme = theme_ts
    )
  })

plots$exposition$eBC$cumulative <-
  lapply(setNames(names(data_expo$eBC$exposition_distrib), names(data_expo$eBC$exposition_distrib)), function(year) {
    ggplot_expo_cumulative(
      data = data_expo$eBC$exposition_distrib[[year]], x = "eBC", y = "population_relative", linewidth = 1,
      xlims = c(0,2.2), xbreaks = seq(0,2.2,0.2),
      xlabel = ggplot2::xlab(openair::quickText("Jahresmittel-Belastung eBC (µg/m3)")),
      titlelab = ggplot2::ggtitle(
        label = openair::quickText("Bevölkerungsexposition - Russ im Feinstaub (eBC)"),
        subtitle = openair::quickText(paste0("relativer Anteil (kumuliert), Wohnbevölkerung im Kanton Zürich im Jahr ",year))
      ), 
      captionlab = ggplot2::labs(caption = "Datengrundlage: BAFU & BFS"),
      theme = theme_ts
    )
  })

# plot distributions of exceedance of critical loads for nitrogen 2020
plots$exposition$Ndep$distribution$`2020` <-
  data_raster$Ndep_exceedance$`2020` %>% 
  dplyr::select(EXNMAX) %>% 
  tibble::as_tibble() %>% 
  na.omit() %>% 
  dplyr::group_by(EXNMAX = floor(EXNMAX) + 0.5) %>% # abgerundet auf 1, Klassenmitte
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::ungroup() %>% 
  ggplot2::ggplot(aes(x = EXNMAX, y = n, fill = EXNMAX)) + 
  ggplot2::geom_bar(stat = "identity", color = NA, width = 1) +
  ggplot2::geom_vline(xintercept = 0, color = get_threshold(threshold_values, "NO2")$color[1], 
                      linetype = get_threshold(threshold_values, "NO2")$linetype[1], linewidth = get_threshold(threshold_values, "NO2")$linesize[1]) +
  ggplot2::geom_text(data = tibble::tibble(x = -0.25, label = "kritische Eintragsrate CLN"), mapping = aes(x = x, y = 0, label = label), size = get_threshold(threshold_values, "NO2")$labelsize[1], 
                     hjust = 0, vjust = 0, angle = 90, nudge_x = -42*0.01, inherit.aes = FALSE) +
  ggplot2::scale_x_continuous(limits = c(-5,45), breaks = seq(-5,45,5), expand = c(0.01,0.01)) +
  ggplot2::scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01), labels = function(x) format(x, big.mark = "'")) +
  immission_colorscale_ndep_exc +
  ggplot2::xlab(expression("max. Stickstoff-Überschuss im Vergleich zu den kritischen Eintragsraten (kgN " * ha^-1 * Jahr^-1 * ")")) +
  ggplot2::ggtitle(
    label = openair::quickText("Exposition empfindlicher Ökosysteme durch Stickstoffeinträge"),
    subtitle = "Anzahl empfindlicher Ökosysteme im Kanton Zürich im Jahr 2020" 
  ) +
  ggplot2::labs(caption = "Quelle: BAFU") +
  theme_ts +
  ggplot2::theme(axis.title.x = ggplot2::element_text())

plots$exposition$Ndep$cumulative$`2020` <- 
  data_raster$Ndep_exceedance$`2020` %>% 
  dplyr::select(EXNMAX) %>% 
  tibble::as_tibble() %>% 
  na.omit() %>% 
  dplyr::group_by(EXNMAX = floor(EXNMAX) + 0.5) %>% # abgerundet auf 1, Klassenmitte
  dplyr::summarise(n = dplyr::n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(EXNMAX) %>% 
  dplyr::mutate(n_relative = cumsum(n) / sum(n)) %>% 
  ggplot2::ggplot(aes(x = EXNMAX, y = n_relative)) + 
  ggplot2::geom_line(linewidth = linewidth, color = "gray40") +
  ggplot2::geom_vline(xintercept = 0, color = get_threshold(threshold_values, "NO2")$color[1], 
                      linetype = get_threshold(threshold_values, "NO2")$linetype[1], linewidth = get_threshold(threshold_values, "NO2")$linesize[1]) +
  ggplot2::geom_text(data = tibble::tibble(x = -0.25, label = "kritische Eintragsrate CLN"), mapping = aes(x = x, y = 0, label = label), size = get_threshold(threshold_values, "NO2")$labelsize[1], 
                     hjust = 0, vjust = 0, angle = 90, nudge_x = -50*0.01, inherit.aes = FALSE) +
  ggplot2::scale_y_continuous(limits = c(0,1), expand = c(0.01,0.01), labels = scales::percent_format()) +
  ggplot2::scale_x_continuous(limits = c(-5,45), breaks = seq(-5,45,5), expand = c(0.01,0.01)) +
  ggplot2::xlab(expression("max. Stickstoff-Überschuss im Vergleich zu den kritischen Eintragsraten (kgN " * ha^-1 * Jahr^-1 * ")")) +
  theme_ts +
  ggplot2::theme(axis.title.x = ggplot2::element_text()) +
  ggplot2::ggtitle(
    label = openair::quickText("Exposition empfindlicher Ökosysteme durch Stickstoffeinträge"),
    subtitle = "relativer Anteil empfindlicher Ökosysteme (kumuliert) im Kanton Zürich im Jahr 2020" 
  ) +
  ggplot2::labs(caption = "Quelle: BAFU")




# -----------------------------------------------
# plot population-weighted mean values (single value for Kanton Zürich & per municipality)
# -----------------------------------------------

# ... for NO2
plots$exposition$NO2$population_weighted_mean <-
  lapply(setNames(names(data_expo$NO2$population_weighted_mean), names(data_expo$NO2$population_weighted_mean)), function(year) {
    data_expo$NO2$population_weighted_mean[[year]]$municipalities %>% 
      ggplot2::ggplot(aes(fill = NO2)) +
      ggplot2::geom_sf() + 
      ggplot2::coord_sf(datum = sf::st_crs(crs)) +
      immission_colorscale_no2 +
      theme_map +
      ggplot2::ggtitle(
        label = openair::quickText("Bevölkerungsgewichtete Schadstoffbelastung - Stickstoffdioxid (NO2)"),
        subtitle = openair::quickText(paste0("Mittlere NO2-Belastung pro Einwohner/in nach Gemeinde im Jahr ",year,"; Kanton = ",data_expo$NO2$population_weighted_mean[[year]]$canton," µg/m3"))
      ) +
      ggplot2::labs(caption = "Datengrundlage: BAFU & BFS")
  })

# ... for PM10
plots$exposition$PM10$population_weighted_mean <-
  lapply(setNames(names(data_expo$PM10$population_weighted_mean), names(data_expo$PM10$population_weighted_mean)), function(year) {
    data_expo$PM10$population_weighted_mean[[year]]$municipalities %>% 
      ggplot2::ggplot(aes(fill = PM10)) +
      ggplot2::geom_sf() + 
      ggplot2::coord_sf(datum = sf::st_crs(crs)) +
      immission_colorscale_pm10 +
      theme_map +
      ggplot2::ggtitle(
        label = openair::quickText("Bevölkerungsgewichtete Schadstoffbelastung - Feinstaub (PM10)"),
        subtitle = openair::quickText(paste0("Mittlere PM10-Belastung pro Einwohner/in nach Gemeinde im Jahr ",year,"; Kanton = ",data_expo$PM10$population_weighted_mean[[year]]$canton," µg/m3"))
      ) +
      ggplot2::labs(caption = "Datengrundlage: BAFU & BFS")
  })

# ... for PM2.5
plots$exposition$PM2.5$population_weighted_mean <-
  lapply(setNames(names(data_expo$PM2.5$population_weighted_mean), names(data_expo$PM2.5$population_weighted_mean)), function(year) {
    data_expo$PM2.5$population_weighted_mean[[year]]$municipalities %>% 
      ggplot2::ggplot(aes(fill = PM2.5)) +
      ggplot2::geom_sf() + 
      ggplot2::coord_sf(datum = sf::st_crs(crs)) +
      immission_colorscale_pm2_5 +
      theme_map +
      ggplot2::ggtitle(
        label = openair::quickText("Bevölkerungsgewichtete Schadstoffbelastung - Feinstaub (PM2.5)"),
        subtitle = openair::quickText(paste0("Mittlere PM2.5-Belastung pro Einwohner/in nach Gemeinde im Jahr ",year,"; Kanton = ",data_expo$PM2.5$population_weighted_mean[[year]]$canton," µg/m3"))
      ) +
      ggplot2::labs(caption = "Datengrundlage: BAFU & BFS")
  })

# ... for eBC
plots$exposition$eBC$population_weighted_mean <-
  lapply(setNames(names(data_expo$eBC$population_weighted_mean), names(data_expo$eBC$population_weighted_mean)), function(year) {
    data_expo$eBC$population_weighted_mean[[year]]$municipalities %>% 
      ggplot2::ggplot(aes(fill = eBC)) +
      ggplot2::geom_sf() + 
      ggplot2::coord_sf(datum = sf::st_crs(crs)) +
      immission_colorscale_ebc +
      theme_map +
      ggplot2::ggtitle(
        label = openair::quickText("Bevölkerungsgewichtete Schadstoffbelastung - Russ im Feinstaub (eBC)"),
        subtitle = openair::quickText(paste0("Mittlere eBC-Belastung pro Einwohner/in nach Gemeinde im Jahr ",year,"; Kanton = ",data_expo$eBC$population_weighted_mean[[year]]$canton," µg/m3"))
      ) +
      ggplot2::labs(caption = "Datengrundlage: BAFU & BFS")
  })




# save complete plot-dataset
# ------------------------------------------------------------

# saveRDS(plots, "data/output/ggplots.rds")




# clean up
# ------------------------------------------------------------

rm(list = c("basesize", "col_lrv", "col_who", "lty_lrv", "lty_who", "lsz_lrv", "lsz_who", "lbsz", "url", "request"))
rm(list = c("req", "rsd", "rsd_filters", "data_temp", "data_temp2", "data_rsd", "data_rsd_per_norm", "data_rsd_per_yearmodel", "data_rsd_per_yearmeas", "rsd_meta", "emikat", "data_emikat", "cols_emissions"))
rm(list = c("parameters", "pointsize", "n_years", "temp", "cln", "data_temp", "data_ndep", "data_thrshlds", "scale_color_siteclass", "scale_fill_siteclass", "site_meta", "data", "files"))
rm(list = c("data_expo", "data_raster", "boundaries", "boundaries_hull", "threshold_values", "crs", "years", "theme_map", "theme_ts", "immission_colorscale_no2", "immission_colorscale_pm10",
            "immission_colorscale_pm2_5", "immission_colorscale_ebc", "immission_colorscale_nh3", "immission_colorscale_ndep", "immission_colorscale_ndep_exc"))



