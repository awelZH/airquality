# add Langzeitbelastungsindex LBI
# ... currently, LBI needs to be revised by Cercl'Air => prepared for later inclusion
immission_threshold_values <- readr::read_delim(paste("inst/extdata", files$airquality$thresh, sep = "/"), delim = ";",locale = readr::locale(encoding = "UTF-8"))
data_monitoring_aq <-
  data_monitoring_aq %>%
  calc_lbi(immission_threshold_values) %>%
  dplyr::bind_rows(data_monitoring_aq)





# request available ogd datasets:
req <- httr2::request("https://opendata.swiss/api/3/action/package_list")
req <- httr2::req_perform(req)
opendatasets <- unlist(httr2::resp_body_json(req)$result) # all available datasets
opendatasets[stringr::str_detect(opendatasets, "luftschadstoffemissionen-im-kanton-zurich")] # => exists
opendatasets[stringr::str_detect(opendatasets, "messdaten-langjahriger-abgasmessungen-im-realen-fahrbetrieb-mittels-remote-sensing-rsd")] # => exists








# plotting air pollutant maps
# ------------------------------------------------------------

# get all available raster data regarding inhabitant population (from BFS), air pollutants (from geolion) and reactive nitrogen (from data.geo.admin); join population and air pollutant data
#FIXME: see issue 12
data_raster <- get_prepare_raster_data(files, map_canton)

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






# plot distribution of yearly nitrogen deposition across several monitoring sites since 2019 (structured per year and ecosystem type)
plots$airquality$monitoring$Ndep$all <-
  data_monitoring_ndep %>%
  dplyr::filter(year >= 2019 & parameter != "N-Deposition") %>%
  dplyr::group_by(year, site, site_long, siteclass, ecosystem_category, critical_load_min, critical_load_single, critical_load_max, parameter, unit) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup() %>%
  plot_timeseries_ndep_bars(xbreaks = seq(2000,max(years), 2)) +
  ggplot2::geom_text(data = dplyr::filter(data_monitoring_ndep, year >= 2019 & parameter == "N-Deposition" & estimate == "geschätzt"), label = "*", color = "gray40") +
  ggplot2::labs(caption = "*: mind. NH3 gemessen, restlicher Eintrag geschätzt; Quelle: OSTLUFT") +
  ggh4x::facet_nested_wrap(.~ecosystem_category*site, nrow = 2, strip.position = "top", axes = "x", solo_line = TRUE) +
  ggplot2::ggtitle(
    label = openair::quickText("Luftqualitätsmesswerte - Stickstoffeintrag in empfindliche Ökosysteme"),
    subtitle = expression("Stickstoffeintrag (kgN " * ha^-1 * Jahr^-1 * ")")
  ) +
  ggplot2::theme(
    strip.text = ggplot2::element_text(size = ggplot2::rel(0.66), hjust = 0.5),
    ggh4x.facet.nestline = ggplot2::element_line(colour = "gray40"),
    panel.spacing = unit(2, "lines"),
    legend.position = "bottom"
  )





