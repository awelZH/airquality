### -----------------------------------------------
### -----------------------------------------------
### air pollution inhabitant / sensitive ecosystem exposition
### -----------------------------------------------
### -----------------------------------------------


data_expo <- list()

### -----------------------------------------------
### calculate population-weighted mean values per municipality (and for canton)
### -----------------------------------------------

### ... for NO2
data_expo$NO2$population_weighted_mean <- 
  lapply(setNames(names(data_raster$NO2), names(data_raster$NO2)), function(year) {
    data <- sf::st_join(boundaries, sf::st_as_sf(data_raster$NO2[[year]]))
    canton <- round.off(population_weighted_mean(data$NO2, data$population), 1)
    municipalities <- aggregate_population_weighted_mean(data, y = "NO2")
    municipalities <- dplyr::left_join(boundaries, municipalities, by = "geodb_oid")
    return(list(canton = canton, municipalities = municipalities))
  })

### ... for PM10
data_expo$PM10$population_weighted_mean <- 
  lapply(setNames(names(data_raster$PM10), names(data_raster$PM10)), function(year) {
    data <- sf::st_join(boundaries, sf::st_as_sf(data_raster$PM10[[year]]))
    canton <- round(population_weighted_mean(data$PM10, data$population), 1)
    municipalities <- aggregate_population_weighted_mean(data, y = "PM10")
    municipalities <- dplyr::left_join(boundaries, municipalities, by = "geodb_oid")
    return(list(canton = canton, municipalities = municipalities))
  })

### ... for PM2.5
data_expo$PM2.5$population_weighted_mean <- 
  lapply(setNames(names(data_raster$PM2.5), names(data_raster$PM2.5)), function(year) {
    data <- sf::st_join(boundaries, sf::st_as_sf(data_raster$PM2.5[[year]]))
    canton <- round.off(population_weighted_mean(data$PM2.5, data$population), 1)
    municipalities <- aggregate_population_weighted_mean(data, y = "PM2.5")
    municipalities <- dplyr::left_join(boundaries, municipalities, by = "geodb_oid")
    return(list(canton = canton, municipalities = municipalities))
  })

### ... for eBC
data_expo$eBC$population_weighted_mean <- 
  lapply(setNames(names(data_raster$eBC), names(data_raster$eBC)), function(year) {
    data <- sf::st_join(boundaries, sf::st_as_sf(data_raster$eBC[[year]]))
    canton <- round.off(population_weighted_mean(data$eBC, data$population), 1)
    municipalities <- aggregate_population_weighted_mean(data, y = "eBC")
    municipalities <- dplyr::left_join(boundaries, municipalities, by = "geodb_oid")
    return(list(canton = canton, municipalities = municipalities))
  })

### ... for O3p98
# ...



### -----------------------------------------------
### calculate exposition distribution 
### (absolute and relative/cumlulative)
### -----------------------------------------------

### ... for NO2
data_expo$NO2$exposition_distrib <- 
  lapply(setNames(names(data_raster$NO2), names(data_raster$NO2)), function(year) {
    data <- aggregate_exposition_distrib(data_raster$NO2[[year]], y = "NO2") # abgerundet auf 1, Klassenmitte
    exposition_distrib_cumulative(data, y = "NO2")
  })

### ... for PM10
data_expo$PM10$exposition_distrib <- 
  lapply(setNames(names(data_raster$PM10), names(data_raster$PM10)), function(year) {
    data <- aggregate_exposition_distrib(data_raster$PM10[[year]], y = "PM10", fun = function(x) {floor(x * 5) / 5 + 0.1}) # abgerundet auf 0.2, Klassenmitte
    exposition_distrib_cumulative(data, y = "PM10")
  })

### ... for PM2.5
data_expo$PM2.5$exposition_distrib <- 
  lapply(setNames(names(data_raster$PM2.5), names(data_raster$PM2.5)), function(year) {
    data <- aggregate_exposition_distrib(data_raster$PM2.5[[year]], y = "PM2.5", fun = function(x) {floor(x * 5) / 5 + 0.1}) # abgerundet auf 0.2, Klassenmitte
    exposition_distrib_cumulative(data, y = "PM2.5")
  })

### ... for eBC
data_expo$eBC$exposition_distrib <- 
  lapply(setNames(names(data_raster$eBC), names(data_raster$eBC)), function(year) {
    data <- aggregate_exposition_distrib(data_raster$eBC[[year]], y = "eBC", fun = function(x) {floor(x * 20) / 20 + 0.025}) # abgerundet auf 0.05, Klassenmitte
    exposition_distrib_cumulative(data, y = "eBC")
  })











### -----------------------------------------------
### plot inhabitant exposure distribution
### -----------------------------------------------

### ... for NO2
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

### ... for PM10
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

### ... for PM2.5
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

### ... for eBC
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

### plot distributions of exceedance of critical loads for nitrogen 2020
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




### -----------------------------------------------
### plot population-weighted mean values (single value for Kanton Zürich & per municipality)
### -----------------------------------------------

### ... for NO2
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

### ... for PM10
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

### ... for PM2.5
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

### ... for eBC
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





### -----------------------------------------------
### save complete plot-dataset and clean up
### -----------------------------------------------

# saveRDS(plots, "data/output/ggplots.rds")

rm(list = c("data_expo", "data_raster", "boundaries", "boundaries_hull", "threshold_values", "crs", "years", "theme_map", "theme_ts", "immission_colorscale_no2", "immission_colorscale_pm10",
           "immission_colorscale_pm2_5", "immission_colorscale_ebc", "immission_colorscale_nh3", "immission_colorscale_ndep", "immission_colorscale_ndep_exc"))
