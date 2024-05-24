# To-Do:
# ---
# finales set an Belastungskarten inkl O3 von Geolion mit allen verfügbaren Jahren
# Datensätze gezielt für Statistikamt-Visualisierungen in /data/output/ abspeichern
# ... NO2_PS Airmo Abfrage: inkl. Bauma Oberstufen, Wald Feuerwehr, Zürich Bullingerhof sobald 2023 verfügbar
# ... Messdaten & Rasterdaten 2023 ergänzen
# ... Weiteres gemäss "review" feedbacks





### -----------------------------------------------
### input data files
### -----------------------------------------------
files <- list()

### Estimated emissions of air pollutants in the Canton of Zürich for the years 2015, 2020, 2030 
### - source: OSTLUFT and Amt für Abfall, Wasser, Energie und Luft (AWEL) / Kanton Zürich
### - derived as open data from: https://www.zh.ch/de/politik-staat/statistik-daten/datenkatalog.html#/datasets/2382@awel-kanton-zuerich
### - accessed: on the fly via opendata.swiss api:
files$emissions$budget$opendata <- 'https://ckan.opendata.swiss/api/3/action/package_show?id=luftschadstoffemissionen-im-kanton-zurich'

### Monitoring data vehicle remote emission sensing (RSD) 
### - source: Amt für Abfall, Wasser, Energie und Luft (AWEL) / Kanton Zürich / Schweiz, Langjährige Abgasmessungen im realen Fahrbetrieb mittels Remote Sensing.
### - derived as open data from: https://www.zh.ch/de/politik-staat/statistik-daten/datenkatalog.html#/datasets/2382@awel-kanton-zuerich
### - accessed: on the fly via opendata.swiss api:
files$emissions$rsd$opendata <- 'https://ckan.opendata.swiss/api/3/action/package_show?id=messdaten-langjahriger-abgasmessungen-im-realen-fahrbetrieb-mittels-remote-sensing-rsd'
files$emissions$rsd$meta <- "misc/rsd_auxiliary_data.csv" # some offline additional data per Euronorm etc

### Selected air quality monitoring data Kanton Zürich from national monitoring network NABEL (yearly values)
### - source: Bundesamt für Umwelt BAFU/NABEL
### - derived as open data from: https://www.arias.ch/ibonline/ib_online.php
### - retrieved on 14.12.2023
files$airquality$monitoring$nabel_y1 <- "airquality_monitoring/nabel_ib_download_y1.csv"

### Selected air quality monitoring data Kanton Zürich from intercantonal monitoring network OSTLUFT (yearly values)
### - source: OSTLUFT
### - derived as (open) data from: direct export by OSTLUFT (can be in principal downloaded as open data from https://www.ostluft.ch/index.php?id=datenabfragen, but only for smaller queriesy)
### - retrieved on 15.12.2023
files$airquality$monitoring$ostluft_y1 <- "airquality_monitoring/ostluft_airmo_y1_kanton_zurich.csv"

### Selected O3 peak-season monitoring data in Kanton Zürich from intercantonal monitoring network OSTLUFT and national monitoring network NABEL (yearly values)
### - source: OSTLUFT & NABEL (BAFU & Empa)
### - derived as (open) data in hourly aggregation intervals; O3 peak-season calculation is performed offline on these data
### - compiled on 19.02.2024
files$airquality$monitoring$ostluft_nabel_peakseason_y1 <- "airquality_monitoring/ostluft_nabel_offline_o3_peakseason_y1_kanton_zurich.csv"

### Selected nitrogen deposition monitoring data Kanton Zürich from intercantonal monitoring network OSTLUFT (yearly values)
### - source: OSTLUFT
### - derived from: offline calculations to derive component nitrogen deposition to sensitive ecosystems based on measured NH3 concentrations and, if available,
###   further measured components of nitrogen deposition; if further component deposition was not measured, it has been derived by statistical relationships.
###   The fundamentals of the process to derive component nitrogen deposition is documented elsewhere: ... link Fachbericht.
### - compiled on 05.01.2024
files$airquality$monitoring$ostluft_ndep_y1 <- "airquality_monitoring/ostluft_offline_ndep_y1_kanton_zurich.csv"

### site metadata OSTLUFT
### - source: OSTLUFT
### - derived as excerpt from internal OSTLUFT metadatabase
### - compiled on 26.01.2024
files$airquality$monitoring$ostluft_meta <- "airquality_monitoring/ostluft_site_metadata.csv"

### Selected threshold values (LRV legal limits WHO air quality guideline 
### - source: Luftreinhalteverordnung des Bundes and World Health Organization WHO (2021), see also EKL (2023): https://www.ekl.admin.ch/inhalte/dateien/pdf/EKL-231120_de_orig.pdf
### - derived from: https://www.fedlex.admin.ch/eli/cc/1986/208_208_208/de and https://www.who.int/publications/i/item/9789240034228
### - compiled on 14.12.2023
files$airquality$thresh <- "misc/airquality_threshold_values.csv"

### Vector data administrative Canton and municipality boundaries Canton Zürich
### - source: Kanton Zürich
### - derived on the fly as open data from: https://geolion.zh.ch/geodatenservice/1054
files$boundaries$wfs <- "https://maps.zh.ch/wfs/GemZHWFS" 

### PolluMap raster data yearly concentrations of mean nitrogen dioxide (NO2), mean particulate matter PM10, mean particulate matter PM2.5, eBC, ozone 95%-percentile
### - resolution: variable
### - source:, Bundesamt für Umwelt BAFU / Meteotest on https://geolion.zh.ch
### - derived on the fly for the year 2025 from https://geolion.zh.ch/geodatenservice/1058 and for 2020 onward from: https://geolion.zh.ch/geodatensatz/4730 (NO2), https://geolion.zh.ch/geodatensatz/4729 (PM2.5), https://geolion.zh.ch/geodatensatz/4731 (PM10)
files$rasterdata$bafu_airquality$pollumap <- "http://wms.zh.ch/ImmissionenZHWCS"
files$rasterdata$bafu_airquality$jahreskarte$no2 <- "http://wms.zh.ch/AwelLHNO2JahreZHWCS"
files$rasterdata$bafu_airquality$jahreskarte$o3p98 <- "http://wms.zh.ch/AwelLHMP98JahreZHWCS"
files$rasterdata$bafu_airquality$jahreskarte$pm25 <- "http://wms.zh.ch/AwelLHPM25JahreZHWCS"
files$rasterdata$bafu_airquality$jahreskarte$pm10 <- "http://wms.zh.ch/AwelLHPM10JahreZHWCS"

### Modelled raster data ammonia concentrations 2020
### - resolution: 500x500m
### - source: Bundesamt für Umwelt BAFU
### - derived on the fly as open data from: https://data.geo.admin.ch/browser/index.html#/collections/ch.bafu.luftreinhaltung-ammoniakkonzentration?.language=en
### - needs to be aquired by temporary download of *.zip and local extraction of needed data
files$rasterdata$bafu_nh3 <- "https://data.geo.admin.ch/ch.bafu.luftreinhaltung-ammoniakkonzentration/luftreinhaltung-ammoniakkonzentration/luftreinhaltung-ammoniakkonzentration_2056.shp.zip"

### Modelled raster data nitrogen deposition 2020
### - resolution: 500x500m
### - source: Bundesamt für Umwelt BAFU
### - derived as on the fly open data from: https://data.geo.admin.ch/browser/index.html#/collections/ch.bafu.luftreinhaltung-stickstoffdeposition?.language=en
### - needs to be aquired by temporary download of *.zip and local extraction of needed data
files$rasterdata$bafu_ndep <- "https://data.geo.admin.ch/ch.bafu.luftreinhaltung-stickstoffdeposition/luftreinhaltung-stickstoffdeposition/luftreinhaltung-stickstoffdeposition_2056.shp.zip"

### Modelled raster data exceedance of critical loads for nitrogen (CLN) 2020
### - resolution: 1000x1000m
### - source: Bundesamt für Umwelt BAFU
### - derived on the fly as open data from: https://data.geo.admin.ch/browser/index.html#/collections/ch.bafu.luftreinhaltung-stickstoff_kritischer_eintrag?.language=en
### - needs to be aquired by temporary download of *.zip and local extraction of needed data
files$rasterdata$bafu_ndep_exc <- "https://data.geo.admin.ch/ch.bafu.luftreinhaltung-stickstoff_kritischer_eintrag/luftreinhaltung-stickstoff_kritischer_eintrag/luftreinhaltung-stickstoff_kritischer_eintrag_2056.shp.zip"

### Raster data inhabitant-statistics
### - resolution: 100x100m
### - source: Bundesamt für Statistik STATPOP
### - derived on the fly as open data from: https://www.bfs.admin.ch/bfs/de/home/dienstleistungen/geostat/geodaten-bundesstatistik/gebaeude-wohnungen-haushalte-personen/bevoelkerung-haushalte-ab-2010.html
files$rasterdata$bfs_pop$"2015" <- "https://dam-api.bfs.admin.ch/hub/api/dam/assets/1442443/master"
files$rasterdata$bfs_pop$"2020" <- "https://dam-api.bfs.admin.ch/hub/api/dam/assets/19106709/master"
files$rasterdata$bfs_pop$"2021" <- "https://dam-api.bfs.admin.ch/hub/api/dam/assets/23528269/master"
files$rasterdata$bfs_pop$"2022" <- "https://dam-api.bfs.admin.ch/hub/api/dam/assets/27965868/master"




### -----------------------------------------------
### RSD data analysis filter criteria
### -----------------------------------------------
rsd_filters <- 
  list(
    nmin = 50, # minimum number of valid records per aggregation
    vehicleyears = 1992:lubridate::year(Sys.Date()), # vehicle model years
    velocityrange = c(5, 60), # range of vehicle velocity in km/h
    accelerationrange = c(-2, 4), # range of vehicle acceleration in km/h/s
    vsprange = c(1, 35), # range of vehicle specific power in n kW/t
    weightmax = 3500 # maximum vehicle unloaded weight in kg
  )




### -----------------------------------------------
### air quality immission threshold values
### -----------------------------------------------

### LRV & WHO air quality guideline threshold values
threshold_values <-
  fs::path("data/input", files$airquality$thresh) %>%
  readr::read_delim(delim = ";",locale = readr::locale(encoding = "UTF-8"))

### add plotting parameter
threshold_values <-
  tibble::tibble(
    source = c("LRV Grenzwert", "WHO Richtwert"),
    col = c(col_lrv, col_who),
    lty = c(lty_lrv, lty_who),
    lsz = c(lsz_lrv, lsz_who),
    lbsz = lbsz
  ) %>% 
  dplyr::right_join(threshold_values, by = "source")




### -----------------------------------------------
### map fundamentals
### -----------------------------------------------

### map projection CRS = LV95
crs <- 2056 # default map projection: CH1903+ / LV95

### from https://geolion.zh.ch/
### query in R, see e.g. here: https://inbo.github.io/tutorials/tutorials/spatial_wfs_services/

# client <- ows4R::WFSClient$new(files$boundaries$wfs, serviceVersion = "2.0.0")
# client$getFeatureTypes(pretty = TRUE)
# client$getCapabilities()
# client$
#   getCapabilities()$
#   getOperationsMetadata()$
#   getOperations() %>%
#   purrr::map_chr(function(x){x$getName()})
# client$
#   describeFeatureType(typeName = "ms:grenzen") %>%
#   purrr::map_chr(function(x){x$getName()})

url <- httr2::url_parse(files$boundaries$wfs)
url$query <- list(service = "wfs",
                  version = "2.0.0",
                  request = "GetFeature",
                  typename = "ms:gem_grenzen", # "ms:gem_seen_grenzen",
                  srsName = paste0("EPSG:", crs)
)
request <- httr2::url_build(url)
boundaries <- 
  request %>% 
  sf::read_sf(type = 6) %>% 
  sf::st_transform(crs = sf::st_crs(crs))

# ggplot() +
#   geom_sf(data = boundaries) +
#   theme_void()

# boundaries %>%
#   st_union() %>%
#   st_boundary() %>%
#   st_cast("POLYGON") %>%
#   ggplot() +
#   geom_sf() +
#   theme_void()

boundaries_hull <-
  boundaries %>%
  sf::st_union() %>%
  sf::st_boundary() %>% 
  sf::st_cast("POLYGON")














### -----------------------------------------------
### plotting parameter
### -----------------------------------------------

years <- 2000:(lubridate::year(Sys.Date()) - 2) # years to consider for plotting 
n_years <- 3 # consider last 3 years for plotting relative threshold comparison    
parameters <- c("NO2", "NO2_PS", "PM10", "PM2.5", "O3_max_98%_m1", "O3_peakseason_mean_d1_max_mean_h8gl") # parameters to include for timeseries plotting

basesize <- 12 # ggplot theme base_size
pointsize <- 2 # size of point markers
linewidth <- 1 # width of lines

col_lrv <- "red3" # color of LRV threshold value
col_who <- "gray30" # color of WHO guideline threshold value
lty_lrv <- 1 # line type of LRV threshold value
lty_who <- 2 # line type WHO guideline threshold value
lsz_lrv <- 1 # line width of LRV threshold value
lsz_who <- 1 # line width of WHO guideline threshold value
lbsz <- 4 # label size of threshold value line text

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

immission_colorscale_no2 <- immission_colorscale(limits = c(0,50), breaks = seq(0,50,10), name = "NO2\n(µg/m3)")
immission_colorscale_pm10 <- immission_colorscale(limits = c(0,34), breaks = c(seq(0,30,10), 34), name = "PM10\n(µg/m3)")
immission_colorscale_pm2_5 <- immission_colorscale(limits = c(0,17), breaks = c(seq(0,15,2.5), 17), name = "PM2.5\n(µg/m3)")
immission_colorscale_ebc <- immission_colorscale(limits = c(0,1.5), breaks = seq(0,1.5,0.3), name = "eBC\n(µg/m3)")
immission_colorscale_nh3 <- rOstluft.plot::scale_fill_viridis_squished(name = "NH3\n(µg/m3)", limits = c(1, 7), breaks = seq(1, 7, 2), direction = -1,  option = "A", na.value = NA)
immission_colorscale_ndep <- rOstluft.plot::scale_fill_viridis_squished(name = "Ndep\n(kgN/ha/Jahr)", limits = c(15, 30), breaks = seq(15, 30, 5), direction = -1, option = "A", na.value = NA)
immission_colorscale_ndep_exc <- rOstluft.plot::scale_fill_viridis_squished(name = "Ndep > CLN\n(kgN/ha/Jahr)", limits = c(0, 30), breaks = seq(0, 30, 5), direction = -1, option = "A", na.value = NA)

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

plots <- list() # empty list to collect all plots




### clean up
rm(list = c("basesize", "col_lrv", "col_who", "lty_lrv", "lty_who", "lsz_lrv", "lsz_who", "lbsz", "url", "request"))


























