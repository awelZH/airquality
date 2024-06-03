# To-Do:
# ---
# finales set an Belastungskarten inkl O3 von Geolion mit allen verfügbaren Jahren
# Datensätze gezielt für Statistikamt-Visualisierungen in /data/output/ abspeichern
# ... NO2_PS Airmo Abfrage: inkl. Bauma Oberstufen, Wald Feuerwehr, Zürich Bullingerhof sobald 2023 verfügbar
# ... Messdaten & Rasterdaten 2023 ergänzen
# ... Weiteres gemäss "review" feedbacks






### input data files
### ------------------------------------------------------------

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
### - derived on the fly for the year 2015 from https://geolion.zh.ch/geodatenservice/1058 and for 2020 onward from: https://geolion.zh.ch/geodatensatz/4730 (NO2), https://geolion.zh.ch/geodatensatz/4729 (PM2.5), https://geolion.zh.ch/geodatensatz/4731 (PM10)
files$rasterdata$bafu_airquality$pollumap <- "http://wms.zh.ch/ImmissionenZHWCS"
files$rasterdata$bafu_airquality$jahreskarte$no2 <- "http://wms.zh.ch/AwelLHNO2JahreZHWCS"
files$rasterdata$bafu_airquality$jahreskarte$o3p98 <- "http://wms.zh.ch/AwelLHMP98JahreZHWCS"
files$rasterdata$bafu_airquality$jahreskarte$pm25 <- "http://wms.zh.ch/AwelLHPM25JahreZHWCS"
files$rasterdata$bafu_airquality$jahreskarte$pm10 <- "http://wms.zh.ch/AwelLHPM10JahreZHWCS"

### Modelled raster data ammonia concentrations
### - resolution: 500x500m
### - source: Bundesamt für Umwelt BAFU
### - derived on the fly as open data from: https://data.geo.admin.ch/browser/index.html#/collections/ch.bafu.luftreinhaltung-ammoniakkonzentration?.language=en
### - needs to be aquired by temporary download of *.zip and local extraction of needed data
files$rasterdata$bafu_nh3$"2020" <- "https://data.geo.admin.ch/ch.bafu.luftreinhaltung-ammoniakkonzentration/luftreinhaltung-ammoniakkonzentration/luftreinhaltung-ammoniakkonzentration_2056.shp.zip"

### Modelled raster data nitrogen deposition
### - resolution: 500x500m
### - source: Bundesamt für Umwelt BAFU
### - derived as on the fly open data from: https://data.geo.admin.ch/browser/index.html#/collections/ch.bafu.luftreinhaltung-stickstoffdeposition?.language=en
### - needs to be aquired by temporary download of *.zip and local extraction of needed data
files$rasterdata$bafu_ndep$"2020" <- "https://data.geo.admin.ch/ch.bafu.luftreinhaltung-stickstoffdeposition/luftreinhaltung-stickstoffdeposition/luftreinhaltung-stickstoffdeposition_2056.shp.zip"

### Modelled raster data exceedance of critical loads for nitrogen (CLN)
### - resolution: 1000x1000m
### - source: Bundesamt für Umwelt BAFU
### - derived on the fly as open data from: https://data.geo.admin.ch/browser/index.html#/collections/ch.bafu.luftreinhaltung-stickstoff_kritischer_eintrag?.language=en
### - needs to be aquired by temporary download of *.zip and local extraction of needed data
files$rasterdata$bafu_ndep_exc$"2020" <- "https://data.geo.admin.ch/ch.bafu.luftreinhaltung-stickstoff_kritischer_eintrag/luftreinhaltung-stickstoff_kritischer_eintrag/luftreinhaltung-stickstoff_kritischer_eintrag_2056.shp.zip"

### Raster data inhabitant-statistics
### - resolution: 100x100m
### - source: Bundesamt für Statistik STATPOP
### - derived on the fly as open data from: https://www.bfs.admin.ch/bfs/de/home/dienstleistungen/geostat/geodaten-bundesstatistik/gebaeude-wohnungen-haushalte-personen/bevoelkerung-haushalte-ab-2010.html
files$rasterdata$bfs_pop$"2015" <- "https://dam-api.bfs.admin.ch/hub/api/dam/assets/1442443/master"
files$rasterdata$bfs_pop$"2020" <- "https://dam-api.bfs.admin.ch/hub/api/dam/assets/19106709/master"
files$rasterdata$bfs_pop$"2021" <- "https://dam-api.bfs.admin.ch/hub/api/dam/assets/23528269/master"
files$rasterdata$bfs_pop$"2022" <- "https://dam-api.bfs.admin.ch/hub/api/dam/assets/27965868/master"

