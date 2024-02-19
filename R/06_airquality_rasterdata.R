### -----------------------------------------------
### -----------------------------------------------
### pollutant-maps based on air quality modelling raster data 
### (semi-empirical, calibrated by monitoring data, source: BAFU/Meteotest)
### as well as inhabitant raster data
### -----------------------------------------------
### -----------------------------------------------


### -----------------------------------------------
### read, restructure and combine data
### ... raster data can have different grids, even for same pollutant (various years)
### ... since these data are primarily used to derive inhabitant exposition (and not for raster plotting),
### ... which are on a standard ha raster, pollutant grids are generalised to ha raster
### -----------------------------------------------

data_raster <- list()

### connection to air quality raster data on https://geolion.zh.ch
### adopted from here: https://cran.r-project.org/web/packages/ows4R/vignettes/wcs.html
### establish connection and get capabilities
client <- ows4R::WCSClient$new(files$rasterdata$bafu_airquality$wcs, serviceVersion = "2.0.1")
capabilities <- client$getCapabilities()
maps <- purrr::map_chr(capabilities$getCoverageSummaries(), function(x){x$CoverageId})
maps <- setNames(maps, extract_year(maps))
maps <- maps[extract_year(maps) %in% years] # make sure, scenario (future projection) data are not included


### read and restructure population data for the years in which air quality maps are available
# req <- httr2::request('https://api3.geo.admin.ch/rest/services/api/MapServer')
# req <- httr2::req_perform(req)
# layers <- httr2::resp_body_json(req)$layers
# maps <- unlist(purrr::map(layers, function(x) x$layerBodId))
# id <- which(stringr::str_detect(maps, "ch.bfs.volkszaehlung-bevoelkerungsstatistik_einwohner"))
# layers[[id]]
# layers[[id]]$attributes$downloadUrl # => only available as yearly zip files, or WMTS => data zip files need to be downloaded individually

data_raster$population <-
  setNames(as.character(unique(extract_year(maps))), unique(extract_year(maps))) %>% 
  lapply(function(year) {
    data <- read_statpop_csv(fs::path("data/input", files$rasterdata$bfs_pop[year]), year)
    return(sf::st_crop(data, boundaries_hull))
  })

grid <- dplyr::select(data_raster$population[[1]], RELI) # BFS 100 x 100m grid is similar across the years

### download, read and restructure NO2 raster data
data_raster$NO2 <- 
  maps[stringr::str_detect(maps, "no2")] %>% 
  lapply(function(cov) {
    print(cov)
    data <- 
      get_geolion_wcs(cov, capabilities, name = "NO2") %>%
      sf::st_crop(boundaries_hull) %>% 
      stars::st_warp(grid, method = "average", use_gdal = TRUE, no_data_value = -999) # make sure we have the same grid as BFS population data
    data <- setNames(data, "NO2")
    return(data)
  })

### download, read and restructure PM10 raster data
data_raster$PM10 <- 
  maps[stringr::str_detect(maps, "pm-10")] %>% 
  lapply(function(cov) {
    print(cov)
    data <- 
      get_geolion_wcs(cov, capabilities, name = "PM10") %>%
      sf::st_crop(boundaries_hull) %>% 
      stars::st_warp(grid, method = "average", use_gdal = TRUE, no_data_value = -999)
    data <- setNames(data, "PM10")
    return(data)
  })

### download, read and restructure PM2.5 raster data
data_raster$PM2.5 <- 
  maps[stringr::str_detect(maps, "pm-25")] %>% 
  lapply(function(cov) {
    print(cov)
    data <- 
      get_geolion_wcs(cov, capabilities, name = "PM2.5") %>%
      sf::st_crop(boundaries_hull) %>% 
      stars::st_warp(grid, method = "average", use_gdal = TRUE, no_data_value = -999)
    data <- setNames(data, "PM2.5")
    return(data)
  })

### download, read and restructure eBC raster data
data_raster$eBC <- 
  maps[stringr::str_detect(maps, "bc")] %>% 
  lapply(function(cov) {
    print(cov)
    data <- 
      get_geolion_wcs(cov, capabilities, name = "eBC", divisor = 100) %>%
      sf::st_crop(boundaries_hull) %>% 
      stars::st_warp(grid, method = "average", use_gdal = TRUE, no_data_value = -999)
    data <- setNames(data, "eBC")
    return(data)
  })

### download, read and restructure NH3 raster data for the year 2020
data_raster$NH3 <-
  setNames(files$rasterdata$bafu_nh3, 2020) %>%
  lapply(function(x) {
    data <- read_bafu_zip_shp(x, path_destination = "data/input")
    return(dplyr::filter(data, st_intersects(data, boundaries_hull, sparse = FALSE)))
  })

### download, read and restructure reactive nitrogen deposition (Ndep) raster data for the year 2020
data_raster$Ndep <-
  setNames(files$rasterdata$bafu_ndep, 2020) %>%
  lapply(function(x) {
    data <- read_bafu_zip_shp(x, path_destination = "data/input")
    return(dplyr::filter(data, st_intersects(data, boundaries_hull, sparse = FALSE)))
  })

### download, read and restructure max Ndep > Critical Loads of Nitrogen (CLN) raster data for the year 2020
data_raster$Ndep_exceedance <-
  setNames(files$rasterdata$bafu_ndep_exc, 2020) %>%
  lapply(function(x) {
    data <- read_bafu_zip_shp(x, path_destination = "data/input")
    return(dplyr::filter(data, st_intersects(data, boundaries_hull, sparse = FALSE)))
  })

### join air quality and population raster data
data_raster$NO2 <-
  setNames(names(data_raster$NO2), names(data_raster$NO2)) %>% 
  lapply(function(year) {sf::st_join(data_raster$NO2[[year]], data_raster$population[[year]])})

data_raster$PM10 <-
  setNames(names(data_raster$PM10), names(data_raster$PM10)) %>% 
  lapply(function(year) {sf::st_join(data_raster$PM10[[year]], data_raster$population[[year]])})

data_raster$PM2.5 <-
  setNames(names(data_raster$PM2.5), names(data_raster$PM2.5)) %>% 
  lapply(function(year) {sf::st_join(data_raster$PM2.5[[year]], data_raster$population[[year]])})

data_raster$eBC <-
  setNames(names(data_raster$eBC), names(data_raster$eBC)) %>% 
  lapply(function(year) {sf::st_join(data_raster$eBC[[year]], data_raster$population[[year]])})





### -----------------------------------------------
### plot air quality maps
### -----------------------------------------------

### maps of mean air pollutant concentrations
### ... for NO2
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

### ... for PM10
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

### ... for PM2.5
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

### ... for eBC
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

### map of mean NH3 concentration in 2020
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

### map of total nitrogen deposition in 2020
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

### map of total nitrogen deposition CLN exceedance in 2020
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



### clean up
# rm(list = "client", "capabilities", "maps", "grid")
