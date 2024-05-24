
### air quality modelling raster data 
### (semi-empirical, calibrated by monitoring data, source: BAFU/Meteotest)
### as well as inhabitant raster data
### ------------------------------------------------------------

### read, restructure and combine data
### ... raster data can have different grids, even for same pollutant (various years)
### ... since these data are primarily used to derive inhabitant exposition (and not for raster plotting),
### ... which are on a standard ha raster, pollutant grids are generalised to 100 x 100m raster

data_raster <- list()
capabilities <- list()
maplist <- list()

### connection to pollumap air quality raster data on https://geolion.zh.ch for the year 2015 (adopted from here: https://cran.r-project.org/web/packages/ows4R/vignettes/wcs.html)

client <- ows4R::WCSClient$new(files$rasterdata$bafu_airquality$pollumap, serviceVersion = "2.0.1")
capabilities$pollumap <- client$getCapabilities()
maplist <- add_to_maplist(maplist, "pollumap", NULL)
maplist$pollumap <- maplist$pollumap[extract_year(maplist$pollumap) == 2015] # make sure, only pollumap 2015 is included

### connection to NO2-Jahreskarte air quality raster data on https://geolion.zh.ch for the years 2020ff

client <- ows4R::WCSClient$new(files$rasterdata$bafu_airquality$jahreskarte$no2, serviceVersion = "1.0.0") # "2.0.1")
capabilities$jahreskarte$no2 <- client$getCapabilities()
maplist <- add_to_maplist(maplist, "jahreskarte", "no2")

### connection to O3p98-Jahreskarte air quality raster data on https://geolion.zh.ch for the years 2020ff

client <- ows4R::WCSClient$new(files$rasterdata$bafu_airquality$jahreskarte$o3p98, serviceVersion = "1.0.0") # "2.0.1")
capabilities$jahreskarte$o3p98 <- client$getCapabilities()
maplist <- add_to_maplist(maplist, "jahreskarte", "o3p98")

### connection to PM2.5-Jahreskarte air quality raster data on https://geolion.zh.ch for the years 2020ff

client <- ows4R::WCSClient$new(files$rasterdata$bafu_airquality$jahreskarte$pm25, serviceVersion = "1.0.0") # "2.0.1")
capabilities$jahreskarte$pm25 <- client$getCapabilities()
maplist <- add_to_maplist(maplist, "jahreskarte", "pm25")

### connection to PM10-Jahreskarte air quality raster data on https://geolion.zh.ch for the years 2020ff

client <- ows4R::WCSClient$new(files$rasterdata$bafu_airquality$jahreskarte$pm10, serviceVersion = "1.0.0") # "2.0.1")
capabilities$jahreskarte$pm10 <- client$getCapabilities()
maplist <- add_to_maplist(maplist, "jahreskarte", "pm10")

### all maps in one vector

maps <- unlist(maplist)

### population data: download, read and restructure BFS population raster data from BFS homepage for the years in which air quality maps are available

data_raster$population <-
  setNames(as.character(unique(extract_year(maps))), unique(extract_year(maps))) %>% 
  lapply(function(year) {
    data <- read_bfs_zip_data(url = as.character(files$rasterdata$bfs_pop[year]), path_destination = "data/input")
    return(sf::st_crop(data, boundaries_hull))
  })

### BFS 100 x 100m grid is similar across the years => pick one grid for all years

grid <- dplyr::select(data_raster$population[[1]], RELI) 

### retrieve air pollution maps

### download, read and restructure BAFU NO2 raster data from geolion WCS (source = BAFU)

data_raster$NO2 <- lapply(maps[stringr::str_detect(maps, "no2")], function(coverage) get_map(coverage, capabilities, maps, "NO2", grid, boundaries_hull))

### download, read and restructure PM10 raster data from geolion WCS (source = BAFU)

data_raster$PM10 <- lapply(maps[stringr::str_detect(maps, "10")], function(coverage) get_map(coverage, capabilities, maps, "NO2", grid, boundaries_hull))

### download, read and restructure PM2.5 raster data from geolion WCS (source = BAFU)

data_raster$PM2.5 <-lapply(maps[stringr::str_detect(maps, "25")], function(coverage) get_map(coverage, capabilities, maps, "NO2", grid, boundaries_hull))

### download, read and restructure eBC raster data from geolion WCS (source = BAFU)

data_raster$eBC <- lapply(maps[stringr::str_detect(maps, "bc")], function(coverage) get_map(coverage, capabilities, maps, "NO2", grid, boundaries_hull))

### download, read and restructure O3 max. monthly 98%-percentile raster data from geolion WCS (source = BAFU)

data_raster$O3p98 <-lapply(maps[stringr::str_detect(maps, "98")], function(coverage) get_map(coverage, capabilities, maps, "NO2", grid, boundaries_hull))





### download, read and restructure NH3 raster data for the year 2020 from https://data.geo.admin.ch (source = BAFU)
data_raster$NH3 <-
  setNames(files$rasterdata$bafu_nh3, 2020) %>%
  lapply(function(x) {
    data <- read_bafu_zip_shp(x, path_destination = "data/input")
    return(dplyr::filter(data, st_intersects(data, boundaries_hull, sparse = FALSE)))
  })

### download, read and restructure reactive nitrogen deposition (Ndep) raster data for the year 2020 from https://data.geo.admin.ch (source = BAFU)
data_raster$Ndep <-
  setNames(files$rasterdata$bafu_ndep, 2020) %>%
  lapply(function(x) {
    data <- read_bafu_zip_shp(x, path_destination = "data/input")
    return(dplyr::filter(data, st_intersects(data, boundaries_hull, sparse = FALSE)))
  })

### get / calculate exposition data

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

data_raster$O3p98 <-
  setNames(names(data_raster$PM10), names(data_raster$PM10)) %>% 
  lapply(function(year) {sf::st_join(data_raster$PM10[[year]], data_raster$population[[year]])})

### exposition already available: download, read and restructure max Ndep > Critical Loads of Nitrogen (CLN) raster data for the year 2020 from https://data.geo.admin.ch (source = BAFU)

data_raster$Ndep_exceedance <-
  setNames(files$rasterdata$bafu_ndep_exc, 2020) %>%
  lapply(function(x) {
    data <- read_bafu_zip_shp(x, path_destination = "data/input")
    return(dplyr::filter(data, st_intersects(data, boundaries_hull, sparse = FALSE)))
  })




### clean up
### ------------------------------------------------------------

rm(list = "client", "capabilities", "maps", "grid")
