
get_geolion_wfs <- function(wfs, version = "2.0.0", crs = 2056) {
  
  map <- 
    find_map_geolion(wfs = wfs, version = "2.0.0", crs = crs) |> 
    sf::read_sf(type = 6) |> 
    sf::st_transform(crs = sf::st_crs(crs))
  
  return(map)
}





get_emissions_opendataswiss <- function(apiurl) {

  csv <- find_emikat_opendataswiss(apiurl)
  csv <- csv[which.max(extract_year(csv))]
  data <- 
    readr::read_delim(csv, delim = ",") |> 
    dplyr::select(-einheit_lang) |> 
    dplyr::rename( # ... just for the sake of script language consistency
      year = jahr,
      pollutant = substanz,
      sector = hauptgruppe,
      subsector = untergruppe,
      canton = kanton,
      municipality = gemeinde, 
      unit = einheit
    ) |> 
    dplyr::mutate(
      pollutant = dplyr::case_when(pollutant == "BC" ~"eBC", TRUE ~ pollutant),
      source = "OSTLUFT"
    )
  
  return(data)
}



get_rsd_opendataswiss <- function(apiurl) {
  
  csv <- find_rsd_opendataswiss(apiurl)
  data <- lapply(csv, function(x) readr::read_delim(x, delim = ","))
  data <- dplyr::bind_rows(data)
  data <- dplyr::mutate(data, source = "Kanton Zürich/AWEL")
  
  return(data)
}




# get BFS inhabitant raster data and get, average and join air quality modelling raster data (semi-empirical, calibrated by monitoring data, source: BAFU/Meteotest)
#FIXME: see issue 11
get_prepare_raster_data <- function(files, boundary, path = "inst/extdata", wcs_version = "2.0.1") {

  # empty lists to be filled
  data <- list()
  capabilitylist <- list() 
  maplist <- list() # will contain available dataset 'layers' 
  
  # wcs connection to pollumap air quality raster data for the year 2015 (adopted from here: https://cran.r-project.org/web/packages/ows4R/vignettes/wcs.html) 
  print("get capabilities")
  
  client <- ows4R::WCSClient$new(files$rasterdata$bafu_airquality$pollumap, serviceVersion = wcs_version)
  capabilitylist$pollumap <- client$getCapabilities()
  maplist <- add_to_maplist(capabilitylist, maplist, "pollumap", NULL)
  maplist$pollumap <- maplist$pollumap[extract_year(maplist$pollumap) == 2015] # make sure, only pollumap 2015 is included
  
  # wcs connection to NO2-Jahreskarte air quality raster data on https://geolion.zh.ch for the years 2020ff
  client <- ows4R::WCSClient$new(files$rasterdata$bafu_airquality$jahreskarte$no2, serviceVersion = wcs_version)
  capabilitylist$jahreskarte$no2 <- client$getCapabilities()
  maplist <- add_to_maplist(capabilitylist, maplist, "jahreskarte", "no2")
  
  # wcs connection to O3p98-Jahreskarte air quality raster data on https://geolion.zh.ch for the years 2020ff
  client <- ows4R::WCSClient$new(files$rasterdata$bafu_airquality$jahreskarte$`o3_max_98p_m1`, serviceVersion = wcs_version)
  capabilitylist$jahreskarte$`o3_max_98p_m1` <- client$getCapabilities()
  maplist <- add_to_maplist(capabilitylist, maplist, "jahreskarte", "o3_max_98p_m1")

  # wcs connection to PM2.5-Jahreskarte air quality raster data on https://geolion.zh.ch for the years 2020ff
  client <- ows4R::WCSClient$new(files$rasterdata$bafu_airquality$jahreskarte$pm25, serviceVersion = wcs_version)
  capabilitylist$jahreskarte$pm25 <- client$getCapabilities()
  maplist <- add_to_maplist(capabilitylist, maplist, "jahreskarte", "pm25")

  # wcs connection to PM10-Jahreskarte air quality raster data on https://geolion.zh.ch for the years 2020ff
  client <- ows4R::WCSClient$new(files$rasterdata$bafu_airquality$jahreskarte$pm10, serviceVersion = wcs_version)
  capabilitylist$jahreskarte$pm10 <- client$getCapabilities()
  maplist <- add_to_maplist(capabilitylist, maplist, "jahreskarte", "pm10")

  # all maps/layers in one vector
  maps <- unlist(maplist)
  
  # population inhabitant data: download, read and restructure BFS population raster data from BFS homepage for the years in which air quality maps are available
  print("get BFS statpop raster data")
  
  data$population <- lapply(set_year(maps), function(year) get_bfs_statpop_rasterdata(year, path_destination = path, boundary = boundary))
  
  # air quality raster data can have different grids, even for same pollutant (various years)
  # since raster data are primarily used to derive inhabitant exposition (and not for air quality plotting - these plots can be found here: https://web.maps.zh.ch/), 
  # which are on the standard BFS 100 x 100m raster, pollutant grids are averaged to this 100 x 100m raster
  # BFS 100 x 100m grid is similar across the years => pick one grid for all years
  grid <- dplyr::select(data$population[[as.character(max(as.numeric(names(data$population))))]], RELI) 
  
  # download, read and restructure air pollution NO2, PM2.5, PM10, eBC, O3_max_98p_m1 raster data from geolion WCS
  print("get geolion air pollution raster data")
  
  print("... NO2")
  data$NO2 <- get_all_aq_rasterdata("NO2", maps, capabilitylist, grid, boundary)
  print("... PM2.5")
  data$PM2.5 <- get_all_aq_rasterdata("PM2.5", maps, capabilitylist, grid, boundary)
  print("... PM10")
  data$PM10 <- get_all_aq_rasterdata("PM10", maps, capabilitylist, grid, boundary)
  print("... eBC")
  data$eBC <- get_all_aq_rasterdata("eBC", maps, capabilitylist, grid, boundary)
  print("... O3_max_98p_m1")
  data$`O3_max_98p_m1` <- get_all_aq_rasterdata("O3_max_98p_m1", maps, capabilitylist, grid, boundary)
  
  # join air quality and population raster data
  print("join statpop and geolion raster data")
  
  pollutants <- names(data)[!(names(data) %in% c("population", "NH3", "Ndep", "Ndep_exceedance"))]
  data <- lapply(setNames(pollutants, pollutants), function(pollutant) join_raster_data_aq_bfs(pollutant, data))
  
  # download, read and restructure air pollution NH3, Ndep, Ndep_exceedance  raster data from https://data.geo.admin.ch
  print("get data.geo.admin reactive nitrogen raster data")
  
  print("... NH3")
  data$NH3 <- get_crop_all_bafu_rasterdata(files$rasterdata$bafu_nh3, boundary, path)
  print("... Ndep")
  data$Ndep <- get_crop_all_bafu_rasterdata(files$rasterdata$bafu_ndep, boundary, path)
  print("... Ndep-exmax")
  data$Ndep_exceedance <- get_crop_all_bafu_rasterdata(files$rasterdata$bafu_ndep_exc, boundary, path)
  
  return(data)
} 




