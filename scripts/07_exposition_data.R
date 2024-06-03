
### air pollution inhabitant / sensitive ecosystem exposition
### ------------------------------------------------------------

data_expo <- list()

### calculate population-weighted mean values per municipality (and for canton)

### ... for NO2



# FIXME: wrapper funktion!
# wie bei airquality-population
data_expo$NO2$population_weighted_mean <- 
  lapply(setNames(names(data_raster$NO2), names(data_raster$NO2)), function(year) {
    data <- sf::st_join(boundaries, sf::st_as_sf(data_raster$NO2[[year]]))
    canton <- round_off(population_weighted_mean(data$NO2, data$population), 1)
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
    canton <- round_off(population_weighted_mean(data$PM2.5, data$population), 1)
    municipalities <- aggregate_population_weighted_mean(data, y = "PM2.5")
    municipalities <- dplyr::left_join(boundaries, municipalities, by = "geodb_oid")
    return(list(canton = canton, municipalities = municipalities))
  })

### ... for eBC

data_expo$eBC$population_weighted_mean <- 
  lapply(setNames(names(data_raster$eBC), names(data_raster$eBC)), function(year) {
    data <- sf::st_join(boundaries, sf::st_as_sf(data_raster$eBC[[year]]))
    canton <- round_off(population_weighted_mean(data$eBC, data$population), 1)
    municipalities <- aggregate_population_weighted_mean(data, y = "eBC")
    municipalities <- dplyr::left_join(boundaries, municipalities, by = "geodb_oid")
    return(list(canton = canton, municipalities = municipalities))
  })

### ... for O3p98

# ...

### calculate exposition distribution (absolute and relative/cumlulative)

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

