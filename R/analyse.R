aggregate_map <- function(map) {
  
  map <- 
    map %>%
    sf::st_union() %>%
    sf::st_boundary() %>% 
    sf::st_cast("POLYGON")
  
  return(map)
}





# make sure we have the same grid as BFS population data
aggregate_to_grid <- function(data, grid, parameter, boundary, method = "average", na_val = -999) { 
  
  data <- 
    data %>% 
    sf::st_crop(boundary) %>% 
    stars::st_warp(grid, method = method, use_gdal = TRUE, no_data_value = na_val)
  data <- setNames(data, parameter)
  
  return(data)
}



aggregate_population_weighted_mean <- function(data, y, group = "geodb_oid") {
  
  data <- 
    data %>% 
    sf::st_drop_geometry() %>%
    dplyr::mutate(!!y := ifelse(art_code != 1, NA, !!rlang::sym(y))) %>%
    na.omit() %>%
    dplyr::group_by(!!rlang::sym(group)) %>% 
    dplyr::summarise(!!y := population_weighted_mean(!!rlang::sym(y), population)) %>% 
    dplyr::ungroup()
  
  return(data)
}




aggregate_exposition_distrib <- function(data, y, fun = function(x) {floor(x) + 0.5}) { # fun: abgerundet auf 1, Klassenmitte
  
  data <- 
    data %>% 
    dplyr::select(!!y, population) %>% 
    tibble::as_tibble() %>% 
    na.omit() %>% 
    dplyr::group_by(!!rlang::sym(y) := fun(!!rlang::sym(y))) %>% 
    dplyr::summarise(population = sum(population)) %>%
    dplyr::ungroup()
  
  return(data)
}






exposition_distrib_cumulative <- function(data, y) {
  
  data <- 
    data %>% 
    dplyr::filter(population > 0) %>% 
    dplyr::arrange(!!y) %>% 
    dplyr::mutate(population_cum_relative = cumsum(population) / sum(population))
  
  return(data)
}




# ... only a valid approximation for vehicles weighting less than 3.5 t
calc_vsp <- function(speed, accel, slope, # speed in m/s, accel in m/s/s, slope as ratio, mass = 3.5 in t
                     vsp.a = 1.1, vsp.b = 0.132, vsp.c = 0.000302, vsp.g = 9.81) {
  
  vsp <- speed * (vsp.a * accel + (vsp.g * slope) + vsp.b) + (vsp.c * speed^3)
  
  return(vsp)
}




calc_rsd_nox_emission <- function(NO, p, CO2, CO, HC) { # all concentrations in mixing ratios as percent
  
  Q <- CO / CO2
  Q1 <- HC / CO2
  Q2 <- NO / CO2
  NO_emission <- 30 * Q2 * 860 / ((1 + Q + 6 * Q1) * 12)
  NOx_emission <- NO_emission * 46 / (30 * (1 - p))
  
  return(NOx_emission)
}




# aggregate RSD data calculating: n, percentiles, median, mean, standard deviation, standard error
aggregate_groups_rsd <- function(data, y, groups = c("vehicle_type", "vehicle_fuel_type", "vehicle_euronorm"),
                                 nmin = 100, perc = list(ymin = 0.05, lower = 0.25, middle = 0.5, upper = 0.75, ymax = 0.95)) {
  
  data <-
    data %>%
    dplyr::group_by_at(dplyr::vars(groups)) %>%
    dplyr::summarise(
      n = length(na.omit(!!rlang::sym(y))),
      min = quantile(!!rlang::sym(y), perc$ymin, na.rm = TRUE),
      lower = quantile(!!rlang::sym(y), perc$lower, na.rm = TRUE),
      middle = quantile(!!rlang::sym(y), perc$middle, na.rm = TRUE),
      upper = quantile(!!rlang::sym(y), perc$upper, na.rm = TRUE),
      max = quantile(!!rlang::sym(y), perc$ymax, na.rm = TRUE),
      mean = mean(!!rlang::sym(y), na.rm = TRUE),
      standarddeviation = sd(!!rlang::sym(y), na.rm = TRUE),
      standarderror = standarddeviation / sqrt(n)
    ) %>%
    ungroup()
  
  data_all <-
    data %>% 
    dplyr::select(tidyr::all_of(groups)) %>% 
    dplyr::distinct_all() %>% 
    tidyr::expand(tidyr::crossing(!!!rlang::syms(groups)))
  
  data <- dplyr::left_join(data_all, data, by = groups) 
  data <- dplyr::mutate_at(data, c("min", "lower", "middle", "upper", "max", "mean", "standarddeviation", "standarderror"), list(~ifelse(n < nmin, NA, .)))
  data <- dplyr::mutate_at(data, c("min", "lower", "middle", "upper", "max", "mean", "standarddeviation", "standarderror"), list(~ifelse(is.nan(.), NA, .)))
  data <- dplyr::mutate_at(data, c("min", "lower", "middle", "upper", "max", "mean", "standarddeviation", "standarderror"), list(~ifelse(is.infinite(.), NA, .)))
  data$n <- ifelse(is.na(data$n), 0, data$n)
  
  return(data)
}



aggregate_nox_rsd <- function(data, meta, nmin = 50, groups = c("vehicle_type", "vehicle_fuel_type", "vehicle_euronorm")) {
  
  data <- 
    data %>% 
    aggregate_groups_rsd(y = "NOx_emission", groups = groups, nmin = nmin) %>% 
    dplyr::rename(NOx_emission = mean) %>% 
    dplyr::mutate(
      unit = "g/kg fuel",
      source = "Kanton Zürich/AWEL"
    )
  
  if(is.null(meta)) {
    data <- dplyr::select(data, !!c(groups, "NOx_emission", "unit", "n", "standarderror", "source"))
  } else {
    data <- 
      data %>% 
      dplyr::left_join(meta, by = groups) %>% 
      dplyr::select(!!c(groups, "NOx_emission", "unit", "n", "standarderror", "NOx_emission_threshold_g_per_kg_fuel", "source"))
  } 
  
  return(data)
}





population_weighted_mean <- function(concentration, population) {sum(concentration * population, na.rm = TRUE) / sum(population, na.rm = TRUE)}



# see here: https://gist.github.com/sotoattanito/8e6fad4b7322ceae9f14f342985f1681
round_off <- function (x, digits = 0) {
  
  posneg = sign(x)
  z = trunc(abs(x) * 10 ^ (digits + 1)) / 10
  z = floor(z * posneg + 0.5) / 10 ^ digits
  
  return(z)
}





aggregate_nitrogen_deposition <- function(data) {

  data <-
    data %>%
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
    ) %>% 
    dplyr::rename(
      site_long = site,
      site = site_short
    )
  
  estimate <- 
    data %>% 
    dplyr::filter(parameter == "N-Deposition") %>% 
    dplyr::select(year, site, ecosystem_category, estimate)
  
  data <- 
    data %>% 
    dplyr::group_by(year, site, site_long, source, siteclass, ecosystem_category, critical_load_min, critical_load_single, critical_load_max, parameter, unit) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    left_join(estimate, by = c("year", "site", "ecosystem_category")) %>% 
    dplyr::mutate(estimate = dplyr::case_when(parameter == "N-Deposition" ~ estimate, TRUE ~ NA))
  
  return(data)
  
}



aggregate_emissions <- function(data, fun = sum, groups = c("year", "pollutant", "unit", "sector", "subsector")) {
  
  data <-
    data %>% 
    dplyr::group_by_at(dplyr::vars(groups)) %>%
    dplyr::summarise(emission = fun(emission)) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(emission > 0)
  
  return(data)
}



groups_emission_subsector <- function(data, threshold_fraction = 0.03, index = 1:3) {
  
  # mean emissions per category over all years
  
  groups <- aggregate_emissions(data, mean, c("pollutant", "sector", "subsector"))
  
  # recode subsectors with mean emissions less than the "big x" into subsector "sonstige"
  
  groups <- 
    groups %>% 
    dplyr::group_by(pollutant, sector) %>% 
    dplyr::mutate(
      subsector_new = dplyr::case_when(
        emission < min(sort(emission, decreasing = TRUE)[index]) ~ "diverse",
        TRUE ~ subsector
      )
    ) 
  
  # aggregate emissions accordingly

  groups2 <- aggregate_emissions(groups, sum, c("pollutant", "sector", "subsector_new"))
  
  # second iteration: if subsector emission < overall emission * threshold fraction, then also recode into "sonstige"
  
  groups2 <- 
    groups2 %>% 
    dplyr::group_by(pollutant) %>% 
    dplyr::mutate(
      subsector_new = dplyr::case_when(
        subsector_new != "diverse" & emission < threshold_fraction * sum(emission)  ~ "diverse",
        TRUE ~ subsector_new
      ),
      subsector_new = paste0(sector, " / ", subsector_new)
    ) %>% 
    dplyr::ungroup()
  
  # aggregate accordingly
  
  groups2 <- aggregate_emissions(groups2, sum, c("pollutant", "subsector_new"))
  
  # restructure and prepare as lookup table for use outside this function
  
  groups2 <- dplyr::filter(groups2, !stringr::str_detect(subsector_new, "diverse"))
  
  groups <-
    groups %>% 
    dplyr::select(-emission) %>% 
    dplyr::mutate(subsector_new = paste0(sector, " / ", subsector_new)) %>% 
    dplyr::left_join(groups2, by = c("pollutant", "subsector_new")) %>% 
    dplyr::arrange(pollutant, sector, dplyr::desc(emission)) %>%
    dplyr::mutate(
      subsector_new = dplyr::case_when(is.na(emission) ~ paste0(sector, " / diverse"), TRUE ~ subsector_new),
      subsector_new = factor(subsector_new, levels = unique(.data$subsector_new))
      ) %>% 
    dplyr::select(-emission)
  
  return(groups)
}





join_raster_data_aq_bfs <- function(pollutant, data_raster){
  
  data <- 
    setNames(names(data_raster[[pollutant]]), extract_year(names(data_raster[[pollutant]]))) %>%
    lapply(function(year) {sf::st_join(data_raster[[pollutant]][[year]], data_raster$population[[as.character(extract_year(year))]])})
  
  return(data)
}





join_raster_data_with_municipalities <- function(pollutant, data_raster, boundaries){
  
  data <- 
    setNames(names(data_raster[[pollutant]]), extract_year(names(data_raster[[pollutant]]))) %>%
    lapply(function(year) {sf::st_join(boundaries, sf::st_as_sf(data_raster[[pollutant]][[year]]))})
  
  return(data)
}




aggregate_population_weighted_mean_boundaries <- function(pollutant, data_expo, boundaries){
  
  municipalities <- aggregate_population_weighted_mean(data_expo, y = pollutant)
  municipalities <- dplyr::left_join(boundaries, municipalities, by = "geodb_oid")
  canton <- round_off(population_weighted_mean(data_expo[[pollutant]], data_expo$population), 1)
  weighted_means <- list(
    canton = canton, 
    municipalities = municipalities
  )
  
  return(weighted_means)
}



calc_all_population_weighted_means <- function(pollutant, data_expo, boundaries){
  
  weighted_means <- 
    setNames(names(data_expo), extract_year(names(data_expo))) %>% 
    lapply(function(year) aggregate_population_weighted_mean_boundaries(pollutant, data_expo[[year]], boundaries))
    
  return(weighted_means)
}





calc_all_population_expo_distr <- function(pollutant, data_raster){
  
  expo_distr <- 
    setNames(names(data_raster), extract_year(names(data_raster))) %>% 
    lapply(function(year) {
      data <- aggregate_exposition_distrib(data_raster[[year]], y = pollutant, fun = bin_fun(pollutant)) 
      exposition_distrib_cumulative(data, y = pollutant)
    })
  
  return(expo_distr)
}



calc_ndep_ecosystem_expo_distr <- function(data_raster, year) {
  
  data_expo <-
    data_raster[[year]] %>% 
    dplyr::select(EXNMAX) %>% 
    tibble::as_tibble() %>% 
    na.omit() %>% 
    dplyr::group_by(EXNMAX = floor(EXNMAX) + 0.5) %>% # abgerundet auf 1, Klassenmitte
    dplyr::summarise(n_ecosys = dplyr::n()) %>%
    dplyr::ungroup()
  
  return(data_expo)
}



calc_ndep_ecosystem_expo_distr_cumulative <- function(data_expo) {
  
  data_expo <-
    data_expo %>% 
    dplyr::arrange(EXNMAX) %>% 
    dplyr::mutate(n_ecosys_cum_relative = cumsum(n_ecosys) / sum(n_ecosys))
  
  return(data_expo)
}



calc_all_ndep_ecosystem_expo_distr <- function(data_raster){

  expo_distr <-
    setNames(names(data_raster), extract_year(names(data_raster))) %>% 
    lapply(function(year) {
      data <- calc_ndep_ecosystem_expo_distr(data_raster, year)
      calc_ndep_ecosystem_expo_distr_cumulative(data)
    })
  
  return(expo_distr)
}


