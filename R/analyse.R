
### make sure we have the same grid as BFS population data
aggregate_to_grid <- function(data, grid, parameter, boundary, method = "average", na_val = -999) { 
  
  data <- 
    data %>% 
    sf::st_crop(boundary) %>% 
    stars::st_warp(grid, method = method, use_gdal = TRUE, no_data_value = na_val)
  data <- setNames(data, parameter)
  
  return(data)
}




### ... to be roughly in line with https://www.bafu.admin.ch/bafu/de/home/themen/luft/publikationen-studien/publikationen/immissionsmessung-von-luftfremdstoffen.html
### however, the OSTLUFT site classes are - as categories - not entirely consistent with the new Immissionsmessempfehlung. We will need to put future effort in a reclassifiacation

# FIXME: hier machst du gar keine aggregation ;) nur ein recoding
aggregate_ostluft_meta_zone <- function(zone) { 
  
  zone <- 
    dplyr::case_when(
      as.numeric(stringr::str_remove(zone, "H")) %in% c(21:23, 31:33) ~ "st\u00e4dtisch", # OSTLUFT: > 20'000 Gesamteinwohner; BAFU: > 1500 Einwohner/km2 und Gesamteinwohnerzahl > 50 000
      as.numeric(stringr::str_remove(zone, "H")) %in% 11:13 ~ "klein-/vorst\u00e4dtisch", # OSTLUFT: > 1'000 Gesamteinwohner; BAFU: > 300 Einwohner/km2 im \u00fcberbauten Gebiet und Gesamteinwohnerzahl > 5000
      as.numeric(stringr::str_remove(zone, "H")) == 0 ~ "l\u00e4ndlich", # OSTLUFT: < 1'000 Gesamteinwohner; BAFU: Gebiete mit geringer Siedlungsdichte (< 300 Einwohner/km2) oder kleinere Ortschaften (< 5000 Einwohner)
      TRUE ~ zone 
    )
  
  return(zone)
}




### ... to be roughly in line with https://www.bafu.admin.ch/bafu/de/home/themen/luft/publikationen-studien/publikationen/immissionsmessung-von-luftfremdstoffen.html
### however, the OSTLUFT site classes are - as categories - not entirely consistent with the new Immissionsmessempfehlung. We will need to put future effort in a reclassifiacation
aggregate_ostluft_meta_type <- function(type) { 
  
  type <- 
    dplyr::case_when(
      as.numeric(stringr::str_remove(type, "S")) %in% c(10:13, 20:23, 30:33) ~ "verkehrsbelastet", # OSTLUFT: DTV_S > 10'000; BAFU: has a finer scale that begins at DTV > 3'000 and cerctain max distance to street 
      as.numeric(stringr::str_remove(type, "S")) == 0 ~ "Hintergrund", # OSTLUFT: DTV_S < 10'000 & street more than 50m (in cities) or 300m (outside of cities) away; BAFU: see above
      TRUE ~ type 
    )
  
  return(type)
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
    dplyr::mutate(population_relative = cumsum(population) / sum(population))
  
  return(data)
}




### ... only a valid approximation for vehicles weighting less than 3.5 t
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




### aggregate RSD data calculating: n, percentiles, median, mean, standard deviation, standard error
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



population_weighted_mean <- function(concentration, population) {sum(concentration * population, na.rm = TRUE) / sum(population, na.rm = TRUE)}



### see here: https://gist.github.com/sotoattanito/8e6fad4b7322ceae9f14f342985f1681
round_off <- function (x, digits = 0) {
  
  posneg = sign(x)
  z = trunc(abs(x) * 10 ^ (digits + 1)) / 10
  z = floor(z * posneg + 0.5) / 10 ^ digits
  
  return(z)
}




