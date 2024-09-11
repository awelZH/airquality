



# prep Emissionskataster
filter_emissions <- function(data, filters = "canton == 'ZH' & emission != 0 & !(subsector %in% c('Weitere Punktquellen OL', 'Rheinschifffahrt', 'Flugverkehr Genf'))") { # exclude some groups that might be redundant due to area distribution methodology
  return(dplyr::filter(data, eval(rlang::parse_expr(filters))))
}


# prep rsd metadata
restructure_rsd_meta <- function(meta) {
  
  meta <- 
    meta |> 
    dplyr::select(-source, -remark) |> 
    tidyr::spread(parameter, value) |> 
    dplyr::mutate(
      vehicle_type = factor(vehicle_type, levels = c("passenger car", "light duty vehicle")),
      vehicle_fuel_type = factor(vehicle_fuel_type, levels = c("gasoline", "diesel"))
    ) |> 
    dplyr::rename(NOx_emission_threshold_g_per_kg_fuel = `nox_emission_threshold_g_per_kg Treibstoff`)
  
  return(meta)
}


restructure_rsd_for_vsp <- function(data) {
  
  data <-
    data |> 
    dplyr::filter(parameter %in% c("acceleration", "velocity") & !is.na(value)) |>
    dplyr::select(id, site_roadgrade, parameter, value) |>
    tidyr::spread(parameter, value)
 
  return(data) 
}


filter_rsd <- function(data, filters) {

  data <- 
    data |> 
    dplyr::filter(
      vehicle_model_year %in% filters$min[filters$parameter == "vehicleyears"]:filters$max[filters$parameter == "vehicleyears"] &
        (acceleration >= filters$min[filters$parameter == "accelerationrange"] & acceleration <= filters$max[filters$parameter == "accelerationrange"]) &
        (velocity >= filters$min[filters$parameter == "velocityrange"] & velocity <= filters$max[filters$parameter == "velocityrange"]) &
        (vehicle_specific_power >= filters$min[filters$parameter == "vsprange"] & vehicle_specific_power <= filters$max[filters$parameter == "vsprange"]) &
        vehicle_unloaded_weight <= filters$max[rsd_filters$parameter == "weightmax"] &
        !is.na(value)
    ) |>
    tidyr::spread(parameter, value) |>
    dplyr::filter(!is.na(NO + CO2 + CO + HC)) # all concentrations are nessecary for NOx emission calculation
  
  return(data)
}


merge_restructure_rsd <- function(data, meta) {
  
  data <- 
    data |> 
    dplyr::mutate(vehicle_euronorm = dplyr::recode(vehicle_euronorm, !!!c("Euro5a" = "Euro5", "Euro5b" = "Euro5"))) |> # merge both sub-Euro5 norms since they are quite similar
    dplyr::left_join(dplyr::filter(meta, is.na(as.numeric(vehicle_euronorm))), by = c("vehicle_type", "vehicle_fuel_type", "vehicle_euronorm")) |> 
    dplyr::mutate(
      vehicle_type = factor(vehicle_type, levels = c("passenger car", "light duty vehicle")),
      vehicle_fuel_type = factor(vehicle_fuel_type, levels = c("gasoline", "diesel"))
    )
  
  return(data)
}



# function to make sure that there are no duplicate measurements per site / year / unit for data with interval = y1 in format rOstluft::format_rolf() 
# in case there have been NO2 monitor and passive sampler measurements (prefer monitor data = reference method); 
# same for PM10 monitor and high volume sampler measurements (prefer high-volume-sampler data = reference method);
# same for PM2.5 monitor and high volume sampler measurements (prefer high-volume-sampler data = reference method)
remove_duplicate_y1 <- function(data){
  
  # FIXME: funktion welche den Parameter als input hat ;)
  # FIXME: etwas stimmt hier noch nicht, was ist parameter?
  replace_no2_ps <- function(parameter, value){
    if (sum(c("NO2", "NO2_PS") %in% parameter) == 2) {
      if (!is.na(value[which(parameter == "NO2")])){
        value[which(parameter == "NO2_PS")] <- NA
      }
    }
    return(value)
  }
  
  replace_pm10 <- function(parameter, value){
    if (sum(c("PM10", "PM10h") %in% parameter) == 2) {
      if (!is.na(value[which(parameter == "PM10h")])){
        value[which(parameter == "PM10")] <- NA
      }
    }
    return(value)
  }
  
  replace_pm25 <- function(parameter, value){
    if (sum(c("PM2.5", "PM2.5h") %in% parameter) == 2) {
      if (!is.na(value[which(parameter == "PM2.5h")])){
        value[which(parameter == "PM2.5")] <- NA
      }
    }
    return(value)
  }
  
  data <- 
    data |> 
    dplyr::group_by(starttime, site, unit) |> 
    dplyr::mutate(
      value = replace_no2_ps(parameter, value),
      value = replace_pm10(parameter, value),
      value = replace_pm25(parameter, value)
    ) |> 
    dplyr::ungroup() |>
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(parameter = dplyr::recode_factor(parameter, !!!c("NO2_PS" = "NO2", "PM10h" = "PM10", "PM2.5h" = "PM2.5")))
  
  return(data)
}





# copy from rOstluft::convert_interval()
convert_interval2 <- function(interval) {
  
  num <- stringr::str_extract(interval, "[:digit:]+")
  units <- stringr::str_extract(interval, "[:alpha:]+")
  units <- stringr::str_to_lower(units)
  if (is.na(num)) num <- "1"
  if (units == "m") units <- "month"
  if (units == "y") units <- "year"
  
  stringr::str_c(num, units, sep = " ")
}



# copy from rOstluft::pad_serie()
pad_serie2 <- function(serie, start_date = NULL, end_date = NULL, drop_last = FALSE) {
  
  if (is.null(start_date)) {
    start_date <- min(serie$starttime)
  }
  
  if (is.null(end_date)) {
    end_date <- max(serie$starttime)
    drop_last <- FALSE
  }
  
  # by joining the data we insert rows with NA values for site, parameter, interval, unit, value
  # we need to fill this with the values from the supplied df
  fill.values <- dplyr::slice(serie, 1)
  fill.values <- as.list(dplyr::select(fill.values, -"starttime", -"value"))
  
  interval <- convert_interval2(fill.values$interval)
  
  all.dates <- tibble::tibble(
    starttime = seq(start_date, end_date, interval)
  )
  
  if (isTRUE(drop_last)) {
    all.dates <- utils::head(all.dates, -1)
  }
  
  padded <- dplyr::full_join(all.dates, serie, by = "starttime")
  tidyr::replace_na(padded, replace = fill.values)
}



# copy from rOstluft::pad() => because this is the only function we need from this package
pad2 <- function(data, start_date = NULL, end_date = NULL, drop_last = FALSE) {
  
  data.grouped <- dplyr::group_by(data, .data$site, .data$parameter, .data$interval, .data$unit)
  data.grouped <- dplyr::do(data.grouped, pad_serie2(.data, start_date, end_date, drop_last))
  
  return(dplyr::ungroup(data.grouped))
}


combine_thresholds <- function(data, threshold_values) {
  
  data <- 
    threshold_values |> 
    dplyr::select(source, pollutant, metric, aggregation, threshold) |> 
    dplyr::rename(
      parameter = pollutant,
      interval = aggregation
    ) |> 
    dplyr::mutate(
      parameter = dplyr::case_when(
        metric == "number hourly mean values > 120 µg/m3" & parameter == "O3" ~ "O3_nb_h1>120",
        metric == "monthly 98%-percentile of ½ hour mean values ≤ 100 µg/m3" & parameter == "O3" ~ "O3_max_98p_m1",
        metric == "mean of daily maximum 8-hour mean concentration in the six consecutive months with the highest six-month running-mean concentration" & parameter == "O3" ~ "O3_peakseason_mean_d1_max_mean_h8gl",
        TRUE ~ parameter
      ),
      interval = dplyr::recode(interval, !!!c("m1" = "y1", "peak-season" = "y1"))
    ) |> 
    dplyr::select(-metric) |> 
    tidyr::spread(source, threshold) |> 
    dplyr::right_join(data, by = c("parameter", "interval")) |> 
    dplyr::select(year, site, parameter, interval, unit, value, siteclass, `LRV Grenzwert`, `WHO Richtwert`, source)
  
  return(data)
  
}




extract_weighted_mean_canton <- function(data_expo, pollutant) {

  lapply(names(data_expo), function(year) {
    tibble(
      year = as.numeric(year),
      admin_boundary = "Canton Zürich",
      parameter = pollutant,
      unit = "μg/m3",
      pop_weighted_mean = data_expo[[as.character(year)]][["canton"]],
      source = "OSTLUFT, BAFU, BFS"
    )
  }) |> 
    dplyr::bind_rows()
}




extract_weighted_mean_municipalities <- function(data_expo, pollutant) {
  
  lapply(names(data_expo), function(year) {
    data <- sf::st_drop_geometry(data_expo[[as.character(year)]][["municipalities"]])
    tibble(
      year = as.numeric(year),
      geodb_oid = dplyr::pull(data, "geodb_oid"),
      bfs = dplyr::pull(data, "bfs"),
      municipality = dplyr::pull(data, "gemeindename"),
      admin_boundary = "municipality",
      parameter = pollutant,
      unit = "μg/m3",
      pop_weighted_mean = dplyr::pull(data, pollutant),
      source = "OSTLUFT, BAFU, BFS"
    )
  }) |> 
    dplyr::bind_rows()
}




extract_exposition_distr_pollutants <- function(data_expo, pollutant) {
  
  lapply(names(data_expo), function(year) {
    data_expo[[as.character(year)]] |> 
      dplyr::rename(concentration = !!pollutant) |> 
      dplyr::mutate(
        year = as.numeric(year),
        parameter = pollutant,
        source = "OSTLUFT, BAFU, BFS"
        )
  }) |> 
    dplyr::bind_rows()
}




extract_exposition_distr_ndep <- function(data_expo) {
  
  lapply(names(data_expo), function(year) {
    data_expo[[as.character(year)]] |> 
      dplyr::mutate(
        year = as.numeric(year),
        parameter = "max Ndep > CLO",
        source = "BAFU"
      )
  }) |> 
    dplyr::bind_rows()
}





