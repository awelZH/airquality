merge_restructure_rsd <- function(data, meta) {
  
  meta <- 
    meta |> 
    dplyr::filter(is.na(as.numeric(vehicle_euronorm))) |> 
    dplyr::select(-source, -remark) |> 
    tidyr::spread(parameter, value)
  
  data <- 
    data |> 
    dplyr::mutate(vehicle_euronorm = dplyr::recode(vehicle_euronorm, !!!c("Euro5a" = "Euro5", "Euro5b" = "Euro5"))) |> # merge both sub-Euro5 norms since they are quite similar
    dplyr::left_join(meta, by = c("vehicle_type", "vehicle_fuel_type", "vehicle_euronorm")) |> 
    dplyr::mutate(
      vehicle_type = factor(vehicle_type, levels = c("passenger car", "light duty vehicle")),
      vehicle_fuel_type = factor(vehicle_fuel_type, levels = c("gasoline", "diesel"))
    )
  
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
        vehicle_unloaded_weight <= filters$max[filters$parameter == "weightmax"] &
        !is.na(value)
    ) |>
    tidyr::spread(parameter, value) |>
    dplyr::filter(!is.na(NO + CO2 + CO + HC)) # all concentrations are nessecary for NOx emission calculation
  
  return(data)
}


prep_vehicle_specific_power <- function(data){
  
  data_vsp <-
    data |>
    dplyr::filter(parameter %in% c("acceleration", "velocity") & !is.na(value)) |>
    dplyr::select(id, site_roadgrade, parameter, value) |>
    tidyr::spread(parameter, value) |> 
    dplyr::mutate(vehicle_specific_power = calc_vsp(velocity * 1000 / 60^2, acceleration * 1000 / 60^2, site_roadgrade)) |>  # also convert velocity from km/h into m/s and acceleration from km/h/s into m/s2
    dplyr::select(id, acceleration, velocity, vehicle_specific_power) # vehicle_specific_power in kW/t
  
  return(data_vsp)
}


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


restructure_monitoring_nabel <- function(data, keep_incomplete = FALSE) {
  
  col_names <- range(as.numeric(names(data)), na.rm = TRUE)
  data <- dplyr::mutate_if(data, is.numeric, as.character)
  data_long <- tidyr::pivot_longer(
    data, 
    cols = as.character(min(col_names):max(col_names)),
    names_to = "starttime"
  )
  data_long_clean <- 
    data_long |> 
    dplyr::mutate(
      value = dplyr::case_when(
        keep_incomplete ~ as.numeric(gsub("\\*|\\;", "", value)),
        TRUE ~ as.numeric(value)
      )
    )
  
  return(data_long_clean) 
}


restructure_monitoring_ostluft <- function(data, keep_incomplete = FALSE, tz = "Etc/GMT-1", na.rm = TRUE) {

  header <- dplyr::slice(data, 1:which(dplyr::pull(data, 1) == "Startzeit"))
  header <- dplyr::select(header, -1)
  data <- dplyr::slice(data, (which(dplyr::pull(data, 1) == "Startzeit") + 1):nrow(data))
  colnames(data)[1] <- "starttime"
  data <- dplyr::mutate(data, starttime = lubridate::parse_date_time(.data$starttime, c("dmYHMS", "dmYHM", "dmY"), tz = tz))

  col_ids <- rlang::names2(data)[-1]
  # FIXME: kann das vereinfacht werden mit pivot_longer? sollte eigentlich möglich sein
  sites <- c(header[1, ], recursive = TRUE)
  sites <- rlang::set_names(sites, col_ids)
  parameters <- c(header[which(dplyr::pull(header,1) %in% c("NO2","NO2_PS","O3","PM10","PM10h","PM2.5","PM2.5h")),], recursive = TRUE) #! ... Liste vervollständigen
  parameters <- rlang::set_names(parameters, col_ids)
  intervals <- c(header[which(dplyr::pull(header,1) %in% c("y1","m1","d1","h1","min30","min10")),], recursive = TRUE)
  intervals <- rlang::set_names(intervals, col_ids)
  units <- c(header[which(dplyr::pull(header,1) %in% c("µg/m3","ppb","ppm","ppt","°C","hPa","%","W/m2")),], recursive = TRUE) #! ... Liste vervollständigen
  units <- rlang::set_names(units, col_ids)

  
  data_long <- tidyr::gather(data, "id", "value", -"starttime", na.rm = na.rm, factor_key = TRUE)
  data_long <- dplyr::mutate(data_long,
                        site = dplyr::recode(.data$id, !!!sites),
                        parameter = dplyr::recode(.data$id, !!!parameters),
                        interval = dplyr::recode(.data$id, !!!intervals),
                        unit = dplyr::recode(.data$id, !!!units)
  )
  data_long <- dplyr::select(data_long, "starttime", "site", "parameter", "interval", "unit", "value")
  
  data_long_clean <- 
    data_long |> 
    dplyr::mutate(
        value = dplyr::case_when(
          keep_incomplete ~ as.numeric(gsub("\\*|\\;", "", value)),
          TRUE ~ as.numeric(value)
        )
    ) |> 
    dplyr::mutate_if(is.character, factor) 
  
  return(data_long_clean) 
}


# function to make sure that there are no duplicate measurements per site / year / unit for data with interval = y1 in format rOstluft::format_rolf() 
# in case there have been NO2 monitor and passive sampler measurements (prefer monitor data = reference method); 
# same for PM10 monitor and high volume sampler measurements (prefer high-volume-sampler data = reference method);
# same for PM2.5 monitor and high volume sampler measurements (prefer high-volume-sampler data = reference method)
remove_duplicate_y1 <- function(data){
  
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


prep_site_meta_ostluft <- function(meta) {
  
  meta <- 
    meta |> 
    dplyr::filter(msKT == "ZH" & !is.na(msNameAirMo) & !is.na(scSiedlungsgroesse) & !is.na(scVerkehrslage)) |>
    dplyr::mutate(
      site_long = paste(msOrt, msOrtsteil, sep = " - "),
      scSiedlungsgroesse = stringr::str_trim(scSiedlungsgroesse),
      scVerkehrslage = stringr::str_trim(scVerkehrslage)
    ) |>
    dplyr::select(msNameAirMo, site_long, spXCoord, spYCoord, spHoehe, scSiedlungsgroesse, scVerkehrslage) |> 
    dplyr::rename(
      site = msNameAirMo,
      x = spXCoord,
      y = spYCoord,
      masl = spHoehe,
      zone = scSiedlungsgroesse, 
      type = scVerkehrslage
    ) |> 
    dplyr::mutate(
      zone = recode_ostluft_meta_zone(zone),
      type = recode_ostluft_meta_type(type),
      source = "Ostluft"
    ) 
  
  return(meta)
}


prep_site_meta_nabel <- function(meta) {

    meta <- dplyr::distinct(meta, Station, `Ost Y`, `Nord X`, Höhe, Zonentyp, Stationstyp)
    meta <- dplyr::mutate(meta, Zonentyp = tolower(Zonentyp))
    meta <- dplyr::rename(meta, 
                          site = Station,
                          y = `Ost Y`,
                          x = `Nord X`,
                          masl = Höhe,
                          zone = Zonentyp,
                          type = Stationstyp
    )
    meta <- dplyr::mutate(meta,
                          ifelse(zone == "vorstädtisch", "klein-/vorstädtisch", zone),
                          site_long = site,
                          source = "NABEL (BAFU & Empa)"
    )
    meta <- dplyr::select(meta, site, site_long, x, y, masl, zone, type, source)
  
  return(meta)
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

