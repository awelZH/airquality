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
