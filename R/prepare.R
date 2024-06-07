

# prep Emissionskataster
filter_emissions <- function(data, filters = "canton == 'ZH' & emission != 0 & !(subsector %in% c('Weitere Punktquellen OL', 'Rheinschifffahrt', 'Flugverkehr Genf'))") { # exclude some groups that might be redundant due to area distribution methodology
  dplyr::filter(data, eval(rlang::parse_expr(filters)))
}


# prep rsd metadata
restructure_rsd_meta <- function(meta) {
  
  meta <- 
    meta %>% 
    dplyr::select(-source, -remark) %>% 
    tidyr::spread(parameter, value) %>% 
    dplyr::mutate(
      vehicle_type = factor(vehicle_type, levels = c("passenger car", "light duty vehicle")),
      vehicle_fuel_type = factor(vehicle_fuel_type, levels = c("gasoline", "diesel"))
    ) %>% 
    dplyr::rename(NOx_emission_threshold_g_per_kg_fuel = `nox_emission_threshold_g_per_kg Treibstoff`)
  
  return(meta)
}


restructure_rsd_for_vsp <- function(data) {
  
  data <-
    data %>% 
    dplyr::filter(parameter %in% c("acceleration", "velocity") & !is.na(value)) %>%
    dplyr::select(id, site_roadgrade, parameter, value) %>%
    tidyr::spread(parameter, value)
 
  return(data) 
}


filter_rsd <- function(data, filters) {

  data <- 
    data %>% 
    dplyr::filter(
      vehicle_model_year %in% filters$min[filters$parameter == "vehicleyears"]:filters$max[filters$parameter == "vehicleyears"] &
        (acceleration >= filters$min[filters$parameter == "accelerationrange"] & acceleration <= filters$max[filters$parameter == "accelerationrange"]) &
        (velocity >= filters$min[filters$parameter == "velocityrange"] & velocity <= filters$max[filters$parameter == "velocityrange"]) &
        (vehicle_specific_power >= filters$min[filters$parameter == "vsprange"] & vehicle_specific_power <= filters$max[filters$parameter == "vsprange"]) &
        vehicle_unloaded_weight <= filters$max[rsd_filters$parameter == "weightmax"] &
        !is.na(value)
    ) %>%
    tidyr::spread(parameter, value) %>%
    dplyr::filter(!is.na(NO + CO2 + CO + HC)) # all concentrations are nessecary for NOx emission calculation
  
  return(data)
}


merge_restructure_rsd <- function(data, meta) {
  
  data <- 
    data %>% 
    dplyr::mutate(vehicle_euronorm = dplyr::recode(vehicle_euronorm, !!!c("Euro5a" = "Euro5", "Euro5b" = "Euro5"))) %>% # merge both sub-Euro5 norms since they are quite similar
    dplyr::left_join(dplyr::filter(meta, is.na(as.numeric(vehicle_euronorm))), by = c("vehicle_type", "vehicle_fuel_type", "vehicle_euronorm")) %>% 
    dplyr::mutate(
      vehicle_type = factor(vehicle_type, levels = c("passenger car", "light duty vehicle")),
      vehicle_fuel_type = factor(vehicle_fuel_type, levels = c("gasoline", "diesel"))
    )
  
  return(data)
}
