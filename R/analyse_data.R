analyse_emmissions <- function(data){
  
  groups <- groups_emission_subsector(data)
  
  data_emikat <- dplyr::left_join(
    data, 
    groups, 
    by = c("pollutant", "sector", "subsector")
  )

  data_emikat <- aggregate_emissions(
    data_emikat, 
    groups = c("year", "pollutant", "unit", "sector", "subsector_new")
  ) #! test possibility here: sum of emissions needs to match that of original data_emikat
  
  return(data_emikat)
}


analyse_nox_euronorm <- function(data, rsd_meta){
  rsd_meta_temp <- dplyr::filter(rsd_meta, is.na(as.numeric(vehicle_euronorm)))
  
  data_rsd_per_norm <- aggregate_nox_rsd(
    data, 
    rsd_meta_temp, 
    nmin = rsd_filters$min[rsd_filters$parameter == "nmin"], 
    groups = c("vehicle_type", "vehicle_fuel_type", "vehicle_euronorm")
  ) 
  
  return(data_rsd_per_norm)
}

analyse_nox_year_model <- function(data, rsd_meta){
  rsd_meta_temp <-
    rsd_meta %>% 
    dplyr::filter(!is.na(as.numeric(vehicle_euronorm))) %>% 
    dplyr::mutate(vehicle_euronorm = as.numeric(vehicle_euronorm)) %>% 
    dplyr::rename(vehicle_model_year = vehicle_euronorm)
  
  data_rsd_per_yearmodel <- aggregate_nox_rsd(
    data_rsd, 
    rsd_meta_temp, 
    nmin = rsd_filters$min[rsd_filters$parameter == "nmin"], 
    groups = c("vehicle_model_year", "vehicle_type", "vehicle_fuel_type")
  )
  
  return(data_rsd_per_yearmodel)
}


analyse_nox_year_measurement <- function(data_rsd, rsd_meta){
  data_rsd_per_yearmeas <-
    data_rsd %>% 
    dplyr::mutate(vehicle_fuel_type = "all") %>% 
    dplyr::bind_rows(data_rsd) %>% 
    dplyr::mutate(year = lubridate::year(date_measured)) %>% 
    aggregate_nox_rsd(
      rsd_meta, 
      nmin = rsd_filters$min[rsd_filters$parameter == "nmin"], 
      groups = c("year", "vehicle_fuel_type")
    )
  
  return(data_rsd_per_yearmeas)
}