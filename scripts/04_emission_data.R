
### read, restructure and combine air pollutant emission data from budgets
### ------------------------------------------------------------

# req <- httr2::request("https://opendata.swiss/api/3/action/package_list")
# req <- httr2::req_perform(req)
# opendatasets <- unlist(httr2::resp_body_json(req)$result) # all available datasets
# opendatasets[stringr::str_detect(opendatasets, "luftschadstoffemissionen-im-kanton-zurich")] # => exists
# opendatasets[stringr::str_detect(opendatasets, "messdaten-langjahriger-abgasmessungen-im-realen-fahrbetrieb-mittels-remote-sensing-rsd")] # => exists

### emission budgets of air pollutants in the Canton of Zürich, stratified for emission sector groups and subgroups 


# FIXME: würde schauen, dass du prep funktionen mit sehr sprechendem damen baust.
# Dann liesst sich der code viel einfacher


# FIXME: eigene funktion 
req <- httr2::request(files$emissions$budget$opendata)
req <- httr2::req_perform(req)
emikat <- httr2::resp_body_json(req)$result        
emikat <- unlist(purrr::map(emikat$resources, function(x) x$url))
emikat <- emikat[stringr::str_detect(emikat, ".csv")]

data_emikat <-
  emikat %>%
  readr::read_delim(delim = ",") %>%
  dplyr::filter(kanton == "ZH" & !(untergruppe %in% c("Weitere Punktquellen OL", "Rheinschifffahrt", "Flugverkehr Genf"))) %>% # exclude some groups that might be redundant due to area distribution methodology
  dplyr::select(-kanton, -einheit_lang) %>% 
  dplyr::rename( # ... just for the sake of script language consistency
    year = jahr,
    pollutant = substanz,
    sector = hauptgruppe,
    subsector = untergruppe,
    municipality = gemeinde, 
    unit = einheit
  ) %>% 
  dplyr::mutate(
    pollutant = dplyr::case_when(pollutant == "BC" ~"eBC", TRUE ~ pollutant),
    source = "OSTLUFT"
  )


### evaluating NOx emissions by vehicle remote sensing (RSD) in the Canton of Zürich
### see: https://www.zh.ch/de/umwelt-tiere/luft-strahlung/luftschadstoffquellen/emissionen-verkehr/abgasmessungen-rsd.html
### ------------------------------------------------------------

### read and restructure RSD data

req <- httr2::request(files$emissions$rsd$opendata)
req <- httr2::req_perform(req)
rsd <- httr2::resp_body_json(req)$result        
rsd <- unlist(purrr::map(rsd$resources, function(x) x$url))
rsd <- rsd[stringr::str_detect(rsd, ".csv")]

data_rsd <- 
  rsd %>% 
  lapply(function(x) readr::read_delim(x, delim = ",")) %>% 
  dplyr::bind_rows()

rsd_meta <-
  fs::path("data/input", files$emissions$rsd$meta) %>% 
  readr::read_delim(delim = ";") %>%
  dplyr::select(-source, -remark) %>% 
  tidyr::spread(parameter, value) %>% 
  dplyr::mutate(
    vehicle_type = factor(vehicle_type, levels = c("passenger car", "light duty vehicle")),
    vehicle_fuel_type = factor(vehicle_fuel_type, levels = c("gasoline", "diesel"))
  ) %>% 
  dplyr::rename(NOx_emission_threshold_g_per_kg_fuel = `nox_emission_threshold_g_per_kg Treibstoff`)

### calculate vehicle specific power and apply data filters for a meaningful analysis
### calculate mean values and corresponding NOx emissions
### ! for a profound analysis, one should also check units, explore site roadgrade, vehicle specific power and air temperature distribution, and general data plausibility and consistency etc in detail

data_temp <- 
  data_rsd %>% 
  dplyr::filter(parameter %in% c("acceleration", "velocity") & !is.na(value)) %>%
  dplyr::select(id, site_roadgrade, parameter, value) %>%
  tidyr::spread(parameter, value) %>% 
  dplyr::mutate(vehicle_specific_power = calc_vsp(velocity * 1000 / 60^2, acceleration * 1000 / 60^2, site_roadgrade)) %>%  # also convert velocity from km/h into m/s and acceleration from km/h/s into m/s2
  dplyr::select(id, acceleration, velocity, vehicle_specific_power) # vehicle_specific_power in kW/t

data_rsd <-
  data_rsd %>%
  dplyr::select(-unit) %>% 
  dplyr::filter(!(parameter %in% c("acceleration", "velocity"))) %>%
  dplyr::left_join(data_temp, by = "id") %>% 
  dplyr::filter(
    vehicle_model_year %in% rsd_filters$vehicleyears &
      (acceleration >= min(rsd_filters$accelerationrange) & acceleration <= max(rsd_filters$accelerationrange)) &
      (velocity >= min(rsd_filters$velocityrange) & velocity <= max(rsd_filters$velocityrange)) &
      (vehicle_specific_power >= min(rsd_filters$vsprange) & vehicle_specific_power <= max(rsd_filters$vsprange)) &
      vehicle_unloaded_weight <= rsd_filters$weightmax &
      !is.na(value)
  ) %>%
  dplyr::mutate(vehicle_euronorm = dplyr::recode(vehicle_euronorm, !!!c("Euro5a" = "Euro5", "Euro5b" = "Euro5"))) %>% # merge both sub-Euro5 norms since they are quite similar
  tidyr::spread(parameter, value) %>%
  dplyr::filter(!is.na(NO + CO2 + CO + HC)) %>%  # all concentrations nessecary for NOx emission calculation
  dplyr::left_join(dplyr::filter(rsd_meta, is.na(as.numeric(vehicle_euronorm))), by = c("vehicle_type", "vehicle_fuel_type", "vehicle_euronorm")) %>% 
  dplyr::mutate(
    vehicle_type = factor(vehicle_type, levels = c("passenger car", "light duty vehicle")),
    vehicle_fuel_type = factor(vehicle_fuel_type, levels = c("gasoline", "diesel")),
    NOx_emission = calc_rsd_nox_emission(NO = NO / 10^4, p = fraction_no2_hbefa, CO2 = CO2, CO = CO, HC = HC / 10^4) # input: concentrations all in percent; originally: NO in ppm, CO2 in %, CO in %, HC in ppm; output: NOx emissions in g/kg fuel;  add HBEFA-derived NO2 and use that for NOx emission calculation rather than measured NO2 since that has only been available since RSD-model 4500
  ) %>% 
  dplyr::filter(!is.na(NOx_emission))

### aggregate RSD NOx emissions per norm, vehicle type and fuel type

data_rsd_per_norm <-
  data_rsd %>% 
  aggregate_groups_rsd(y = "NOx_emission", groups = c("vehicle_type", "vehicle_fuel_type", "vehicle_euronorm"), nmin = rsd_filters$nmin) %>% 
  dplyr::rename(NOx_emission = mean) %>% 
  dplyr::mutate(
    unit = "g/kg fuel",
    source = "Kanton Zürich/AWEL"
  ) %>% 
  dplyr::left_join(dplyr::filter(rsd_meta, is.na(as.numeric(vehicle_euronorm))), by = c("vehicle_euronorm", "vehicle_type", "vehicle_fuel_type")) %>% 
  dplyr::select(vehicle_euronorm, vehicle_type, vehicle_fuel_type, NOx_emission, unit, n, standarderror, NOx_emission_threshold_g_per_kg_fuel, source)

### aggregate RSD NOx emissions per year of vehicle model, vehicle type and fuel type

data_temp <- 
  rsd_meta %>% 
  dplyr::filter(!is.na(as.numeric(vehicle_euronorm))) %>% 
  dplyr::mutate(vehicle_euronorm = as.numeric(vehicle_euronorm)) %>% 
  dplyr::rename(vehicle_model_year = vehicle_euronorm)
  
data_rsd_per_yearmodel <-
  data_rsd %>% 
  aggregate_groups_rsd(y = "NOx_emission", groups = c("vehicle_model_year", "vehicle_type", "vehicle_fuel_type"), nmin = rsd_filters$nmin) %>% 
  dplyr::rename(NOx_emission = mean) %>% 
  dplyr::mutate(
    unit = "g/kg fuel",
    source = "Kanton Zürich/AWEL"
  ) %>% 
  dplyr::left_join(data_temp, by = c("vehicle_model_year", "vehicle_type", "vehicle_fuel_type")) %>% 
  dplyr::select(vehicle_model_year, vehicle_type, vehicle_fuel_type, NOx_emission, unit, n, standarderror, NOx_emission_threshold_g_per_kg_fuel, source)

### aggregate RSD NOx emissions per year of measurement and fuel type

data_rsd_per_yearmeas <-
  data_rsd %>% 
  dplyr::mutate(vehicle_fuel_type = "all") %>% 
  dplyr::bind_rows(data_rsd) %>% 
  dplyr::mutate(year = lubridate::year(date_measured)) %>% 
  aggregate_groups_rsd(y = "NOx_emission", groups = c("year", "vehicle_fuel_type"), nmin = rsd_filters$nmin) %>% 
  dplyr::rename(NOx_emission = mean) %>% 
  dplyr::mutate(
    unit = "g/kg fuel",
    source = "Kanton Zürich/AWEL"
  ) %>% 
  dplyr::select(year, vehicle_fuel_type, NOx_emission, unit, n, standarderror, source)



### write datasets
### ------------------------------------------------------------

# ... emission datasets ?

readr::write_delim(data_rsd_per_norm, file = "data/output/data_nox_emissions_rsd_per_norm.csv", delim = ";", na = "NA")
readr::write_delim(data_rsd_per_yearmodel, file = "data/output/data_nox_emissions_rsd_per_yearmodel.csv", delim = ";", na = "NA")
readr::write_delim(data_rsd_per_yearmeas, file = "data/output/data_nox_emissions_rsd_per_yearmeas.csv", delim = ";", na = "NA")



### clean up
### ------------------------------------------------------------
# ...

