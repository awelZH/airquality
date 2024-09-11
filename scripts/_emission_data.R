# read emission budget data of air pollutants in the Canton of Zürich, stratified for emission sector groups and subgroups, from opendata.swiss
data_emikat <- get_emissions_opendataswiss(filter_ressources(ressources, 1))
update_log(1)

# filter emission data for municipalities within Canton Zürich and exclude some groups that are redundant due to area distribution methodology; also remove emissions of 0
data_emikat <- filter_emissions(data_emikat) 

# group minor subsectors per emission sector in order to be able to have a plot with max 3 subsectors per sector
# this step is only for convenience in plotting!
groups <- groups_emission_subsector(data_emikat)
data_emikat <- left_join(data_emikat, groups, by = c("pollutant", "sector", "subsector"))

# aggregate emissions per pollutant, subsector_new and year
data_emikat <- aggregate_emissions(data_emikat, groups = c("year", "pollutant", "unit", "sector", "subsector_new")) #! test possibility here: sum of emissions needs to match that of original data_emikat

# read Canton Zürich vehicle remote sensing (RS) emission measurement data (RSD) from opendata.swiss, see also: https://www.zh.ch/de/umwelt-tiere/luft-strahlung/luftschadstoffquellen/emissionen-verkehr/abgasmessungen-rsd.html
data_rsd <- get_rsd_opendataswiss(filter_ressources(ressources, 2))
update_log(2)

# read and restructure RSD metadata 
rsd_meta <- readr::read_delim(filter_ressources(ressources, 3), delim = ";")
update_log(3)
rsd_meta <- restructure_rsd_meta(rsd_meta )

# read and restructure RSD filter criteria for NOx emission analysis
rsd_filters <- readr::read_delim(filter_ressources(ressources, 4), delim = ";")
update_log(4)
rsd_filters$max[which(rsd_filters$parameter == "vehicleyears")] <- lubridate::year(Sys.Date()) # include most recent vehicle model years
  
# evaluating NOx emissions by vehicle remote sensing (RSD) in the Canton of Zürich:
# ! for a profound analysis, one should also check units, explore site roadgrade, vehicle specific power and air temperature distribution, and general data plausibility and consistency etc in detail

# calculate vehicle specific power from measurement data subset and merge with RSD dataset
data_vsp <-
  data_rsd |>
  restructure_rsd_for_vsp() |> 
  dplyr::mutate(vehicle_specific_power = calc_vsp(velocity * 1000 / 60^2, acceleration * 1000 / 60^2, site_roadgrade)) |>  # also convert velocity from km/h into m/s and acceleration from km/h/s into m/s2
  dplyr::select(id, acceleration, velocity, vehicle_specific_power) # vehicle_specific_power in kW/t

data_rsd <-
  data_rsd |>
  dplyr::select(-unit) |> 
  dplyr::filter(!(parameter %in% c("acceleration", "velocity"))) |>
  dplyr::left_join(data_vsp, by = "id") 

# apply data filters for a meaningful analysis
data_rsd <- filter_rsd(data_rsd, rsd_filters)

# restructure and merge with Euronorm metadata 
data_rsd <- merge_restructure_rsd(data_rsd, rsd_meta)
  
# calculate NOx emissions
data_rsd <- dplyr::mutate(data_rsd, NOx_emission = calc_rsd_nox_emission(NO = NO / 10^4, p = fraction_no2_hbefa, CO2 = CO2, CO = CO, HC = HC / 10^4)) # input: concentrations all in percent; originally: NO in ppm, CO2 in %, CO in %, HC in ppm; output: NOx emissions in g/kg fuel;  add HBEFA-derived NO2 and use that for NOx emission calculation rather than measured NO2 since that has only been available since RSD-model 4500

### aggregate RSD NOx emissions per norm, vehicle type and fuel type and pick mean values
rsd_meta_temp <- dplyr::filter(rsd_meta, is.na(as.numeric(vehicle_euronorm)))
data_rsd_per_norm <- aggregate_nox_rsd(data_rsd, rsd_meta_temp, nmin = rsd_filters$min[rsd_filters$parameter == "nmin"], groups = c("vehicle_type", "vehicle_fuel_type", "vehicle_euronorm")) 
  
### aggregate RSD NOx emissions per year of vehicle model, vehicle type and fuel type
rsd_meta_temp <-
  rsd_meta |> 
  dplyr::filter(!is.na(as.numeric(vehicle_euronorm))) |> 
  dplyr::mutate(vehicle_euronorm = as.numeric(vehicle_euronorm)) |> 
  dplyr::rename(vehicle_model_year = vehicle_euronorm)

data_rsd_per_yearmodel <- aggregate_nox_rsd(data_rsd, rsd_meta_temp, nmin = rsd_filters$min[rsd_filters$parameter == "nmin"], groups = c("vehicle_model_year", "vehicle_type", "vehicle_fuel_type"))

### aggregate RSD NOx emissions per year of measurement and fuel type (including all = gasoline and diesel)
data_rsd_per_yearmeas <-
  data_rsd |> 
    dplyr::mutate(vehicle_fuel_type = "all") |> 
    dplyr::bind_rows(data_rsd) |> 
    dplyr::mutate(year = lubridate::year(date_measured)) |> 
    aggregate_nox_rsd(NULL, nmin = rsd_filters$min[rsd_filters$parameter == "nmin"], groups = c("year", "vehicle_fuel_type"))
  
# write output datasets
readr::write_delim(data_emikat, file = "inst/extdata/output/data_emissions.csv", delim = ";", na = "NA")
update_log(21)
readr::write_delim(data_rsd_per_norm, file = "inst/extdata/output/data_nox_vehicle_emissions_rsd_per_norm.csv", delim = ";", na = "NA")
readr::write_delim(data_rsd_per_yearmodel, file = "inst/extdata/output/data_nox_emissions_rsd_per_yearmodel.csv", delim = ";", na = "NA")
readr::write_delim(data_rsd_per_yearmeas, file = "inst/extdata/output/data_nox_emissions_rsd_per_yearmeas.csv", delim = ";", na = "NA")
update_log(22)

# clean up

rm(list = c("data_emikat", "groups", "data_rsd", "data_vsp", "data_rsd_per_norm", "data_rsd_per_yearmodel", "data_rsd_per_yearmeas", "rsd_meta", "rsd_meta_temp", "rsd_filters"))
