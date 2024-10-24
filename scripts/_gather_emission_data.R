# compiling air pollutant emissions for the Canton of Zürich:
# ---
# read emission budget data of air pollutants in the Canton of Zürich, stratified for emission sector groups and subgroups, from opendata.swiss
data_emikat <- read_opendataswiss(filter_ressources(ressources, 1), source = "Ostluft"); update_log(1)

# prepare dataset
# => filter emission data for municipalities within Canton Zürich and exclude some groups that are redundant due to area distribution methodology; also remove emissions of 0
# => group minor subsectors per emission sector in order to be able to have a plot with max 3 subsectors per sector (this step is only for convenience in plotting!)
data_emikat <- prepare_emmissions(data_emikat)

# aggregate dataset
# => aggregate emissions per pollutant, subsector_new and year
data_emikat <- aggregate_emmissions(data_emikat)




# compiling average vehicle NOx emissions from real-world vehicle remote sensing (RS) emission measurements using a remote sensing detector (RSD):
# ---
# read Canton Zürich data (RSD) from opendata.swiss, see also: https://www.zh.ch/de/umwelt-tiere/luft-strahlung/luftschadstoffquellen/emissionen-verkehr/abgasmessungen-rsd.html
data_rsd <- read_opendataswiss(filter_ressources(ressources, 2), source = "Kanton Zürich/AWEL"); update_log(2)

# read local metadata (e.g. fractions NO:NO2, emission thresholds, etc) and filter criteria 
rsd_auxiliary <- list(meta = read_local_csv(filter_ressources(ressources, 3))); update_log(3)
rsd_auxiliary$filters <- read_local_csv(filter_ressources(ressources, 4)); update_log(4)

# prepare dataset
# => several steps like calculating vehicle specific power, filtering, restructuring, calculating NOx emissions
data_rsd <- prepare_rsd(data_rsd, rsd_auxiliary)

# aggregate dataset
# => aggregate NOx emissions per euronorm, vehicle type and fuel type as mean values
data_rsd_per_norm <- aggregate_rsd_nox(data_rsd, rsd_auxiliary, groups = c("vehicle_type", "vehicle_fuel_type", "vehicle_euronorm"))

# => aggregate NOx emissions per year of vehicle model, vehicle type and fuel type as mean values
data_rsd_per_yearmodel <- aggregate_rsd_nox(data_rsd, rsd_auxiliary, groups = c("vehicle_model_year", "vehicle_type", "vehicle_fuel_type"))

# => aggregate NOx emissions per year of measurement and fuel type (including all = gasoline and diesel) as mean values
data_rsd_per_yearmeas <- aggregate_rsd_nox(data_rsd, rsd_auxiliary, groups = c("year", "vehicle_fuel_type"))




# write output datasets & clean up:
# ---
readr::write_delim(data_emikat, file = "inst/extdata/output/data_emissions.csv", delim = ";", na = "NA"); update_log(21)
readr::write_delim(data_rsd_per_norm, file = "inst/extdata/output/data_nox_vehicle_emissions_rsd_per_norm.csv", delim = ";", na = "NA")
readr::write_delim(data_rsd_per_yearmodel, file = "inst/extdata/output/data_nox_emissions_rsd_per_yearmodel.csv", delim = ";", na = "NA")
readr::write_delim(data_rsd_per_yearmeas, file = "inst/extdata/output/data_nox_emissions_rsd_per_yearmeas.csv", delim = ";", na = "NA"); update_log(22)
rm(list = c("data_emikat", "data_rsd", "data_rsd_per_norm", "data_rsd_per_yearmodel", "data_rsd_per_yearmeas", "rsd_auxiliary"))
