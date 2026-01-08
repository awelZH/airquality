# compiling air pollutant emissions for the Canton of Zürich:


# read dataset ...
# ---
# => read emission budget data of air pollutants in the Canton of Zürich, stratified for emission sector groups and subgroups, from opendata.swiss. Also new pre-defined subsector groups for aggregation
# data_emikat <- airquality.methods::read_opendataswiss(airquality.methods::filter_ressources(ressources, 1), source = "Ostluft")
data_emikat <- airquality.methods::read_local_csv("C:/Users/joerg.sintermann@zh.ch/Downloads/emi_lss_gem_zh.csv", delim = ",", locale = readr::locale(encoding = "UTF-8")) |> 
  mutate(
    stand = readr::parse_number(stand),
    source = "Ostluft"
    ) |> 
  dplyr::filter(stand == max(stand)) |> 
  dplyr::select(-stand)
subsector_new <- airquality.methods::read_local_csv(airquality.methods::filter_ressources(ressources, 27), locale = readr::locale(encoding = "UTF-8"))

# prepare dataset ...
# ---
# => filter emission data for municipalities within Canton Zürich and exclude some groups that are redundant due to area distribution methodology; also remove emissions of 0
# => group minor subsectors per emission sector in order to be able to have a plot with max 3 subsectors per sector (this step is only for convenience in plotting!)
data_emikat <- airquality.methods::prepare_emmissions(data_emikat)

# aggregate dataset ...
# ---
# => aggregate emissions per pollutant, subsector_new and year
data_emikat <- airquality.methods::aggregate_emmissions(data_emikat, subsector_new)


# compiling average vehicle NOx emissions from real-world vehicle remote sensing (RS) emission measurements using a remote sensing detector (RSD):

# read dataset ...
# ---
# => read Canton Zürich data (RSD) from opendata.swiss, see also: https://www.zh.ch/de/umwelt-tiere/luft-strahlung/luftschadstoffquellen/emissionen-verkehr/abgasmessungen-rsd.html
data_rsd <- airquality.methods::read_opendataswiss(airquality.methods::filter_ressources(ressources, 2), source = "Kanton Zürich/AWEL")

# => read local metadata (e.g. fractions NO:NO2, emission thresholds, etc) and filter criteria 
rsd_auxiliary <- list(meta = airquality.methods::read_local_csv(airquality.methods::filter_ressources(ressources, 3)))
rsd_auxiliary$filters <- airquality.methods::read_local_csv(airquality.methods::filter_ressources(ressources, 4))

# prepare dataset ...
# ---
# => several steps like calculating vehicle specific power, filtering, restructuring, calculating NOx emissions
data_rsd <- airquality.methods::prepare_rsd(data_rsd, rsd_auxiliary)

# aggregate datasets ...
# ---
# => aggregate NOx emissions per euronorm, vehicle type and fuel type as mean values
data_rsd_per_norm <- airquality.methods::aggregate_rsd_nox(data_rsd, rsd_auxiliary, groups = c("vehicle_type", "vehicle_fuel_type", "vehicle_euronorm"))

# => aggregate NOx emissions per year of vehicle model, vehicle type and fuel type as mean values
data_rsd_per_yearmodel <- airquality.methods::aggregate_rsd_nox(data_rsd, rsd_auxiliary, groups = c("vehicle_model_year", "vehicle_type", "vehicle_fuel_type"))

# => aggregate NOx emissions per year of measurement and fuel type (including all = gasoline and diesel) as mean values
data_rsd_per_yearmeas <- airquality.methods::aggregate_rsd_nox(data_rsd, rsd_auxiliary, groups = c("year", "vehicle_fuel_type"))

# write output datasets & clean up:
# ---
airquality.methods::write_local_csv(data_emikat, file = "inst/extdata/output/data_emissions.csv")
airquality.methods::write_local_csv(data_rsd_per_norm, file = "inst/extdata/output/data_nox_vehicle_emissions_rsd_per_norm.csv")
airquality.methods::write_local_csv(data_rsd_per_yearmodel, file = "inst/extdata/output/data_nox_emissions_rsd_per_yearmodel.csv")
airquality.methods::write_local_csv(data_rsd_per_yearmeas, file = "inst/extdata/output/data_nox_emissions_rsd_per_yearmeas.csv")
rm(list = c("data_emikat", "data_rsd", "data_rsd_per_norm", "data_rsd_per_yearmodel", "data_rsd_per_yearmeas", "rsd_auxiliary"))
