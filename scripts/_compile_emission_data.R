# compiling air pollutant emissions for the Canton of Zürich (functions in R/emissions.R):


# emission inventory (EMIKAT) ...
# ---
# => read emission budget data of air pollutants in the Canton of Zürich, stratified for emission sector groups and
#    subgroups, from opendata.swiss; and the lookup table merging and renaming subsectors thematically
data_emikat <- airquality.methods::read_opendataswiss(filter_ressources(ressources, 1), source = "Ostluft & BAFU")
subsector_new <- airquality.methods::read_local_csv(filter_ressources(ressources, 27), locale = readr::locale(encoding = "UTF-8"))

# => latest inventory version for the Canton of Zürich without projections beyond emis_year_max, without zero
#    emissions and without subsectors that are redundant due to the area distribution methodology
data_emikat <- prepare_emissions(data_emikat, year_max = emis_year_max)

# => sum up emissions per year, pollutant, sector and grouped subsector
data_emikat <- aggregate_emissions(data_emikat, subsector_new)

# => per pollutant and sector: small subsectors (mean yearly share) into "verschiedene", at most emis_subsectors_max
#    subsectors incl. "verschiedene" (keeps the plots readable; same groups for the whole time series of a pollutant)
data_emikat <- group_minor_subsectors(data_emikat, min_share = emis_subsector_min_share, max_per_sector = emis_subsectors_max)

# => plot order and colour per subsector, ranked by the emissions of the published years
data_emikat <- add_emission_colours(data_emikat)


# vehicle NOx emissions from real-world remote sensing (RSD) measurements ...
# ---
# => read Canton Zürich RSD data from opendata.swiss, see also:
#    https://www.zh.ch/de/umwelt-tiere/luft-strahlung/luftschadstoffquellen/emissionen-verkehr/abgasmessungen-rsd.html
#    and local metadata (NO2 fractions, NOx thresholds per Euronorm and model year) and filter criteria
data_rsd <- airquality.methods::read_opendataswiss(filter_ressources(ressources, 2), source = "Kanton Zürich/AWEL")
rsd_meta <- airquality.methods::read_local_csv(filter_ressources(ressources, 3))
rsd_filters <- airquality.methods::read_local_csv(filter_ressources(ressources, 4))

# => one row per vehicle: vehicle specific power, filter criteria, metadata, NOx emission
data_rsd <- prepare_rsd(data_rsd, rsd_meta, rsd_filters, model_year_max = emis_year_max)

# => mean NOx emissions per Euronorm, per vehicle model year and per year of measurement (incl. all fuel types)
data_rsd_per_norm <- aggregate_rsd_nox(data_rsd, rsd_meta, rsd_filters, groups = c("vehicle_type", "vehicle_fuel_type", "vehicle_euronorm"))
data_rsd_per_yearmodel <- aggregate_rsd_nox(data_rsd, rsd_meta, rsd_filters, groups = c("vehicle_model_year", "vehicle_type", "vehicle_fuel_type"))
data_rsd_per_yearmeas <- aggregate_rsd_nox(data_rsd, rsd_meta, rsd_filters, groups = c("year", "vehicle_fuel_type"))


# write output datasets & clean up:
# ---
airquality.methods::write_local_csv(data_emikat, file = "data/output/data_emissions.csv")
airquality.methods::write_local_csv(data_rsd_per_norm, file = "data/output/data_nox_vehicle_emissions_rsd_per_norm.csv")
airquality.methods::write_local_csv(data_rsd_per_yearmodel, file = "data/output/data_nox_emissions_rsd_per_yearmodel.csv")
airquality.methods::write_local_csv(data_rsd_per_yearmeas, file = "data/output/data_nox_emissions_rsd_per_yearmeas.csv")
rm(list = c("data_emikat", "subsector_new", "data_rsd", "rsd_meta", "rsd_filters",
            "data_rsd_per_norm", "data_rsd_per_yearmodel", "data_rsd_per_yearmeas"))
