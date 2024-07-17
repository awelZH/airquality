emmisionen <- read_opendataswiss(
  'https://ckan.opendata.swiss/api/3/action/package_show?id=luftschadstoffemissionen-im-kanton-zurich',
  source = "OSTLUFT")


emmission_prep <- prep_emmissions(emmisionen)


emikat <- analyse_emmissions(emmission_prep)


data_rsd <- read_opendataswiss(
  'https://ckan.opendata.swiss/api/3/action/package_show?id=messdaten-langjahriger-abgasmessungen-im-realen-fahrbetrieb-mittels-remote-sensing-rsd',
  source = "OSTLUFT")

rsd_meta <- read_local_csv("misc_rsd_auxiliary.csv")

# evaluating NOx emissions by vehicle remote sensing (RSD) in the Canton of Zürich:
# ! for a profound analysis, one should also check units, explore site roadgrade, vehicle specific power and air temperature distribution, and general data plausibility and consistency etc in detail
data_rsd_prep <- prep_rsd(data_rsd, rsd_meta)


### aggregate RSD NOx emissions per norm, vehicle type and fuel type and pick mean values
data_rsd_per_norm <- analyse_nox_euronorm(data_rsd, rsd_meta)

### aggregate RSD NOx emissions per year of vehicle model, vehicle type and fuel type
data_rsd_per_yearmodel <- analyse_nox_year_model(data_rsd, rsd_meta)


### aggregate RSD NOx emissions per year of measurement and fuel type (including all = gasoline and diesel)
data_rsd_per_yearmeas <- analyse_nox_year_measurement(data_rsd, NULL)

# write output datasets

readr::write_delim(data_emikat, file = "inst/extdata/output_data_emissions.csv", delim = ";", na = "NA")
readr::write_delim(data_rsd_per_norm, file = "inst/extdata/output_data_nox_vehicle_emissions_rsd_per_norm.csv", delim = ";", na = "NA")
readr::write_delim(data_rsd_per_yearmodel, file = "inst/extdata/output_data_nox_emissions_rsd_per_yearmodel.csv", delim = ";", na = "NA")
readr::write_delim(data_rsd_per_yearmeas, file = "inst/extdata/output_data_nox_emissions_rsd_per_yearmeas.csv", delim = ";", na = "NA")

# clean up

rm(list = c("data_emikat", "groups", "data_rsd", "data_vsp", "data_rsd_per_norm", "data_rsd_per_yearmodel", "data_rsd_per_yearmeas", "rsd_meta", "rsd_meta_temp", "rsd_filters"))



