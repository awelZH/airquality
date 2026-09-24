# emissions: vehicle NOx emissions from real-world remote sensing (RSD) measurements of the Canton of Zurich
# (functions in R/emissions.R) -> the 3 data_nox_*rsd*.csv

pipeline_emissions_rsd <- tar_plan(
  # RSD measurements from opendata.swiss, checked on every run, see also
  # https://www.zh.ch/de/umwelt-tiere/luft-strahlung/luftschadstoffquellen/emissionen-verkehr/abgasmessungen-rsd.html
  tar_target(
    emis_rsd_raw,
    airquality.methods::read_opendataswiss(filter_ressources(setup_ressources, 2), source = "Kanton Zürich/AWEL"),
    cue = tar_cue("always")
  ),
  # vehicle metadata (NO2 fractions, NOx thresholds per Euronorm and model year) and filter criteria
  tar_target(emis_rsd_file_meta, filter_ressources(setup_ressources, 3), format = "file"),
  tar_target(emis_rsd_file_filters, filter_ressources(setup_ressources, 4), format = "file"),
  emis_rsd_meta = airquality.methods::read_local_csv(emis_rsd_file_meta),
  emis_rsd_filters = airquality.methods::read_local_csv(emis_rsd_file_filters),

  # one row per vehicle: vehicle specific power, filter criteria, metadata, NOx emission
  emis_rsd_vehicles = prepare_rsd(emis_rsd_raw, emis_rsd_meta, emis_rsd_filters, model_year_max = emis_year_max),

  # mean NOx emissions per Euronorm, per vehicle model year and per year of measurement
  emis_rsd_per_norm = aggregate_rsd_nox(emis_rsd_vehicles, emis_rsd_meta, emis_rsd_filters,
                                        groups = c("vehicle_type", "vehicle_fuel_type", "vehicle_euronorm")),
  emis_rsd_per_yearmodel = aggregate_rsd_nox(emis_rsd_vehicles, emis_rsd_meta, emis_rsd_filters,
                                             groups = c("vehicle_model_year", "vehicle_type", "vehicle_fuel_type")),
  emis_rsd_per_yearmeas = aggregate_rsd_nox(emis_rsd_vehicles, emis_rsd_meta, emis_rsd_filters,
                                            groups = c("year", "vehicle_fuel_type")),

  tar_target(emis_rsd_out_per_norm, write_output(emis_rsd_per_norm, "data_nox_vehicle_emissions_rsd_per_norm.csv", dir = path_output), format = "file"),
  tar_target(emis_rsd_out_per_yearmodel, write_output(emis_rsd_per_yearmodel, "data_nox_emissions_rsd_per_yearmodel.csv", dir = path_output), format = "file"),
  tar_target(emis_rsd_out_per_yearmeas, write_output(emis_rsd_per_yearmeas, "data_nox_emissions_rsd_per_yearmeas.csv", dir = path_output), format = "file")
)
