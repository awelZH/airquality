# monitoring: nitrogen deposition in sensitive ecosystems at the sites in the Canton of Zurich
# (functions in R/monitoring.R) -> data_ndep_pars_monitoring_y1.csv, data_ndep_monitoring_y1.csv

pipeline_monitoring_ndep <- tar_plan(
  # pre-compiled deposition data and site metadata of airquality.data, checked on every run
  tar_target(mon_ndep_raw_sites, airquality.data::site_meta_ndep, cue = tar_cue("always")),
  tar_target(mon_ndep_raw, airquality.data::data_monitoring_ndep_y1, cue = tar_cue("always")),

  # site metadata with the Ostluft site class and the NH3 emission class
  mon_ndep_site_meta = prepare_ndep_site_meta(mon_ndep_raw_sites),
  # one row per site, ecosystem, year and deposition parameter
  mon_ndep_parameters = prepare_ndep_parameters(mon_ndep_raw, mon_ndep_site_meta, cantons = mon_cantons),
  # total deposition per site and year, with its estimated (modelled) part
  mon_ndep_total = aggregate_ndep(mon_ndep_parameters, additional_groups = c("x", "y", "masl", "pollutant", "metric")),

  tar_target(mon_ndep_out_parameters, write_output(mon_ndep_parameters, "data_ndep_pars_monitoring_y1.csv", dir = path_output), format = "file"),
  tar_target(mon_ndep_out_total, write_output(mon_ndep_total, "data_ndep_monitoring_y1.csv", dir = path_output), format = "file")
)
