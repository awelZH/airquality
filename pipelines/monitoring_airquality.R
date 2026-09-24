# monitoring: yearly air quality values of the Ostluft and NABEL sites in the Canton of Zurich
# (functions in R/monitoring.R) -> data_airquality_monitoring_y1.csv

pipeline_monitoring_airquality <- tar_plan(
  # pre-compiled yearly monitoring data of airquality.data (https://github.com/awelZH/airquality.data), all
  # cantons; checked on every run, the package may have been updated
  tar_target(mon_aq_raw, airquality.data::data_monitoring_aq_y1, cue = tar_cue("always")),

  # sites of the canton (incl. the NABEL sites Zürich-Kaserne and Dübendorf-EMPA)
  mon_aq_data = prepare_monitoring_airquality(mon_aq_raw, cantons = mon_cantons),

  tar_target(mon_aq_out, write_output(mon_aq_data, "data_airquality_monitoring_y1.csv", dir = path_output), format = "file")
)
