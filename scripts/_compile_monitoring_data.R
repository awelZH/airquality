# compiling air quality monitoring data from several sites in the Canton of Zürich by Ostluft and NABEL monitoring networks


# load monitoring datasets from airquality.data (see here: https://github.com/awelZH/airquality.data)
# TODO: ... remove data_monitoring_ndep from airquality.data when ndep-analysis is online at github. Integrate future github dataset instead
# TODO: ... replace airquality.data as soon as useful api methods are available for this kind of data
# ---
data_monitoring_aq <- airquality.data::data_monitoring_aq
data_monitoring_ndep <- airquality.data::data_monitoring_ndep


# aggregate dataset ...
# ---
# => aggregate individual components of nitrogen deposition
data_monitoring_ndep <- aggregate_nitrogen_deposition(data_monitoring_ndep)


# write output datasets & clean up:
# ---
write_local_csv(data_monitoring_aq, file = "inst/extdata/output/data_airquality_monitoring_y1.csv")
write_local_csv(data_monitoring_ndep, file = "inst/extdata/output/data_ndep_monitoring_y1.csv")
rm(list = c("data_monitoring_aq", "data_monitoring_ndep"))
