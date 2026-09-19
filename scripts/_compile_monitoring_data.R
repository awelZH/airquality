# compiling air quality monitoring data from several sites in the Canton of Zürich by Ostluft and NABEL monitoring
# networks (functions in R/monitoring.R):
# TODO: ... replace airquality.data as soon as useful api methods are available for this kind of data


# air pollutants ...
# ---
# => read pre-compiled yearly monitoring data from airquality.data (see: https://github.com/awelZH/airquality.data)
#    and keep the monitoring sites of the canton (incl. the NABEL sites Zürich-Kaserne and Dübendorf-EMPA)
data_monitoring_aq <- prepare_monitoring_airquality(airquality.data::data_monitoring_aq_y1, cantons = mon_cantons)


# nitrogen deposition in sensitive ecosystems ...
# ---
# => pre-compiled deposition data based on monitoring, raster data and statistical models based on NABEL data
# => site metadata with the Ostluft site class (agricultural pressure within 5 km) and the NH3 emission class
site_meta_ndep <- prepare_ndep_site_meta(airquality.data::site_meta_ndep)

# => one row per site, ecosystem, year and deposition parameter, with the site metadata
data_monitoring_ndep_pars <- prepare_ndep_parameters(airquality.data::data_monitoring_ndep_y1, site_meta_ndep,
                                                     cantons = mon_cantons)

# => total deposition per site and year, with its estimated (modelled) part
data_monitoring_ndep <- aggregate_ndep(data_monitoring_ndep_pars,
                                       additional_groups = c("x", "y", "masl", "pollutant", "metric"))


# write output datasets & clean up:
# ---
airquality.methods::write_local_csv(data_monitoring_aq, file = "inst/extdata/output/data_airquality_monitoring_y1.csv")
airquality.methods::write_local_csv(data_monitoring_ndep_pars, file = "inst/extdata/output/data_ndep_pars_monitoring_y1.csv")
airquality.methods::write_local_csv(data_monitoring_ndep, file = "inst/extdata/output/data_ndep_monitoring_y1.csv")
rm(list = c("data_monitoring_aq", "site_meta_ndep", "data_monitoring_ndep_pars", "data_monitoring_ndep"))
