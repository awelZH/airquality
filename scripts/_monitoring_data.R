# read NABEL monitoring site metadata
site_meta_nabel <- read_site_meta_nabel_arias_csv(filter_ressources(ressources, 5))
update_log(5)

# read OSTLUFT monitoring site metadata
site_meta_ostluft <- read_site_meta_ostluft_metadb_csv(filter_ressources(ressources, 9))
update_log(9)

# merge & finalise
site_meta <- 
  site_meta_nabel |> 
  dplyr::bind_rows(site_meta_ostluft) |>
  dplyr::mutate(siteclass = paste(zone, type, sep = " - ")) |>
  dplyr::select(-zone, -type)

# read NABEL y1 monitoring airquality data
data_monitoring_nabel <- read_monitoring_data_nabel_arias_csv(filter_ressources(ressources, 5))
update_log(5)

# read OSTLUFT y1 monitoring airquality data
data_monitoring_ostluft <- read_monitoring_data_ostluft_airmo_csv(filter_ressources(ressources, 6))
update_log(6)

# remove duplicate parameters in OSTLUFT data => use preferred method (NO2 passive samplers vs. monitor & PM10/PM2.5 HVS vs. monitor)
data_monitoring_ostluft <- remove_duplicate_y1(data_monitoring_ostluft)

# read pre-compiled OSTLUFT & NABEL O3 peak-season y1 data
data_monitoring_o3_peakseason <- readr::read_delim(filter_ressources(ressources, 7), delim = ";", locale = readr::locale(tz = "Etc/GMT-1"))
update_log(7)

# merge & finalise
data_monitoring_aq <-
  data_monitoring_nabel |> 
  dplyr::bind_rows(data_monitoring_ostluft) |> 
  dplyr::bind_rows(data_monitoring_o3_peakseason) |> 
  pad2() |>
  dplyr::mutate(
    year = lubridate::year(starttime),
    parameter = dplyr::recode(parameter, "O3_max_98%_m1" = "O3_max_98p_m1") # for technical reasons
    ) |> 
  dplyr::select(-source) |>
  dplyr::left_join(site_meta, by = "site") |>
  dplyr::filter(!is.na(siteclass)) |> 
  dplyr::arrange(site, parameter, starttime) |> 
  dplyr::select(year, site, site_long, siteclass, x, y, masl, source, parameter, interval, unit, value)
 
# read pre-compiled OSTLUFT y1 monitoring data for nitrogen deposition to sensitive ecosystems into separate dataset
data_monitoring_ndep <- readr::read_delim(filter_ressources(ressources, 8), delim = ";")
update_log(8)

# aggregate individual components of nitrogen deposition
data_monitoring_ndep <- aggregate_nitrogen_deposition(data_monitoring_ndep)

# write output datasets
readr::write_delim(data_monitoring_aq , file = "inst/extdata/output/data_airquality_monitoring_y1.csv", delim = ";", na = "NA")
update_log(23)
readr::write_delim(data_monitoring_ndep , file = "inst/extdata/output/data_ndep_monitoring_y1.csv", delim = ";", na = "NA")
update_log(24)

# clean up
rm(list = c("data_monitoring_aq", "data_monitoring_ndep", "site_meta_nabel", "site_meta_ostluft", "data_monitoring_nabel", "data_monitoring_ostluft", "data_monitoring_o3_peakseason"))
