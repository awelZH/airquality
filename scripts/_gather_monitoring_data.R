# compiling air quality monitoring data from several sites in the Canton of Zürich by Ostluft and NABEL monitoring networks
# ---
# read air quality monitoring datasets
# => read NABEL y1 monitoring airquality data
data_monitoring_nabel <- read_monitoring_data_nabel_arias_csv(filter_ressources(ressources, 5)); update_log(5)

# => read Ostluft y1 monitoring airquality data
data_monitoring_ostluft <- read_monitoring_data_ostluft_airmo_csv(filter_ressources(ressources, 6)); update_log(6)

# => read pre-compiled Ostluft & NABEL O3 peak-season y1 data
data_monitoring_o3_peakseason <- read_local_csv(filter_ressources(ressources, 7)); update_log(7)

# => read pre-compiled Ostluft y1 monitoring data for nitrogen deposition to sensitive ecosystems into separate dataset
data_monitoring_ndep <- read_local_csv(filter_ressources(ressources, 8)); update_log(8)

# => read NABEL monitoring site metadata
site_meta_nabel <- read_site_meta_nabel_arias_csv(filter_ressources(ressources, 5)); update_log(5)

# => read Ostluft monitoring site metadata
site_meta_ostluft <- read_site_meta_ostluft_metadb_csv(filter_ressources(ressources, 9)); update_log(9)





# prepare dataset
# => merge & finalise site metadata
site_meta <- 
  site_meta_nabel |> 
  dplyr::bind_rows(site_meta_ostluft) |>
  dplyr::mutate(siteclass = paste(zone, type, sep = " - ")) |>
  dplyr::select(-zone, -type)

# => remove duplicate parameters in Ostluft data => use preferred method (NO2 passive samplers vs. monitor & PM10/PM2.5 HVS vs. monitor)
data_monitoring_ostluft <- remove_duplicate_y1(data_monitoring_ostluft)

# => merge & finalise monitoring data
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
 




# aggregate dataset (where nessesary)
# => aggregate individual components of nitrogen deposition
data_monitoring_ndep <- aggregate_nitrogen_deposition(data_monitoring_ndep)





# write output datasets and clean up
# ---
write_local_csv(data_monitoring_aq , file = "inst/extdata/output/data_airquality_monitoring_y1.csv"); update_log(23)
write_local_csv(data_monitoring_ndep , file = "inst/extdata/output/data_ndep_monitoring_y1.csv"); update_log(24)
rm(list = c("data_monitoring_aq", "data_monitoring_ndep", "site_meta_nabel", "site_meta_ostluft", "data_monitoring_nabel", "data_monitoring_ostluft", "data_monitoring_o3_peakseason"))
