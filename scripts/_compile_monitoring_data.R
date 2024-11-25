# compiling air quality monitoring data from several sites in the Canton of Zürich by Ostluft and NABEL monitoring networks


# read datasets ...
# ---
# => read NABEL y1 monitoring airquality data
data_monitoring_nabel <- read_local_csv(filter_ressources(ressources, 5)); update_log(5)

# => read Ostluft y1 monitoring airquality data
data_monitoring_ostluft <- read_local_csv(filter_ressources(ressources, 6), locale = readr::locale(encoding = "UTF-8"), col_names = FALSE); update_log(6)

# => read pre-compiled Ostluft & NABEL O3 peak-season y1 data
# TODO: instead directly include peak-season calculation via calc_all_o3_peakseason(), see issue #36
data_monitoring_o3_peakseason <- read_local_csv(filter_ressources(ressources, 7), locale = readr::locale(encoding = "UTF-8")); update_log(7)

# => read pre-compiled Ostluft y1 monitoring data for nitrogen deposition to sensitive ecosystems into separate dataset
data_monitoring_ndep <- read_local_csv(filter_ressources(ressources, 8), locale = readr::locale(encoding = "UTF-8")); update_log(8)

# => read NABEL monitoring site metadata
site_meta_nabel <- read_local_csv(filter_ressources(ressources, 5), col_select = c("Station", "Ost Y", "Nord X", "Höhe", "Zonentyp", "Stationstyp")); update_log(5)

# => read Ostluft monitoring site metadata
site_meta_ostluft <- read_local_csv(filter_ressources(ressources, 9), delim = ",", locale = readr::locale(encoding = "UTF-8")); update_log(9)

# prepare datasets ...
# ---
# => merge, simplify & finalise site metadata
site_meta <- prepare_monitoring_meta(site_meta_ostluft, site_meta_nabel)

# => restructure NABEL 
data_monitoring_nabel <- prepare_monitoring_nabel_y1(data_monitoring_nabel)

# => restructure Ostluft
data_monitoring_ostluft <- prepare_monitoring_ostluft_y1(data_monitoring_ostluft)

# => merge & finalise datasets
data_monitoring_aq <-
  data_monitoring_nabel |> 
  dplyr::bind_rows(data_monitoring_ostluft) |> 
  dplyr::bind_rows(data_monitoring_o3_peakseason) |> 
  prepare_monitoring_aq(site_meta)
   
# aggregate dataset ...
# ---
# => aggregate individual components of nitrogen deposition
data_monitoring_ndep <- aggregate_nitrogen_deposition(data_monitoring_ndep)

# write output datasets & clean up:
# ---
write_local_csv(data_monitoring_aq, file = "inst/extdata/output/data_airquality_monitoring_y1.csv"); update_log(23)
write_local_csv(data_monitoring_ndep, file = "inst/extdata/output/data_ndep_monitoring_y1.csv"); update_log(24)
rm(list = c("data_monitoring_aq", "data_monitoring_ndep", "site_meta_nabel", "site_meta_ostluft", "data_monitoring_nabel", "site_meta", "data_monitoring_ostluft", "data_monitoring_o3_peakseason"))
