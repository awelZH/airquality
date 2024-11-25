# compiling air quality monitoring data from several sites in the Canton of Zürich by Ostluft and NABEL monitoring networks


# read datasets ...
# ---
# => read NABEL monitoring airquality data (y1 & h1)
data_monitoring_nabel <- read_local_csv(filter_ressources(ressources, 5)); update_log(5)
# data_monitoring_nabel_h1 <- read_local_csv(filter_ressources(ressources, 22))

# => read Ostluft monitoring airquality data (y1 & h1)
data_monitoring_ostluft <- read_local_csv(filter_ressources(ressources, 6), locale = readr::locale(encoding = "UTF-8"), col_names = FALSE); update_log(6)
data_monitoring_ostluft_h1 <- read_local_csv(filter_ressources(ressources, 21), locale = readr::locale(encoding = "UTF-8"), col_names = FALSE)

# => read pre-compiled Ostluft y1 monitoring data for nitrogen deposition to sensitive ecosystems into separate dataset
data_monitoring_ndep <- read_local_csv(filter_ressources(ressources, 8), locale = readr::locale(encoding = "UTF-8")); update_log(8)

# => read NABEL & Ostluft monitoring site metadata
site_meta_nabel <- read_local_csv(filter_ressources(ressources, 5), col_select = c("Station", "Ost Y", "Nord X", "Höhe", "Zonentyp", "Stationstyp")); update_log(5)
site_meta_ostluft <- read_local_csv(filter_ressources(ressources, 9), delim = ",", locale = readr::locale(encoding = "UTF-8")); update_log(9)

# prepare datasets ...
# ---
# => merge, simplify & finalise site metadata
site_meta <- prepare_monitoring_meta(site_meta_ostluft, site_meta_nabel)

# => restructure NABEL & calculate O3 peak season from h1 data
data_monitoring_nabel <- prepare_monitoring_nabel_y1(data_monitoring_nabel)
# data_monitoring_nabel <- 
#   data_monitoring_nabel_h1 |> 
#   prepare_monitoring_nabel_h1() |> 
#   dplyr::bind_rows(data_monitoring_nabel)

# => restructure Ostluft & calculate O3 peak season from h1 data
data_monitoring_ostluft <- prepare_monitoring_ostluft_y1(data_monitoring_ostluft)
data_monitoring_ostluft <- 
  data_monitoring_ostluft_h1 |> 
  prepare_monitoring_ostluft_h1() |> 
  dplyr::bind_rows(data_monitoring_ostluft)

# => merge & finalise datasets
data_monitoring_aq <-
  data_monitoring_nabel |> 
  dplyr::bind_rows(data_monitoring_ostluft) |> 
  prepare_monitoring_aq(site_meta)
   
# aggregate dataset ...
# ---
# => aggregate individual components of nitrogen deposition
data_monitoring_ndep <- aggregate_nitrogen_deposition(data_monitoring_ndep)

# write output datasets & clean up:
# ---
write_local_csv(data_monitoring_aq, file = "inst/extdata/output/data_airquality_monitoring_y1.csv"); update_log(23)
write_local_csv(data_monitoring_ndep, file = "inst/extdata/output/data_ndep_monitoring_y1.csv"); update_log(24)
rm(list = c("data_monitoring_aq", "data_monitoring_ndep", "site_meta_nabel", "site_meta_ostluft", "data_monitoring_nabel", "data_monitoring_nabel_h1", "site_meta", "data_monitoring_ostluft", "data_monitoring_ostluft_h1"))
