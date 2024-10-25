# Selected ozone (O3) monitoring data from national monitoring network NABEL (hourly values) for sites in Kanton Zürich
# - source: Bundesamt für Umwelt BAFU/NABEL
# - derived as (open) data from: direct database export courtesy of NABEL (BAFU & Empa) (can in principal be downloaded as open data from https://www.bafu.admin.ch/bafu/de/home/themen/luft/zustand/daten/datenabfrage-nabel.html, but only for 6 months per query)
files <- list()
files$monitoring$nabel$due_h1 <- "inst/extdata/input_offline/nabel_tse_due_h1.txt"
files$monitoring$nabel$zue_h1 <- "inst/extdata/input_offline/nabel_tse_zue_h1.txt"

# Selected ozone (O3) monitoring data Kanton Zürich from intercantonal monitoring network OSTLUFT (hourly values)
# - source: Ostluft
# - derived as (open) data from: direct export by OSTLUFT (can be in principal downloaded as open data from https://www.ostluft.ch/index.php?id=datenabfragen, but only for smaller queries)
files$monitoring$ostluft$h1 <- "inst/extdata/input_offline/ostluft_airmo_h1_kanton_zurich.csv"




# read datasets ...
# => read NABEL h1 monitoring airquality O3 data
data_monitoring_nabel_h1 <- lapply(files$monitoring$nabel, function(x) read_local_csv(x, delim = "\t"))

# => read Ostluft h1 monitoring airquality O3 data
data_monitoring_ostluft_h1 <- read_local_csv(files$monitoring$ostluft$h1, locale = readr::locale(encoding = "UTF-8"), col_names = FALSE)

# prepare datasets ...
data_monitoring_nabel_h1 <- 
  data_monitoring_nabel_h1 |> 
  lapply(restructure_monitoring_nabel_h1) |> 
  dplyr::bind_rows() |> 
  calc_O3_peakseason() |>
  dplyr::mutate(source = factor("NABEL (BAFU & Empa)"))

data_monitoring_ostluft_h1 <- 
  data_monitoring_ostluft_h1 |> 
  restructure_monitoring_ostluft() |> 
  calc_O3_peakseason() |>
  dplyr::mutate(source = factor("Ostluft"))

data_monitoring_o3_peakseason <- 
  data_monitoring_ostluft_h1 |>
  dplyr::bind_rows(data_monitoring_nabel_h1) |> 
  dplyr::mutate(starttime = format(starttime, "%Y-%m-%d %H:%M:%S"))
  



# write output datasets & clean up:
# ---
write_local_csv(data_monitoring_o3_peakseason, file = "inst/extdata/input/ostluft_nabel_compiled_o3_peakseason_y1.csv")
rm(list = c("files", "data_monitoring_nabel_h1", "data_monitoring_ostluft_h1", "data_monitoring_o3_peakseason"))

