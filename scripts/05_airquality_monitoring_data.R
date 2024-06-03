
### pollutant monitoring data 
### (time series and threshold comparisons)
### ------------------------------------------------------------

### read and restructure NABEL monitoring site metadata

# FIXME: fs package wirklich nötig? 
site_meta <-
  fs::path("data/input", files$airquality$monitoring$nabel_y1) %>%
  get_nabel_meta_arias() %>%
  dplyr::mutate(
    site_long = site,
    zone = ifelse(zone == "vorstädtisch", "klein-/vorstädtisch", zone),
    source = "NABEL (BAFU & Empa)"
  )

### read and restructure OSTLUFT site metadata

site_meta <-
  fs::path("data/input", files$airquality$monitoring$ostluft_meta)  %>%
  read_ostluft_meta() %>%
  dplyr::mutate(
    zone = aggregate_ostluft_meta_zone(zone),
    type = aggregate_ostluft_meta_type(type),
    source = "OSTLUFT"
  ) %>%
  dplyr::bind_rows(site_meta) %>%
  dplyr::mutate(siteclass = factor(paste(zone, type, sep = " - "),
                                   levels = rev(c("ländlich - Hintergrund", "ländlich - verkehrsbelastet", "klein-/vorstädtisch - Hintergrund",
                                                  "klein-/vorstädtisch - verkehrsbelastet", "städtisch - Hintergrund", "städtisch - verkehrsbelastet")))) %>%
  dplyr::select(-zone, -type)

### empty dataset to be consecutively filled with monitoring data

data <- tibble::tibble()

### read and restructure NABEL y1 data

# FIXME: würde zuerst alle datasets vorbereiten und am schluss das bind rows machen

data <-
  fs::path("data/input", files$airquality$monitoring$nabel_y1) %>%
  read_arias() %>%
  dplyr::mutate(source = factor("NABEL (BAFU & Empa)")) %>%
  dplyr::bind_rows(data)

### read and restructure OSTLUFT y1 data

data <-
  fs::path("data/input", files$airquality$monitoring$ostluft_y1) %>%
  read_airmo_csv2() %>%
  remove_duplicate_y1() %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(
    parameter = dplyr::recode_factor(parameter, !!!c("NO2_PS" = "NO2", "PM10h" = "PM10", "PM2.5h" = "PM2.5")),
    source = factor("OSTLUFT")
  ) %>%
  dplyr::bind_rows(data)

### read pre-compiled OSTLUFT & NABEL O3 peak-season y1 data

data <-
  fs::path("data/input", files$airquality$monitoring$ostluft_nabel_peakseason_y1) %>% 
  readr::read_delim(delim = ";", locale = readr::locale(tz = "Etc/GMT-1")) %>%
  dplyr::bind_rows(data)

### read OSTLUFT y1 data for nitrogen deposition to sensitive ecosystems

data_ndep <-
  fs::path("data/input", files$airquality$monitoring$ostluft_ndep_y1) %>%
  readr::read_delim(delim = ";")

### finalise dataset and join with site metadata

data <-
  data %>%
  # dplyr::bind_rows(calc_lbi(data, threshold_values)) %>% # add Langzeitbelastungsindex LBI ... currently, LBI needs to be revised by Cercl'Air => prepared for later inclusion
  dplyr::filter(lubridate::year(starttime) %in% years & parameter %in% c(parameters, "LBI")) %>% # filter for target years and parameter
  rOstluft::pad() %>%  # pad to complete timeseries for better plotting
  dplyr::select(-source) %>%
  dplyr::left_join(site_meta, by = "site") %>%
  dplyr::filter(!is.na(siteclass))




### write datasets
### ------------------------------------------------------------

data %>%
  dplyr::arrange(site, parameter, starttime) %>%
  dplyr::mutate(starttime = format(starttime, "%Y-%m-%d %H:%M:%S")) %>%
  readr::write_delim(file = "data/output/data_airquality_monitoring_y1.csv", delim = ";", na = "NA")




### clean up
### ------------------------------------------------------------
# ...

