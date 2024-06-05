# add Langzeitbelastungsindex LBI
# ... currently, LBI needs to be revised by Cercl'Air => prepared for later inclusion
immission_threshold_values <- readr::read_delim(paste("inst/extdata", files$airquality$thresh, sep = "/"), delim = ";",locale = readr::locale(encoding = "UTF-8"))
data_monitoring_aq <-
  data_monitoring_aq %>%
  calc_lbi(immission_threshold_values) %>%
  dplyr::bind_rows(data_monitoring_aq)

