# add Langzeitbelastungsindex LBI
# ... currently, LBI needs to be revised by Cercl'Air => prepared for later inclusion
immission_threshold_values <- readr::read_delim(paste("inst/extdata", files$airquality$thresh, sep = "/"), delim = ";",locale = readr::locale(encoding = "UTF-8"))
data_monitoring_aq <-
  data_monitoring_aq %>%
  calc_lbi(immission_threshold_values) %>%
  dplyr::bind_rows(data_monitoring_aq)





# request available ogd datasets:
req <- httr2::request("https://opendata.swiss/api/3/action/package_list")
req <- httr2::req_perform(req)
opendatasets <- unlist(httr2::resp_body_json(req)$result) # all available datasets
opendatasets[stringr::str_detect(opendatasets, "luftschadstoffemissionen-im-kanton-zurich")] # => exists
opendatasets[stringr::str_detect(opendatasets, "messdaten-langjahriger-abgasmessungen-im-realen-fahrbetrieb-mittels-remote-sensing-rsd")] # => exists
