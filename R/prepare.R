prepare_emmissions <- function(data, filter_args = canton == 'ZH' & emission != 0 & !(subsector %in% c('Weitere Punktquellen OL', 'Rheinschifffahrt', 'Flugverkehr Genf'))){
  
  filter_args <- rlang::enquo(filter_args)
  data_prep <- 
    data |> 
    dplyr::rename(
      year = jahr,
      pollutant = substanz,
      sector = hauptgruppe,
      subsector = untergruppe,
      canton = kanton,
      municipality = gemeinde, 
      unit = einheit
    ) |> 
    dplyr::mutate(
      pollutant =ifelse(pollutant == "BC", "eBC", pollutant)
    ) |> 
    dplyr::filter(!!filter_args)
  
  return(data_prep)
}


prepare_rsd <- function(data, rsd_auxiliary){
  
  rsd_meta <- rsd_auxiliary$meta
  rsd_filters <- rsd_auxiliary$filters
  rsd_filters$max[which(rsd_filters$parameter == "vehicleyears")] <- lubridate::year(Sys.Date()) # include most recent vehicle model years
  
  # calculate vehicle specific power from measurement data subset and merge with RSD dataset
  data_vsp <- prep_vehicle_specific_power(data)
  data_rsd <-
    data |>
    dplyr::select(-unit) |> 
    dplyr::filter(!(parameter %in% c("acceleration", "velocity"))) |>
    dplyr::left_join(data_vsp, by = "id") 
  
  # apply data filters for a meaningful analysis
  data_rsd <- filter_rsd(data_rsd, rsd_filters)
  
  # restructure and merge with Euronorm metadata 
  data_rsd <- merge_restructure_rsd(data_rsd, rsd_meta)
  
  # calculate NOx emissions
  data_rsd <- dplyr::mutate(data_rsd, nox_emission = calc_rsd_nox_emission(NO = NO / 10^4, p = fraction_no2_hbefa, CO2 = CO2, CO = CO, HC = HC / 10^4)) # input: concentrations all in percent; originally: NO in ppm, CO2 in %, CO in %, HC in ppm; output: NOx emissions in g/kg fuel;  add HBEFA-derived NO2 and use that for NOx emission calculation rather than measured NO2 since that has only been available since RSD-model 4500
  
  return(data_rsd)
}


prepare_ressources <- function(ressources) {
  
  ressources <- 
    ressources |> 
    dplyr::mutate(
      get = dplyr::case_when(
        stringr::str_detect(DOWNLOAD_URL, "inst/extdata") ~ paste(DOWNLOAD_URL, DATASET_NAME, sep = "/"),
        DOWNLOAD_SOURCE == "swisstopo" ~ DATASET_NAME,
        TRUE ~ DOWNLOAD_URL
      )
    )
  
  return(ressources)
}


prepare_monitoring_meta <- function(meta_ostluft, meta_nabel) {

  meta_ostluft <- prep_site_meta_ostluft(meta_ostluft)
  meta_nabel <- prep_site_meta_nabel(meta_nabel)
  
  meta <- 
    meta_ostluft |> 
    dplyr::bind_rows(meta_nabel) |> 
    dplyr::mutate(siteclass = paste(zone, type, sep = " - ")) |>
    dplyr::select(-zone, -type)
  
  return(meta)
}


# function to prepare tibble with air pollutant year-statistics exported and read from https://www.arias.ch/ibonline/ib_online.php and restructure the data similar to a standard long-format (see rOstluft::format_rolf())
prepare_monitoring_nabel_y1 <- function(data, keep_incomplete = FALSE, tz = "Etc/GMT-1") {
  
  data <- 
    data |> 
    restructure_monitoring_nabel() |> 
    dplyr::mutate(
      interval = "y1",
      Schadstoff = dplyr::case_when(
        Messparameter == "höchster 98%-Wert eines Monats" ~ "O3_max_98p_m1",
        Messparameter == "Anzahl Stundenmittel > 120 µg/m3" ~ "O3_nb_h1>120",
        Messparameter == "Dosis AOT40f" ~ "O3_AOT40",
        Schadstoff == "Partikelanzahl" ~ "PN",
        Schadstoff == "EC / Russ" ~ "eBC",
        TRUE ~ Schadstoff
      ),
      Einheit = ifelse(Einheit == "ppm·h", "ppm*h", Einheit),
      starttime = as.POSIXct(paste0(starttime, "-01-01"), tz = tz),
      source = factor("NABEL (BAFU & Empa)")
    ) |> 
    dplyr::select(
      starttime,
      site = Station,
      parameter = Schadstoff,
      interval,
      unit = Einheit,
      value,
      source
    ) |> 
    dplyr::mutate_if(is.character, as.factor)
  
  return(data)
}



prepare_monitoring_ostluft_y1 <- function(data, keep_incomplete = FALSE, tz = "Etc/GMT-1") {
  
  data <- restructure_monitoring_ostluft(data, keep_incomplete = keep_incomplete, tz = tz, na.rm = TRUE) 
  
  # remove duplicate parameters in Ostluft data => use preferred method (NO2 passive samplers vs. monitor & PM10/PM2.5 HVS vs. monitor)
  data <- remove_duplicate_y1(data)
  
  data <- dplyr::mutate(data, source = factor("ostluft"))
  
  return(data)
}


prepare_monitoring_aq <- function(data, meta) { 
  
  data <- 
    data |> 
    pad2() |>
    dplyr::mutate(
      year = lubridate::year(starttime),
      parameter = dplyr::recode(parameter, "O3_max_98%_m1" = "O3_max_98p_m1") # for technical reasons
    ) |> 
    dplyr::select(-source) |>
    dplyr::left_join(meta, by = "site") |>
    dplyr::filter(!is.na(siteclass)) |> 
    dplyr::arrange(site, parameter, starttime) |> 
    dplyr::select(year, site, site_long, siteclass, x, y, masl, source, parameter, interval, unit, value)
  
  return(data)
}



