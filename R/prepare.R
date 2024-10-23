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