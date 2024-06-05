
add_to_maplist <- function(maplist, target_source, target_parameter) {
  
  if (target_source == "pollumap") {
    
    coverage_summary <- capabilities[[target_source]]$getCoverageSummaries()
    coverages <- purrr::map_chr(coverage_summary, function(x){x$CoverageId})
    maplist <- purrr::list_modify(maplist, !!target_source := coverages)
    maplist[[target_source]]<- setNames(maplist[[target_source]], extract_year(maplist[[target_source]]))
    
  } else {
    
    coverage_summary <- capabilities[[target_source]][[target_parameter]]$getCoverageSummaries()
    coverages <- purrr::map_chr(coverage_summary, function(x){x$CoverageId})
    temp <- list()
    temp <- purrr::list_modify(temp, !!target_parameter := coverages)
    maplist <- purrr::list_modify(maplist, !!target_source := temp)
    maplist[[target_source]][[target_parameter]] <- setNames(maplist[[target_source]][[target_parameter]], extract_year(maplist[[target_source]][[target_parameter]]))
    
  }
  
  return(maplist)
}



get_geolion_wcs_capabilities_from_list <- function(capablilitylist, maplist, coverage, parameter) {
  
  product <- unlist(strsplit(names(maplist[maplist == coverage]), split = ".", fixed = TRUE))[1]
  capability <- capablilitylist[[product]]
  if (extract_year(coverage) != 2015) {capability <- capability[[tolower(parameter)]]} 
  
  return(capability)
}




# function to make sure that there are no duplicate measurements per site / year / unit for data with interval = y1 in format rOstluft::format_rolf() 
# in case there have been NO2 monitor and passive sampler measurements (prefer monitor data = reference method); 
# same for PM10 monitor and high volume sampler measurements (prefer high-volume-sampler data = reference method);
# same for PM2.5 monitor and high volume sampler measurements (prefer high-volume-sampler data = reference method)
remove_duplicate_y1 <- function(data){
  
  # FIXME: funktion welche den Parameter als input hat ;)
  # FIXME: etwas stimmt hier noch nicht, was ist parameter?
  replace_no2_ps <- function(parameter, value){
    if (sum(c("NO2", "NO2_PS") %in% parameter) == 2) {
      if (!is.na(value[which(parameter == "NO2")])){
        value[which(parameter == "NO2_PS")] <- NA
      }
    }
    return(value)
  }
  
  replace_pm10 <- function(parameter, value){
    if (sum(c("PM10", "PM10h") %in% parameter) == 2) {
      if (!is.na(value[which(parameter == "PM10h")])){
        value[which(parameter == "PM10")] <- NA
      }
    }
    return(value)
  }
  
  replace_pm25 <- function(parameter, value){
    if (sum(c("PM2.5", "PM2.5h") %in% parameter) == 2) {
      if (!is.na(value[which(parameter == "PM2.5h")])){
        value[which(parameter == "PM2.5")] <- NA
      }
    }
    return(value)
  }
  
  data <- 
    data %>% 
    dplyr::group_by(starttime, site, unit) %>% 
    dplyr::mutate(
      value = replace_no2_ps(parameter, value),
      value = replace_pm10(parameter, value),
      value = replace_pm25(parameter, value)
    ) %>% 
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(parameter = dplyr::recode_factor(parameter, !!!c("NO2_PS" = "NO2", "PM10h" = "PM10", "PM2.5h" = "PM2.5")))
  
  return(data)
}




# function to extract target threshold values from overall threshold data for plotting with ggplot_timeseries()
get_threshold <- function(threshold_values, pollutant = NULL, aggregation = "y1", metric = "mean", unit = "µg/m3", 
                          source = c("LRV Grenzwert", "WHO Richtwert")) {
  
  thresholds <-
    threshold_values %>%
    dplyr::filter(
      source %in% !!source &
        pollutant == !!pollutant & aggregation == !!aggregation &
        metric == !!metric & unit == !!unit
    ) %>%
    dplyr::arrange(source)
  
  thresholds <-
    list(
      value = thresholds$threshold,
      color = thresholds$col,
      labels = thresholds$source,
      labelsize = thresholds$lbsz,
      linetype = thresholds$lty,
      linesize = thresholds$lsz
    )
  
  return(thresholds)
}




combine_thresholds <- function(data, threshold_values) {
  
  data <- 
    threshold_values %>% 
    dplyr::select(source, pollutant, metric, aggregation, threshold) %>% 
    dplyr::rename(
      parameter = pollutant,
      interval = aggregation
    ) %>% 
    dplyr::mutate(
      parameter = dplyr::case_when(
        metric == "number hourly mean values > 120 µg/m3" & parameter == "O3" ~ "O3_nb_h1>120",
        metric == "monthly 98%-percentile of ½ hour mean values ≤ 100 µg/m3" & parameter == "O3" ~ "O3_max_98%_m1",
        metric == "mean of daily maximum 8-hour mean concentration in the six consecutive months with the highest six-month running-mean concentration" & parameter == "O3" ~ "O3_peakseason_mean_d1_max_mean_h8gl",
        TRUE ~ parameter
      ),
      interval = dplyr::recode(interval, !!!c("m1" = "y1", "peak-season" = "y1"))
    ) %>% 
    dplyr::select(-metric) %>% 
    tidyr::spread(source, threshold) %>% 
    dplyr::right_join(data, by = c("parameter", "interval")) %>% 
    dplyr::select(starttime, site, parameter, interval, unit, value, siteclass, `LRV Grenzwert`, `WHO Richtwert`, source)
  
  return(data)
  
}



longtitle <- function(x) {
  
  long <- dplyr::case_when(
    x == "PM10" ~ "Feinstaub PM10",
    x == "PM2.5" ~ "Feinstaub PM2.5",
    x == "NMVOC" ~ "nicht-Methan Kohlenwasserstoffe",
    x == "NH3" ~ "Ammoniak",
    x == "CO" ~ "Kohlenstoffmonoxid",
    x == "SO2" ~ "Schwefeldioxîd",
    x == "NOx" ~ "Stickoxide",
    x == "eBC" ~ "Russ",
    TRUE ~ x
  )
  
  return(long)
}



extract_year <- function(string) {as.numeric(stringr::str_extract(string, "(1|2)[0-9]{3}"))}




# copy from rOstluft::convert_interval()
convert_interval2 <- function(interval) {
  
  num <- stringr::str_extract(interval, "[:digit:]+")
  units <- stringr::str_extract(interval, "[:alpha:]+")
  units <- stringr::str_to_lower(units)
  if (is.na(num)) num <- "1"
  if (units == "m") units <- "month"
  if (units == "y") units <- "year"
  
  stringr::str_c(num, units, sep = " ")
}



# copy from rOstluft::pad_serie()
pad_serie2 <- function(serie, start_date = NULL, end_date = NULL, drop_last = FALSE) {
  
  if (is.null(start_date)) {
    start_date <- min(serie$starttime)
  }
  
  if (is.null(end_date)) {
    end_date <- max(serie$starttime)
    drop_last <- FALSE
  }
  
  # by joining the data we insert rows with NA values for site, parameter, interval, unit, value
  # we need to fill this with the values from the supplied df
  fill.values <- dplyr::slice(serie, 1)
  fill.values <- as.list(dplyr::select(fill.values, -"starttime", -"value"))
  
  interval <- convert_interval2(fill.values$interval)
  
  all.dates <- tibble::tibble(
    starttime = seq(start_date, end_date, interval)
  )
  
  if (isTRUE(drop_last)) {
    all.dates <- utils::head(all.dates, -1)
  }
  
  padded <- dplyr::full_join(all.dates, serie, by = "starttime")
  tidyr::replace_na(padded, replace = fill.values)
}



# copy from rOstluft::pad() => because this is the only function we need from this package
pad2 <- function(data, start_date = NULL, end_date = NULL, drop_last = FALSE) {
  
  data.grouped <- dplyr::group_by(data, .data$site, .data$parameter, .data$interval, .data$unit)
  data.grouped <- dplyr::do(data.grouped, pad_serie2(.data, start_date, end_date, drop_last))
  
  return(dplyr::ungroup(data.grouped))
}

