
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




### function to make sure that there are no duplicate measurements per site / year / unit for data with interval = y1 in format rOstluft::format_rolf() 
### in case there have been NO2 monitor and passive sampler measurements (prefer monitor data = reference method); 
### same for PM10 monitor and high volume sampler measurements (prefer high-volume-sampler data = reference method);
### same for PM2.5 monitor and high volume sampler measurements (prefer high-volume-sampler data = reference method)
remove_duplicate_y1 <- function(data){
  
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
    dplyr::ungroup()
  
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





