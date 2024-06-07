
add_to_maplist <- function(capabilities, maplist, target_source, target_parameter) {

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



extract_from_capabilitylist <- function(capablilitylist, maplist, coverage, parameter) {
  
  product <- unlist(strsplit(names(maplist[maplist == coverage]), split = ".", fixed = TRUE))[1]
  capabilities <- capablilitylist[[product]]
  if (extract_year(coverage) != 2015) {capabilities <- capabilities[[tolower(parameter)]]} 
  
  return(capabilities)
}



# function to extract target threshold values from overall threshold data for plotting with ggplot_timeseries()
extract_threshold <- function(threshold_values, pollutant = NULL, aggregation = "y1", metric = "mean", unit = "µg/m3", 
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



set_year <- function(maps) setNames(as.character(unique(extract_year(maps))), unique(extract_year(maps)))




bin_fun <- function(pollutant) {
  
  fun <- function(x) {floor(x) + 0.5} # default: abgerundet auf 1, Klassenmitte
  if (pollutant == "PM10") {fun <- function(x) {floor(x * 5) / 5 + 0.1}} # abgerundet auf 0.2, Klassenmitte
  if (pollutant == "PM2.5") {fun <- function(x) {floor(x * 5) / 5 + 0.1}} # abgerundet auf 0.2, Klassenmitte
  if (pollutant == "eBC") {fun <- function(x) {floor(x * 20) / 20 + 0.025}} # abgerundet auf 0.05, Klassenmitte
  
  return(fun)
}


