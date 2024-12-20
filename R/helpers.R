#' Get entry from ressources.csv by internal id
#'
#' @param ressources 
#' @param internal_id 
#'
#' @keywords internal
filter_ressources <- function(ressources, internal_id) {
  
  filters <- paste0("INTERNAL_ID == ", internal_id)
  ressource <- dplyr::filter(ressources, eval(rlang::parse_expr(filters)))
  ressource <- dplyr::pull(ressource, get)
  
  return(ressource)
}


#' Extract target threshold values from threshold table for plotting
#'
#' @param threshold_values 
#' @param pollutant 
#' @param aggregation 
#' @param metric 
#' @param unit 
#' @param source 
#'
#' @keywords internal
extract_threshold <- function(threshold_values, pollutant = NULL, metric = "Jahresmittel", interval = "y1", unit = "µg/m3", 
                          source = c("LRV Grenzwert", "WHO Richtwert")) {

  thresholds <-
    threshold_values |>
    dplyr::filter(
      source %in% !!source &
        pollutant == !!pollutant & interval == !!interval &
        metric == !!metric & unit == !!unit
    ) |>
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



#' Recode short pollutant/parameter string to long string
#'
#' @param x 
#'
#' @keywords internal
longpollutant <- function(x) {
  
  long <- dplyr::case_when(
    x == "PM10" ~ "Feinstaub PM10",
    x == "PM2.5" ~ "Feinstaub PM2.5",
    x == "NO2" ~ "Stickstoffdioxid",
    x == "NMVOC" ~ "nicht-Methan Kohlenwasserstoffe",
    x == "NH3" ~ "Ammoniak",
    x == "CO" ~ "Kohlenstoffmonoxid",
    x == "SO2" ~ "Schwefeldioxîd",
    x == "NOx" ~ "Stickoxide",
    x == "eBC" ~ "Russ",
    x == "O3" ~ "Ozon", 
    x == "O3_max_98p_m1" ~ "Ozon",
    x == "O3_peakseason_mean_d1_max_mean_h8gl" ~ "Ozon",
    x == "O3_nb_h1>120" ~ "Ozon",
    x == "N-Eintrag" ~ "Stickstoffeintrag in empfindliche Ökosysteme",
    TRUE ~ x
  )
  
  return(long)
}



#' Recode parameter string to short pollutant string
#'
#' @param x 
#'
#' @keywords internal
shortpollutant <- function(x) {
  
  long <- dplyr::case_when(
    x == "O3_max_98p_m1" ~ "O3",
    x == "O3_peakseason_mean_d1_max_mean_h8gl" ~ "O3",
    x == "O3_nb_h1>120" ~ "O3",
    TRUE ~ x
  )
  
  return(long)
}


#' Recode short pollutant/parameter string to long metric string
#'
#' @param x 
#'
#' @keywords internal
longmetric <- function(x) {
  
  long <- dplyr::case_when(
    x == "PM10" ~ "Jahresmittel",
    x == "PM2.5" ~ "Jahresmittel",
    x == "NO2" ~ "Jahresmittel",
    x == "NMVOC" ~ "Jahresmittel",
    x == "NH3" ~ "Jahresmittel",
    x == "CO" ~ "Jahresmittel",
    x == "SO2" ~ "Jahresmittel",
    x == "NOx" ~ "Jahresmittel",
    x == "eBC" ~ "Jahresmittel",
    x == "O3" ~ "Jahresmittel", 
    x == "O3_max_98p_m1" ~ "höchstes monatl. 98%-Perzentil der ½-Stundenmittel",
    x == "O3_peakseason_mean_d1_max_mean_h8gl" ~ "mittlere sommerliche Tagesbelastung",
    x == "O3_nb_h1>120" ~ "Anzahl Stundenmittel > 120 μg/m3", 
    TRUE ~ x
  )
  
  return(long)
}


#' Recode short parameter string to long metric string
#'
#' @param x 
#'
#' @keywords internal
longparameter <- function(x) {
  
  long <- dplyr::case_when(
    x == "O3_max_98p_m1" ~ "max. monatl. 98%-Perz.",
    x == "O3_peakseason_mean_d1_max_mean_h8gl" ~ "Sommersaison",
    TRUE ~ "Jahresmittel"
  )
  
  return(long)
}


#' Extract numeric year from string
#'
#' @param string 
#'
#' @keywords internal
extract_year <- function(string) {as.numeric(stringr::str_extract(string, "(1|2)[0-9]{3}"))}


#' Return function to bin concentration depending on parameter
#'
#' @param pollutant 
#'
#' @keywords internal
bin_fun <- function(pollutant) {
  
  fun <- function(x) {floor(x) + 0.5} # default, e.g. NO2: abgerundet auf 1, Klassenmitte
  if (pollutant == "O3_max_98p_m1") {fun <- function(x) {floor(x * 2) / 2 + 1}} # abgerundet auf 2, Klassenmitte
  if (pollutant == "O3_peakseason_mean_d1_max_mean_h8gl") {fun <- function(x) {floor(x * 2) / 2 + 1}} # abgerundet auf 2, Klassenmitte
  if (pollutant == "PM10") {fun <- function(x) {floor(x * 5) / 5 + 0.1}} # abgerundet auf 0.2, Klassenmitte
  if (pollutant == "PM2.5") {fun <- function(x) {floor(x * 5) / 5 + 0.1}} # abgerundet auf 0.2, Klassenmitte
  if (pollutant == "eBC") {fun <- function(x) {floor(x * 20) / 20 + 0.025}} # abgerundet auf 0.05, Klassenmitte
  
  return(fun)
}



#' Write local *csv 
#'
#' @param data 
#' @param file 
#' @param delim 
#' @param na 
#'
#' @keywords internal
write_local_csv <- function(data, file, delim = ";", na = "NA"){
  
  readr::write_delim(data, file, delim = delim, na = na)

}


#' Round to nearest whole number (kaufmännisches Runden)
#'
#' @param x 
#' @param digits 
#'
#' @keywords internal
round_off <- function (x, digits = 0) {
  
  posneg = sign(x)
  z = trunc(abs(x) * 10 ^ (digits + 1)) / 10
  z = floor(z * posneg + 0.5) / 10 ^ digits
  
  return(z)
}



#' Converts coverage stack from geolion api request of air pollutant raster data to unambiguous matrix of coverages
#'
#' @param cov_stack 
#' @param years_pollumap 
#'
#' @keywords internal
filter_availability <- function(cov_stack, years_pollumap = 2015) {
  
  data_availability <- 
    cov_stack |> 
    to_stack_df() |> 
    dplyr::filter(
      (!stringr::str_detect(layer_name, "jahre") & as.numeric(year) %in% years_pollumap) | # only select pollumap for the year in which it calibrated with monitoring data
        as.numeric(year) < lubridate::year(Sys.Date()) & # no future pollumap projections
        stringr::str_detect(layer_name, "jahre") # apart from that: always use jahreskarte
    ) |> 
    dplyr::filter(pollutant != "bc") # no bc since this only available for pollumap
  
  return(data_availability)
}



