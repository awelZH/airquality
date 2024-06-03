### internal function for read_airmo_csv2() to restructure data; based on similar function in rOstluft-package
airmo_wide_to_long2 <- function(header, data, tz = "Etc/GMT-1", na.rm = TRUE){
  
  colnames(data)[1] <- "starttime"
  col_ids <- rlang::names2(data)[-1]
  
  
  # FIXME: kann das vereinfacht werden mit pivot_longer? sollte eigentlich möglich sein
  sites <- c(header[1, ], recursive = TRUE)
  sites <- rlang::set_names(sites, col_ids)
  parameters <- c(header[2, ], recursive = TRUE)
  parameters <- rlang::set_names(parameters, col_ids)
  intervals <- c(header[3, ], recursive = TRUE)
  intervals <- rlang::set_names(intervals, col_ids)
  units <- c(header[4, ], recursive = TRUE)
  units <- rlang::set_names(units, col_ids)
  
  data <- dplyr::mutate(data, starttime = lubridate::parse_date_time(.data$starttime, c("dmYHMS", "dmYHM", "dmY"), tz = tz))
  data <- tidyr::gather(data, "id", "value", -"starttime", na.rm = na.rm, factor_key = TRUE)
  data <- dplyr::mutate(data,
                        site = dplyr::recode(.data$id, !!!sites),
                        parameter = dplyr::recode(.data$id, !!!parameters),
                        interval = dplyr::recode(.data$id, !!!intervals),
                        unit = dplyr::recode(.data$id, !!!units)
  )
  data <- dplyr::select(data, "starttime", "site", "parameter", "interval", "unit", "value")
  
  return(data)
}





read_statpop_csv <- function(file, year, crs = 2056) {
  
  var <- paste0("B", stringr::str_sub(year, 3, 4), "BTOT")
  delim <- ifelse(as.numeric(year) > 2015, ";", ",")
  data <- 
    file %>% 
    readr::read_delim(delim = delim, locale = readr::locale(encoding = "UTF-8")) %>% 
    dplyr::select(RELI, E_KOORD, N_KOORD, !!var) %>% 
    dplyr::rename(
      x = E_KOORD,
      y = N_KOORD,
      population = !!var
    ) %>% 
    # dplyr::mutate(year = as.numeric(year)) %>% 
    # sf::st_as_sf(coords = c("x", "y", "year"), dim = "XYZ") %>% 
    sf::st_as_sf(coords = c("x", "y"), dim = "XY") %>%
    sf::st_set_crs(value = crs) %>% 
    stars::st_rasterize() 
  
  return(data)
}




### function to extract target threshold values from overall threshold data for plotting with ggplot_timeseries()
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





