# content %>%
#   dplyr::filter(site %in% sites & interval == "h1" & year %in% 2018:2021 & parameter == "O3" & unit == "µg/m3") %>%
  # # dplyr::filter(site %in% sites & interval == "d1" & year %in% 2018:2021 & parameter %in% c("PM2.5", "PM10", "NO2") & unit == "µg/m3") %>%
  # # dplyr::distinct(site, parameter) %>%
  # View
# content %>%
#   dplyr::filter(site %in% sites & interval == "d1" & year %in% 2018:2021 & unit == "µg/m3") %>% View
  


### get d1 for PM2.5, PM10, PM2.5 & h1 for O3
d1 <- aqmet$get(site = sites, year = years, interval = "d1", filter = parameter %in% c("PM2.5", "PM10", "NO2") & unit == "µg/m3")
d2 <- aqmet$get(site = sites, year = years, interval = "h1", filter = parameter == "O3" & unit == "µg/m3")
d3 <- aqmet$get(site = sites, year = years, interval = "m1", filter = parameter == "O3" & unit == "µg/m3")



### calculate nb > WHO AQG for interval d1
get_statistic_limit <- function(limit) {
  function(x) {
    if (all(is.na(x))) {
      NA
    } else {
      sum(x > limit, na.rm = TRUE)
    }
  }
  
}

d1a <- 
  d1 %>% 
  dplyr::filter(parameter == "PM10") %>% 
  resample(get_statistic_limit(45), new_interval = "y1", data_thresh = 0.9) %>% 
  mutate(parameter = factor("PM10_nb_d1>45"), unit = factor(1))

d1b <- 
  d1 %>% 
  dplyr::filter(parameter == "PM2.5") %>% 
  resample(get_statistic_limit(15), new_interval = "y1", data_thresh = 0.9) %>% 
  mutate(parameter = factor("PM2.5_nb_d1>15"), unit = factor(1))

d1c <-
  d1 %>% 
  resample(get_statistic_limit(25), new_interval = "y1", data_thresh = 0.9) %>% 
  mutate(parameter = factor("NO2_nb_d1>25"), unit = factor(1))



### calculate peak season for O3
### ... own definition due to unclear WHO definition: 
# 1) Peak season Monate auf Stundenmittelbasis bestimmen (= analog AOT40, das sich auch über eine "Saison" erstreckt, 
# da gibt die Messempfehlung auch Datenverfügbarkeitskriterien vor: 90% aller Stunden => Basis Stundenmittel würde auch erlauben, 
# beide Rechnungsschritte auf einer Grundbasis - nämlich h1 - vorzunehmen). Dabei 2x gleitend über die Monate iterieren, 
# einmal linksbündig, einmal rechtsbündig, denn schlussendlich geht es um das Identifizieren der zusammenhängenden Monate 
# im Jahr mit dem höchsten Mittel - das würde so sichergestellt. 
# 2) pro Tag: O3_max_h8gl (mit Intervall d1) berechnen
# 3) O3_max_h8gl über die peak season mitteln (falls Stundenmittelverfügbarkeit > 90%)


# function identifying relevant months per year for metric calculation 
consecutive_months <- function(starttime, o3_m1) {
  data <- tibble::tibble(starttime, O3 = o3_m1) 
  data <- dplyr::arrange(data, starttime) 
  data <- dplyr::mutate(data, 
                         O3_runmean = zoo::rollapply(.data$O3, 6, mean, fill = NA, align = "left"),
                         month_start = month(data$starttime),
                         month_end = pmin(month(data$starttime) + 5, 12),
                         n_months = month_end - month_start + 1
                         )
  complete_months <- dplyr::filter(data, n_months == 6)
  peak_start <- dplyr::pull(dplyr::slice(complete_months, which.max(.data$O3_runmean)), starttime)
  if (length(peak_start) == 0) {
    peak_season <- rep(NA, nrow(data))
  } else {
    peak_season <- data$starttime %in% (peak_start + months(0:5))
  }
  return(peak_season)
} 




# make sure years are sufficiently data-covered 
# identify consecutive months per year and site  
nmin <- 11/12
coverage <- 
  d3 %>% 
  dplyr::group_by(year = year(lubridate::floor_date(starttime, unit = "1 year")), site) %>% 
  dplyr::summarise(n = sum(!is.na(value))) %>% 
  dplyr::ungroup() 
  
peak_season <- 
  d3 %>% 
  dplyr::mutate(year = year(lubridate::floor_date(starttime, unit = "1 year"))) %>% 
  dplyr::left_join(coverage, by = c("year", "site")) %>% 
  dplyr::filter(n/12 >= nmin) %>% 
  dplyr::group_by(year = lubridate::floor_date(starttime, unit = "1 year"), site) %>%
  dplyr::mutate(peak_season = consecutive_months(starttime, value)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(starttime, site, peak_season)


# calc daily max h8gl from h1 O3
d2 <- rOstluft::resample(d2, "mean", new_interval = "h8gl")
d2 <- rOstluft::resample(d2, "max", new_interval = "d1")


# join with "max_h8gl" data and filter only days within relevant consecutive months 
# calc mean per year, site and consecutive months (= WHO metric) 
d2a <-
  d2 %>% 
  left_join(peak_season, by = c("starttime", "site")) %>% 
    dplyr::filter(peak_season) %>% 
    dplyr::group_by(starttime = lubridate::floor_date(starttime, unit = "1 year"), site) %>% 
    dplyr::summarise(value = mean(value)) %>% 
    ungroup() %>% 
    dplyr::mutate(
      parameter = "O3_mean_d1_max_h8gl_peak_season",
      interval = "y1",
      unit = "µg/m3"
    ) %>% 
    mutate_if(is.character, factor) %>% 
    dplyr::select(starttime, site, parameter, interval, unit, value)
    

# # ... 
# require(ggplot2) 
# ggplot(d2b, aes(x = starttime, y = value, color = site)) + 
#   geom_hline(yintercept = 60) + 
#   geom_line() + 
#   geom_point() +
#   scale_x_datetime(breaks = "2 years", date_labels = "%Y") + 
#   scale_y_continuous(limits = c(0,NA)) 



### merge to a WHO-related dataset and save analysis-ready data as *.rds
data_WHO <- 
  d1a %>% 
  bind_rows(d1b) %>% 
  bind_rows(d1c) %>% 
  bind_rows(d2a) %>% 
  dplyr::filter(site != "Payerne") %>% 
  mutate(year = year(starttime))

data_WHO <-
  meta %>% 
  dplyr::select(site, site2, Standortklasse, year) %>%
  right_join(data_WHO, by = c("site", "year")) %>% 
  # dplyr::distinct(site, Standortklasse, parameter, year, .keep_all = TRUE) %>%
  dplyr::select(-year) %>% 
  dplyr::filter(!is.na(Standortklasse)) %>% 
  mutate_if(is.character, factor)


saveRDS(data_WHO, "data/data_WHO.rds")


