

# => read NABEL monitoring airquality data (y1)
# TODO airquality.data
# data_monitoring_nabel <- ...

# => PM2.5:PM10 ratios
data_monitoring_nabel <- 
  data_monitoring_nabel |> 
  dplyr::filter(parameter %in% c("NO2", "PM2.5", "PM10") & lubridate::year(starttime) >= 2000) |> 
  dplyr::select(starttime, site, parameter, value) |> 
  tidyr::spread(parameter, value) |> 
  dplyr::mutate(
    year = lubridate::year(starttime),
    PMratio = PM2.5 / PM10
  ) |> 
  dplyr::filter(!is.na(PMratio) & site != "Bern-Bollwerk")

# data_monitoring_nabel |>
#   ggplot(aes(x = starttime, y = PMratio, color = site)) +
#   geom_line()
# 
# data_monitoring_nabel |>
#   ggplot(aes(x = PM10, y = PMratio)) +
#   geom_point() + 
#   geom_smooth(method = "rlm", se = FALSE)

regr <- MASS::rlm(formula(PMratio ~ factor(year) -1), data = data_monitoring_nabel)
# summary(regr)
# plot(regr)
# => eigentlich einfach ein fixer Faktor pro Jahr

coef <- 
  tibble::tibble(
    year = extract_year(names(coefficients(regr))),
    offset = coefficients(regr)
  ) |> 
  dplyr::filter(!is.na(year)) #|>  dplyr::mutate(slope = coefficients(regr)[stringr::str_detect(names(coefficients(regr)), "NO2")])


target_years <- as.character(years$all[which(years$all %in% 2010:2014)])

data_raster_aq[target_years] <-
  purrr::map(setNames(target_years, target_years), function(year) { # FIXME: find a better way
    
    cf <- dplyr::filter(coef, year == !!year)
    y <-  data_raster_aq[[year]][["pm10"]]
    y <-
      y |> 
      dplyr::mutate(pm25 = pm10 * cf$offset) |> 
      dplyr::select(-pm10)
    
    return(c(data_raster_aq[[year]], list(pm25 = y)))
  })



