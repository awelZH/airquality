# compiling air quality monitoring O3 peak-season data from several sites outside the Canton of Zürich by Ostluft and NABEL monitoring networks for deriving O3 peak-season raster data



# derive O3 peak-season raster data from statistical relationships
# => read & prepare monitoring data
data_monitoring_aq <-
  read_local_csv("inst/extdata/output/data_airquality_monitoring_y1.csv", locale = readr::locale(encoding = "UTF-8")) |> 
  dplyr::filter(parameter %in% c("O3_peakseason_mean_d1_max_mean_h8gl", "NO2")) |> 
  dplyr::select(year, site, masl, parameter, concentration) |> 
  tidyr::spread(parameter, concentration) |> 
  na.omit() |> 
  dplyr::group_by(year) |> 
  dplyr::mutate(n = dplyr::n()) |> 
  dplyr::ungroup() |> 
  dplyr::filter(n > 6)

# => derive statistical relationships for O3 peak-season
# data_monitoring_aq |> 
#   ggplot(aes(x = NO2, y = O3_peakseason_mean_d1_max_mean_h8gl, color = masl)) +
#   geom_smooth(method = "rlm") +
#   geom_point() +
#   facet_wrap(year~.) +
#   scale_color_viridis_c(direction = -1)
# 
# data_monitoring_aq |>
#   ggplot(aes(x = NO2, y = O3_peakseason_mean_d1_max_mean_h8gl, color = factor(year))) +
#   geom_smooth(method = "rlm", se = FALSE) +
#   # geom_point() +
#   scale_color_viridis_d(direction = -1)

regr <- MASS::rlm(formula(O3_peakseason_mean_d1_max_mean_h8gl ~ NO2 + factor(year) - 1), data = data_monitoring_aq)
# summary(regr)
# plot(regr)

coef <- 
  tibble::tibble(
    year = extract_year(names(coefficients(regr))),
    offset = coefficients(regr)
  ) |> 
  dplyr::filter(!is.na(year)) |> 
  dplyr::mutate(slope = coefficients(regr)[stringr::str_detect(names(coefficients(regr)), "NO2")])

# => calculate O3 peak-season data_raster_aq from NO2
data_raster_aq  <-
  purrr::map(setNames(names(data_raster_aq), names(data_raster_aq)), function(year) { # FIXME: find a better way
    
    
    cf <- dplyr::filter(coef, year == !!year)
    y <- data_raster_aq[[year]][[which(stringr::str_detect(names(data_raster_aq[[year]]), "no2"))]]
    y <-
      y |> 
      dplyr::mutate(O3_peakseason_mean_d1_max_mean_h8gl = no2 * cf$slope + cf$offset) |> 
      dplyr::select(-no2)
    
    # ggplot() + geom_stars(data = y) + scale_fill_viridis_c(na.value = NA) + coord_equal()
    
    return(c(data_raster_aq[[year]], list(O3_peakseason_mean_d1_max_mean_h8gl = y)))
  })



