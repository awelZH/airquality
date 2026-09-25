

# read airquality monitoring data
parameters_validation <- c("NO2", "O3_max_98p_m1", "PM10", "PM2.5", "O3_peakseason_mean_d1_max_mean_h8gl")

data_monitoring_aq <- 
  read_local_csv("data/output/data_airquality_monitoring_y1.csv", locale = readr::locale(encoding = "UTF-8")) |> 
  dplyr::filter(year %in% !!years & parameter %in% !!parameters_validation)

sites <- 
  data_monitoring_aq |> 
  dplyr::select(site, x, y) |> 
  dplyr::distinct(site, .keep_all = TRUE) |> 
  sf::st_as_sf(coords = c("x","y"), crs = sf::st_crs(crs))

data_validate <-
  data_raster_aq |> 
  purrr::map(function(x) {
    
    purrr::map(x, function(y) {
      
      extracted <- stars::st_extract(y, at = sites) 
      
      extracted |> 
        sf::st_drop_geometry() |> 
        dplyr::bind_cols(sf::st_coordinates(extracted)) |> 
        dplyr::bind_cols(sf::st_drop_geometry(sites)) |> 
        tidyr::gather(pollutant, concentration_model, -site, -X, -Y) |> 
        dplyr::rename(
          x = X,
          y = Y
        ) |> 
        tibble::as_tibble() |> 
        dplyr::mutate(
          parameter = dplyr::recode(pollutant,
                                    "no2" = "NO2",
                                    "pm25" = "PM2.5",
                                    "pm10" = "PM10",
                                    "bc" = "eBC",
                                    "mp98" = "O3_max_98p_m1"
          )
        ) |> 
        dplyr::select(-pollutant)
      
    }) |> 
      dplyr::bind_rows()
    
  })

data_validate <-
  purrr::map(names(data_validate), function(yr) {
    dplyr::mutate(data_validate[[yr]], year = as.numeric(yr))
  }) |> 
  dplyr::bind_rows() |> 
  dplyr::right_join(data_monitoring_aq, by = c("x", "y", "site", "year", "parameter")) |> 
  dplyr::select(year, site, siteclass, masl, parameter, pollutant, metric, concentration, concentration_model) |> 
  na.omit()

data_validate |> 
  ggplot(aes(x = concentration, y = concentration_model, color = siteclass)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point() +
  facet_wrap(paste0(pollutant,", ",metric)~., scales = "free") +
  # facet_wrap(paste0(pollutant,", ",metric)~year, scales = "free") +
  scale_color_viridis_d(direction = -1, begin = 0.2, end = 0.97)




