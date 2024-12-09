# # compiling air quality monitoring O3 peak-season data from several sites outside the Canton of Zürich by Ostluft and NABEL monitoring networks for deriving O3 peak-season raster data
# 
# 
# TODO: integrate additional NABEL data
# 
# # read datasets ...
# # ---
# # => read NABEL monitoring airquality data (y1 & h1)
# # data_monitoring_nabel_h1_additional <- lapply(c(filter_ressources(ressources, 22), filter_ressources(ressources, 23)), function(x) read_local_csv(x, delim = "\t"))
# 
# # => read Ostluft monitoring airquality data (y1 & h1)
# data_monitoring_ostluft_h1_additional <- read_local_csv("inst/extdata/input/ostluft_airmo_h1_additional.csv", locale = readr::locale(encoding = "UTF-8"), col_names = FALSE) # data outside Canton Zürich for deriving O3 peak-season map
# 
# # => read NABEL & Ostluft monitoring site metadata
# site_meta_nabel <- read_local_csv(filter_ressources(ressources, 5), col_select = c("Station", "Ost Y", "Nord X", "Höhe", "Zonentyp", "Stationstyp"))
# site_meta_ostluft <- read_local_csv(filter_ressources(ressources, 9), delim = ",", locale = readr::locale(encoding = "UTF-8"))
# 
# # prepare datasets ...
# # ---
# # => merge, simplify & finalise site metadata
# site_meta <- prepare_monitoring_meta(site_meta_ostluft, site_meta_nabel)
# 
# # => restructure NABEL & calculate O3 peak season from h1 data
# # data_monitoring_nabel_additional <- prepare_monitoring_nabel_h1(data_monitoring_nabel_h1_additional)
# 
# # => restructure Ostluft & calculate O3 peak season from h1 data
# data_monitoring_ostluft_additional <- prepare_monitoring_ostluft_h1(data_monitoring_ostluft_h1_additional) 
# 
# # => merge & finalise datasets
# data_monitoring_aq_additional <-
#   data_monitoring_ostluft_additional |> 
#   # dplyr::bind_rows(data_monitoring_nabel_additional) |> 
#   prepare_monitoring_aq(site_meta)



# derive O3 peak-season raster data from statistical relationships
# => read & prepare monitoring data
data_monitoring_aq <-
  read_local_csv("inst/extdata/output/data_airquality_monitoring_y1.csv", locale = readr::locale(encoding = "UTF-8")) |> 
  dplyr::filter(parameter %in% c("O3_peakseason_mean_d1_max_mean_h8gl", "NO2")) |> 
  dplyr::select(year, site, masl, parameter, value) |> 
  tidyr::spread(parameter, value) |> 
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
  dplyr::mutate(
    slope = coefficients(regr)[stringr::str_detect(names(coefficients(regr)), "NO2")]
  )

# => calculate O3 peak-season data_raster_aq from NO2
data_raster_aq <-
  lapply(data_raster_aq, function(x) { # FIXME: find a better way
    
    year <- unique(na.omit(extract_year(names(x))))
    cf <- dplyr::filter(coef, year == !!year)
    y <- x[[which(stringr::str_detect(names(x), "no2"))]]
    y <-
      y |> 
      dplyr::mutate(O3_peakseason_mean_d1_max_mean_h8gl = no2 * cf$slope + cf$offset) |> 
      dplyr::select(-no2)
    
    # ggplot() + geom_stars(data = y) + scale_fill_viridis_c(na.value = NA) + coord_equal()
    
    return(c(x, list(O3_peakseason_mean_d1_max_mean_h8gl = y)))
  })



