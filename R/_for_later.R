

### -----------------------------------------------
### ... for later:
### -----------------------------------------------


# lbi <- function(PM10_rel, NO2_rel, O3_rel) {
#   
#   cut_val_rel <- function(x) {
#     dplyr::case_when(
#       x <= 0.5 ~ 1,
#       x > 0.5 & x <= 0.75 ~ 2,
#       x > 0.75 & x <= 1 ~ 3,
#       x > 1 & x <= 1.25 ~ 4,
#       x > 1.25 & x <= 1.5 ~ 5,
#       x > 1.5 ~ 6
#     )
#   }
#   
#   LBI <- (4.5 * cut_val_rel(PM10_rel) + 4.5 * cut_val_rel(NO2_rel) + cut_val_rel(O3_rel)) / 10
#   LBI <- dplyr::case_when(
#     LBI <= 1.5 ~ 1,
#     LBI > 1.5 & LBI <= 2.5 ~ 2,
#     LBI > 2.5 & LBI <= 3.5 ~ 3,
#     LBI > 3.5 & LBI <= 4.5 ~ 4,
#     LBI > 4.5 & LBI <= 5.5 ~ 5,
#     LBI > 5.5 ~ 6
#   )
#   
#   return(LBI)
# }
# 
# 
# 
# calc_lbi <- function(data, threshold_values) {
#   
#   data <- combine_thresholds(data, threshold_values)
#   data <- 
#     data %>% 
#     dplyr::mutate(value_relative = value / `LRV Grenzwert`) %>% 
#     dplyr::filter(parameter %in% c("NO2", "PM10", "O3_max_98%_m1")) %>% 
#     dplyr::group_by(starttime, site, interval, unit, source) %>% 
#     dplyr::select(parameter, value_relative) %>% 
#     tidyr::spread(parameter, value_relative) %>% 
#     dplyr::mutate(LBI = lbi(PM10, NO2, `O3_max_98%_m1`)) %>% 
#     dplyr::ungroup() %>% 
#     dplyr::select(starttime, site, interval, LBI, source) %>% 
#     tidyr::gather(parameter, value, -starttime, -site, -interval, -source) %>% 
#     dplyr::mutate(
#       parameter = factor(parameter),
#       unit = factor(NA)
#     )
#   
#   return(data)
# }
# 
# 
# 
# 
# recode_lbi <- function(lbi) {
#   
#   lbi <- dplyr::recode(as.character(lbi), !!!c("1" = "gering", "2" = "m\u00e4ssig", "3" = "deutlich", "4" = "erheblich", "5" = "hoch", "6" = "sehr hoch"))
#   lbi <- factor(lbi, levels = c("gering", "m\u00e4ssig", "deutlich", "erheblich", "hoch", "sehr hoch"))
#   
#   return(lbi)
# }





# # plot timeseries Langzeitbelastungsindex LBI, see https://cerclair.ch/assets/pdf/27b_2015_06_10_D_Langzeit_Luftbelastungs_Index.pdf

# sites <-
#   data %>%
#   dplyr::filter(parameter == "LBI") %>%
#   dplyr::group_by(site) %>%
#   dplyr::summarise(n = sum(!is.na(value))) %>%
#   dplyr::ungroup() %>%
#   dplyr::filter(n > 3) %>%
#   dplyr::pull(site) %>%
#   as.character()
#
# plots$airquality$monitoring$LBI$timeseries <-
#   data %>%
#   dplyr::filter(parameter == "LBI" & !is.na(value) & site %in% sites) %>%
#   dplyr::arrange(site, starttime) %>%
#   ggplot2::ggplot(aes(x = lubridate::year(starttime) + 0.5, y = site, fill = recode_lbi(value))) +
#   ggplot2::geom_raster() +
#   ggplot2::scale_x_continuous(expand = c(0,0)) +
#   ggplot2::scale_fill_manual(name = "LBI", values = c("gering" = "#00FFFF", "mässig" = "#80FF00", "deutlich" = "#FFFF00",
#                                                       "erheblich" = "#FFBF00", "hoch"= "#FF8000", "sehr hoch" = "#BF00FF")) +
#   ggplot2::ggtitle(
#     label = openair::quickText("Luftqualitätsmesswerte - Langzeitbelastungsindex (LBI)"),
#     subtitle = openair::quickText("Definition siehe www.cerclair.ch/empfehlungen")
#   ) +
#   ggplot2::labs(caption = "Datenabdeckung: Kanton Zürich, Quelle: OSTLUFT & NABEL (BAFU & Empa)") +
#   theme_ts


