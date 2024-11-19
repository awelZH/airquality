




### plot timeseries per Standortklasse
### ----------------------------------------------------------------------
d0 <-
  data_agg %>% 
  mutate(
    Jahr = year(starttime),
    parameter = dplyr::recode_factor(parameter, !!!par_plot)
  ) %>% 
  dplyr::filter(Jahr >= year_min) %>% 
  dplyr::select(-starttime) %>% 
  dplyr::rename(
    Messgrösse = parameter
  )

d1 <- 
  data %>% 
  mutate(
    parameter = dplyr::recode_factor(parameter, !!!par_plot),
    value = round(value, 2),
    Jahr = year(starttime)
  ) %>% 
  dplyr::filter(Jahr >= year_min) %>% 
  dplyr::select(-starttime) %>% 
  dplyr::rename(
    Standort = site2,
    Messwert = value,
    Messgrösse = parameter,
    Messzeitraum = interval,
    Einheit = unit
  ) %>% 
  mutate(
    Standort = ifelse(is.na(Standort), paste0(as.character(site), "_NABEL"), as.character(Standort)),
    Standort = ifelse(Standort == "Zürich_Kaserne", "Zürich_Kaserne_NABEL", Standort)
  )

d2 <- 
  specific_sites %>% 
  group_split(id = 1:nrow(specific_sites)) %>% 
  lapply(function(x) dplyr::filter(data, parameter == x$parameter & site == x$site & Standortklasse == x$Standortklasse)) %>% 
  bind_rows() %>% 
  mutate(
    parameter = dplyr::recode_factor(parameter, !!!par_plot),
    value = round(value, 2),
    Jahr = year(starttime)
  ) %>% 
  dplyr::filter(Jahr >= year_min) %>% 
  dplyr::select(-starttime) %>% 
  dplyr::rename(
    Standort = site2,
    Messwert = value,
    Messgrösse = parameter,
    Messzeitraum = interval,
    Einheit = unit
  ) %>% 
  mutate(Standort = ifelse(is.na(Standort), paste0(as.character(site), "_NABEL"), as.character(Standort)),
         Standort = ifelse(Standort == "Zürich_Kaserne", "Zürich_Kaserne_NABEL", Standort)
  )


dg <- 
  grenzwerte %>% 
  dplyr::filter(Referenz != "WHO-Empfehlung 2021") %>%
  mutate(
    parameter = dplyr::recode(parameter, !!!par_plot),
    Typ = factor(as.character(Referenz))
  ) %>% 
  dplyr::rename(
    Messgrösse = parameter
  )

data_plot <- 
  list(
    Jahresdaten = d1,
    Jahresdaten_aggregiert = d0,
    Jahresdaten_ausgewaehlte_Standorte = d2,
    Grenzwerte = dg
  )

rm(list = c("d0", "d1", "d2", "dg"))

target <- par_plot[!(names(par_plot) %in% exclude_parameter)]



plots$all <- 
  data_plot$Jahresdaten %>% 
  dplyr::filter(Messgrösse %in% target) %>% 
  mutate(Messgrösse = factor(Messgrösse, levels = target)) %>% 
  ggplot(aes(x = Jahr, y = Messwert)) +
  geom_hline(data = mutate(dplyr::filter(data_plot$Grenzwerte, Messgrösse %in% target), Messgrösse = factor(Messgrösse, levels = target)),
             mapping = aes(yintercept = Referenzwert), color = "black", lwd = linesize, show.legend = TRUE) +
  # geom_ribbon(data = mutate(dplyr::filter(data_plot$Jahresdaten_aggregiert, Messgrösse %in% target), Messgrösse = factor(Messgrösse, levels = target)),
  #             mapping = aes(x = Jahr, ymin = min, ymax = max, fill = Standortklasse), alpha = 0.3, color = NA, inherit.aes = FALSE) +
  # geom_crossbar(data = mutate(dplyr::filter(data_plot$Jahresdaten_aggregiert, Messgrösse %in% target), Messgrösse = factor(Messgrösse, levels = target)),
  #               mapping = aes(x = Jahr, y = min, ymin = min, ymax = max, fill = Standortklasse), alpha = 0.3, color = NA, width = 1, inherit.aes = FALSE) +
  geom_point(mapping = aes(x = Jahr, y = Messwert, color = Standortklasse, fill = Standortklasse),
             shape = 21, size = pointsize * 0.75, alpha = 0.25, inherit.aes = FALSE) +
  geom_line(data = mutate(dplyr::filter(data_plot$Jahresdaten_ausgewaehlte_Standorte, Messgrösse %in% target), Messgrösse = factor(Messgrösse, levels = target)),
            mapping = aes(x = Jahr, y = Messwert, color = Standortklasse), lwd = linesize, inherit.aes = FALSE) +
  scale_fill_manual(values = cols) +
  scale_x_continuous(limits = c(year_min, NA), breaks = seq(1970, 2050, 5), expand = c(0.01,0.01)) +
  scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01)) +
  scale_color_manual(values = cols) +
  # scale_linetype_manual(name = "Referenzwert", values = c("LRV-Grenzwert" = 1, "EKL-Zielwert" = 3, "WHO-Empfehlung 2021" = 2)) +
  lemon::facet_rep_wrap(Messgrösse~., ncol = 3, repeat.tick.labels = TRUE, scales = "free_y") + # , labeller = function(x) openair::quickText(x)) +
  theme_ts2
# plots$all





plots$PM10 <-
  data_plot$Jahresdaten %>% 
  dplyr::filter(Messgrösse %in% target) %>% 
  mutate(Messgrösse = factor(Messgrösse, levels = target)) %>% 
  dplyr::filter(stringr::str_detect(Messgrösse, "PM10")) %>% # dplyr::filter(Messgrösse == "Feinstaub PM10 Jahresmittel\n(PM10, µg/m3)") %>% dplyr::arrange(site, Jahr) %>% View
  ggplot(aes(x = Jahr, y = Messwert)) +
  geom_hline(data = mutate(dplyr::filter(data_plot$Grenzwerte, stringr::str_detect(Messgrösse, "PM10")), Messgrösse = factor(Messgrösse, levels = target)),
             mapping = aes(yintercept = Referenzwert), color = "black", lwd = linesize, show.legend = TRUE) +
  # geom_ribbon(data = mutate(dplyr::filter(data_plot$Jahresdaten_aggregiert, Messgrösse %in% target), Messgrösse = factor(Messgrösse, levels = target)),
  #             mapping = aes(x = Jahr, ymin = min, ymax = max, fill = Standortklasse), alpha = 0.3, color = NA, inherit.aes = FALSE) +
  # geom_crossbar(data = mutate(dplyr::filter(data_plot$Jahresdaten_aggregiert, Messgrösse %in% target), Messgrösse = factor(Messgrösse, levels = target)),
  #               mapping = aes(x = Jahr, y = min, ymin = min, ymax = max, fill = Standortklasse), alpha = 0.3, color = NA, width = 1, inherit.aes = FALSE) +
  geom_point(mapping = aes(x = Jahr, y = Messwert, color = Standortklasse, fill = Standortklasse),
             shape = 21, size = pointsize * 0.75, alpha = 0.25, inherit.aes = FALSE) +
  geom_line(data = mutate(dplyr::filter(data_plot$Jahresdaten_ausgewaehlte_Standorte, stringr::str_detect(Messgrösse, "PM10")), Messgrösse = factor(Messgrösse, levels = target)),
            mapping = aes(x = Jahr, y = Messwert, color = Standortklasse), lwd = linesize, inherit.aes = FALSE) +
  scale_fill_manual(values = cols) +
  scale_x_continuous(limits = c(year_min, NA), breaks = seq(1970, 2050, 5), expand = c(0.01,0.01)) +
  scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01)) +
  scale_color_manual(values = cols) +
  # scale_linetype_manual(name = "Referenzwert", values = c("LRV-Grenzwert" = 1, "EKL-Zielwert" = 3, "WHO-Empfehlung 2021" = 2)) +
  lemon::facet_rep_wrap(Messgrösse~., ncol = 3, repeat.tick.labels = TRUE, scales = "free_y") + # , labeller = function(x) openair::quickText(x)) +
  theme_ts2 +
  theme(legend.position = "bottom", legend.title = element_blank())



plots$PM2.5 <-
  data_plot$Jahresdaten %>% 
  dplyr::filter(Messgrösse %in% target) %>% 
  mutate(Messgrösse = factor(Messgrösse, levels = target)) %>% 
  dplyr::filter(stringr::str_detect(Messgrösse, "PM2.5")) %>% # dplyr::filter(Messgrösse == "Feinstaub PM2.5 Jahresmittel\n(PM2.5, µg/m3)") %>% dplyr::arrange(site, Jahr) %>% View
  ggplot(aes(x = Jahr, y = Messwert)) +
  geom_hline(data = mutate(dplyr::filter(data_plot$Grenzwerte, stringr::str_detect(Messgrösse, "PM2.5")), Messgrösse = factor(Messgrösse, levels = target)),
             mapping = aes(yintercept = Referenzwert), color = "black", lwd = linesize, show.legend = TRUE) +
  # geom_ribbon(data = mutate(dplyr::filter(data_plot$Jahresdaten_aggregiert, Messgrösse %in% target), Messgrösse = factor(Messgrösse, levels = target)),
  #             mapping = aes(x = Jahr, ymin = min, ymax = max, fill = Standortklasse), alpha = 0.3, color = NA, inherit.aes = FALSE) +
  # geom_crossbar(data = mutate(dplyr::filter(data_plot$Jahresdaten_aggregiert, Messgrösse %in% target), Messgrösse = factor(Messgrösse, levels = target)),
  #               mapping = aes(x = Jahr, y = min, ymin = min, ymax = max, fill = Standortklasse), alpha = 0.3, color = NA, width = 1, inherit.aes = FALSE) +
  geom_point(mapping = aes(x = Jahr, y = Messwert, color = Standortklasse, fill = Standortklasse),
             shape = 21, size = pointsize * 0.75, alpha = 0.25, inherit.aes = FALSE) +
  geom_line(data = mutate(dplyr::filter(data_plot$Jahresdaten_ausgewaehlte_Standorte, stringr::str_detect(Messgrösse, "PM2.5")), Messgrösse = factor(Messgrösse, levels = target)),
            mapping = aes(x = Jahr, y = Messwert, color = Standortklasse), lwd = linesize, inherit.aes = FALSE) +
  scale_fill_manual(values = cols) +
  scale_x_continuous(limits = c(year_min, NA), breaks = seq(1970, 2050, 5), expand = c(0.01,0.01)) +
  scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01)) +
  scale_color_manual(values = cols) +
  # scale_linetype_manual(name = "Referenzwert", values = c("LRV-Grenzwert" = 1, "EKL-Zielwert" = 3, "WHO-Empfehlung 2021" = 2)) +
  lemon::facet_rep_wrap(Messgrösse~., ncol = 3, repeat.tick.labels = TRUE, scales = "free_y") + # , labeller = function(x) openair::quickText(x)) +
  theme_ts2 +
  theme(legend.position = "right")



plots$eBC <-
  data_plot$Jahresdaten %>% 
  dplyr::filter(Messgrösse %in% target) %>% 
  mutate(Messgrösse = factor(Messgrösse, levels = target)) %>% 
  dplyr::filter(stringr::str_detect(Messgrösse, "eBC")) %>% # dplyr::filter(Messgrösse == "Russ Jahresmittel*\n(eBC, µg/m3)") %>% dplyr::arrange(site, Jahr) %>% View
  ggplot(aes(x = Jahr, y = Messwert)) +
  geom_hline(data = mutate(dplyr::filter(data_plot$Grenzwerte, stringr::str_detect(Messgrösse, "eBC")), Messgrösse = factor(Messgrösse, levels = target)),
             mapping = aes(yintercept = Referenzwert), color = "black", lwd = linesize, show.legend = TRUE) +
  # geom_ribbon(data = mutate(dplyr::filter(data_plot$Jahresdaten_aggregiert, Messgrösse %in% target), Messgrösse = factor(Messgrösse, levels = target)),
  #             mapping = aes(x = Jahr, ymin = min, ymax = max, fill = Standortklasse), alpha = 0.3, color = NA, inherit.aes = FALSE) +
  # geom_crossbar(data = mutate(dplyr::filter(data_plot$Jahresdaten_aggregiert, Messgrösse %in% target), Messgrösse = factor(Messgrösse, levels = target)),
  #               mapping = aes(x = Jahr, y = min, ymin = min, ymax = max, fill = Standortklasse), alpha = 0.3, color = NA, width = 1, inherit.aes = FALSE) +
  geom_point(mapping = aes(x = Jahr, y = Messwert, color = Standortklasse, fill = Standortklasse),
             shape = 21, size = pointsize * 0.75, alpha = 0.25, inherit.aes = FALSE) +
  geom_line(data = mutate(dplyr::filter(data_plot$Jahresdaten_ausgewaehlte_Standorte, stringr::str_detect(Messgrösse, "eBC")), Messgrösse = factor(Messgrösse, levels = target)),
            mapping = aes(x = Jahr, y = Messwert, color = Standortklasse), lwd = linesize, inherit.aes = FALSE) +
  scale_fill_manual(values = cols) +
  scale_x_continuous(limits = c(year_min, NA), breaks = seq(1970, 2050, 5), expand = c(0.01,0.01)) +
  scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01)) +
  scale_color_manual(values = cols) +
  # scale_linetype_manual(name = "Referenzwert", values = c("LRV-Grenzwert" = 1, "EKL-Zielwert" = 3, "WHO-Empfehlung 2021" = 2)) +
  lemon::facet_rep_wrap(Messgrösse~., ncol = 3, repeat.tick.labels = TRUE, scales = "free_y") + # , labeller = function(x) openair::quickText(x)) +
  theme_ts2 +
  theme(legend.position = "right")



plots$NO2 <-
  data_plot$Jahresdaten %>% 
  dplyr::filter(Messgrösse %in% target) %>% 
  mutate(Messgrösse = factor(Messgrösse, levels = target)) %>% 
  dplyr::filter(stringr::str_detect(Messgrösse, "NO2")) %>% # dplyr::filter(Messgrösse == "Stickstoffdioxid Jahresmittel\n(NO2, µg/m3)") %>% dplyr::arrange(site, Jahr) %>% View
  ggplot(aes(x = Jahr, y = Messwert)) +
  geom_hline(data = mutate(dplyr::filter(data_plot$Grenzwerte, stringr::str_detect(Messgrösse, "NO2")), Messgrösse = factor(Messgrösse, levels = target)),
             mapping = aes(yintercept = Referenzwert), color = "black", lwd = linesize, show.legend = TRUE) +
  # geom_ribbon(data = mutate(dplyr::filter(data_plot$Jahresdaten_aggregiert, Messgrösse %in% target), Messgrösse = factor(Messgrösse, levels = target)),
  #             mapping = aes(x = Jahr, ymin = min, ymax = max, fill = Standortklasse), alpha = 0.3, color = NA, inherit.aes = FALSE) +
  # geom_crossbar(data = mutate(dplyr::filter(data_plot$Jahresdaten_aggregiert, Messgrösse %in% target), Messgrösse = factor(Messgrösse, levels = target)),
  #               mapping = aes(x = Jahr, y = min, ymin = min, ymax = max, fill = Standortklasse), alpha = 0.3, color = NA, width = 1, inherit.aes = FALSE) +
  geom_point(mapping = aes(x = Jahr, y = Messwert, color = Standortklasse, fill = Standortklasse),
             shape = 21, size = pointsize * 0.75, alpha = 0.25, inherit.aes = FALSE) +
  geom_line(data = mutate(dplyr::filter(data_plot$Jahresdaten_ausgewaehlte_Standorte, stringr::str_detect(Messgrösse, "NO2")), Messgrösse = factor(Messgrösse, levels = target)),
            mapping = aes(x = Jahr, y = Messwert, color = Standortklasse), lwd = linesize, inherit.aes = FALSE) +
  scale_fill_manual(values = cols) +
  scale_x_continuous(limits = c(year_min, NA), breaks = seq(1970, 2050, 5), expand = c(0.01,0.01)) +
  scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01)) +
  scale_color_manual(values = cols) +
  # scale_linetype_manual(name = "Referenzwert", values = c("LRV-Grenzwert" = 1, "EKL-Zielwert" = 3, "WHO-Empfehlung 2021" = 2)) +
  lemon::facet_rep_wrap(Messgrösse~., ncol = 3, repeat.tick.labels = TRUE, scales = "free_y") + # , labeller = function(x) openair::quickText(x)) +
  theme_ts2 +
  theme(legend.position = "bottom", legend.title = element_blank())




plots$O3 <-
  data_plot$Jahresdaten %>% 
  dplyr::filter(Messgrösse %in% target) %>% 
  mutate(Messgrösse = factor(Messgrösse, levels = target)) %>% 
  dplyr::filter(stringr::str_detect(Messgrösse, "O3")) %>% # dplyr::filter(Messgrösse == "Ozon max. Monats-98%-Perzentil\n(O3, µg/m3)") %>% dplyr::arrange(site, Jahr) %>% View
  ggplot(aes(x = Jahr, y = Messwert)) +
  geom_hline(data = mutate(dplyr::filter(data_plot$Grenzwerte, stringr::str_detect(Messgrösse, "O3")), Messgrösse = factor(Messgrösse, levels = target)),
             mapping = aes(yintercept = Referenzwert), color = "black", lwd = linesize, show.legend = TRUE) +
  # geom_ribbon(data = mutate(dplyr::filter(data_plot$Jahresdaten_aggregiert, Messgrösse %in% target), Messgrösse = factor(Messgrösse, levels = target)),
  #             mapping = aes(x = Jahr, ymin = min, ymax = max, fill = Standortklasse), alpha = 0.3, color = NA, inherit.aes = FALSE) +
  # geom_crossbar(data = mutate(dplyr::filter(data_plot$Jahresdaten_aggregiert, Messgrösse %in% target), Messgrösse = factor(Messgrösse, levels = target)),
  #               mapping = aes(x = Jahr, y = min, ymin = min, ymax = max, fill = Standortklasse), alpha = 0.3, color = NA, width = 1, inherit.aes = FALSE) +
  geom_point(mapping = aes(x = Jahr, y = Messwert, color = Standortklasse, fill = Standortklasse),
             shape = 21, size = pointsize * 0.75, alpha = 0.25, inherit.aes = FALSE) +
  geom_line(data = mutate(dplyr::filter(data_plot$Jahresdaten_ausgewaehlte_Standorte, stringr::str_detect(Messgrösse, "O3")), Messgrösse = factor(Messgrösse, levels = target)),
            mapping = aes(x = Jahr, y = Messwert, color = Standortklasse), lwd = linesize, inherit.aes = FALSE) +
  scale_fill_manual(values = cols) +
  scale_x_continuous(limits = c(year_min, NA), breaks = seq(1970, 2050, 5), expand = c(0.01,0.01)) +
  scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01)) +
  scale_color_manual(values = cols) +
  # scale_linetype_manual(name = "Referenzwert", values = c("LRV-Grenzwert" = 1, "EKL-Zielwert" = 3, "WHO-Empfehlung 2021" = 2)) +
  lemon::facet_rep_wrap(Messgrösse~., ncol = 3, repeat.tick.labels = TRUE, scales = "free_y") + # , labeller = function(x) openair::quickText(x)) +
  theme_ts2 +
  theme(legend.position = "bottom", legend.title = element_blank())







### for later: another (interactive) version of the plot using ggiraph
### ----------------------------------------------------------------------
# iraph_options <-
#   list(
#     opts_sizing(rescale = FALSE),
#     opts_tooltip(
#       opacity = .8,
#       css = "background-color:gray;color:white;padding:2px;border-radius:2px;"
#     ),
#     # opts_hover_inv(css = "opacity:0.2;"),
#     opts_hover(css = "stroke:#000000;cursor:pointer;")
#   )
# 
# p <-
#   data_plot$Jahresdaten %>%
#   dplyr::filter(Messgrösse %in% target) %>%
#   mutate(Messgrösse = factor(Messgrösse, levels = target)) %>%
#   ggplot(aes(x = Jahr, y = Messwert)) +
#   geom_hline(data = mutate(dplyr::filter(data_plot$Grenzwerte, Messgrösse %in% target), Messgrösse = factor(Messgrösse, levels = target)),
#              mapping = aes(yintercept = `Grenz-/Richtwert`, lty = Typ), color = "black", show.legend = TRUE) +
#   geom_point_interactive(mapping = aes(x = Jahr, y = Messwert, color = Standortklasse, tooltip = paste0(Standort,"\n",Jahr,", ",round(Messwert,1)," ",Einheit), data_id = Standort),
#                          shape = 16, alpha = 0.25, size = 1.5, inherit.aes = FALSE) +
#   geom_line_interactive(data = mutate(dplyr::filter(data_plot$Jahresdaten_ausgewaehlte_Standorte, Messgrösse %in% target), Messgrösse = factor(Messgrösse, levels = target)),
#                         mapping = aes(x = Jahr, y = Messwert, color = Standortklasse, tooltip = Standort, data_id = Standort), inherit.aes = FALSE) +
#   lemon::facet_rep_wrap(Messgrösse~., ncol = 3, repeat.tick.labels = TRUE, scales = "free_y") + # , labeller = function(x) openair::quickText(x)) +
#   scale_x_continuous(limits = c(year_min, NA), breaks = seq(1970, 2050, 5), expand = c(0.01,0.01)) +
#   scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01)) +
#   scale_color_manual(values = cols) +
#   scale_linetype_manual(name = "Referenzwert", values = c("LRV-Grenzwert" = 1, "EKL-Zielwert" = 3, "WHO-Empfehlung 2021" = 2)) +
#   theme_ts2
# 
# p2 <-
#   girafe(ggobj = p,
#     width_svg = 12, height_svg = 8,
#     # fonts = list(sans = "Open Sans"),
#     options = iraph_options
#   )
# p2








### plot values relative to thresholds
### ----------------------------------------------------------------------

### nur LRV
par_plot2 <- c("PM10" = "PM10 JMW", "PM10_nb_d1>50" = "PM10 TMW > 50 µg/m3", "PM2.5" = "PM2.5 JMW", 
               "EC" = "eBC JMW*", "NO2" = "NO2 JMW", "NO2_nb_d1>80" = "NO2 TMW > 80 µg/m3",
               "O3_98%_min30_max_m1" = "O3 max. p98%", "O3_nb_h1>120" = "O3 SMW > 120 µg/m3", "O3_max_h1" = "O3 max. SMW")

d <- 
  grenzwerte %>% 
  mutate(Zeitbasis = ifelse(stringr::str_detect(parameter, "_nb"), "Kurzzeitbelastung", "Langzeitbelastung")) %>% 
  right_join(data, by = "parameter") %>% 
  mutate(
    parameter = dplyr::recode_factor(parameter, !!!par_plot2),
    value = round(value, 2),
    Jahr = year(starttime),
    value_relative = case_when(Zeitbasis == "Langzeitbelastung" ~ value / Referenzwert, TRUE ~ value),
    Referenz = dplyr::recode_factor(Referenz, !!!c("LRV-Grenzwert" = "LRV", "WHO-Empfehlung 2021" = "WHO '21", "EKL-Zielwert" = "EKL"))
  ) %>% 
  dplyr::filter(Jahr %in% years & !is.na(Referenzwert)) %>% 
  dplyr::select(-starttime) %>% 
  dplyr::rename(
    Standort = site2,
    Messwert = value,
    Messwert_relativ = value_relative,
    Messgrösse = parameter,
    Messzeitraum = interval,
    Einheit = unit
  ) %>% 
  mutate(
    Standort = ifelse(is.na(Standort), paste0(as.character(site), "_NABEL"), as.character(Standort)),
    Standort = ifelse(Standort == "Zürich_Kaserne", "Zürich_Kaserne_NABEL", Standort)
  )

d0 <- 
  grenzwerte %>% 
  mutate(
    Zeitbasis = ifelse(stringr::str_detect(parameter, "_nb"), "Kurzzeitbelastung", "Langzeitbelastung"),
    Referenz = dplyr::recode_factor(Referenz, !!!c("LRV-Grenzwert" = "LRV", "WHO-Empfehlung 2021" = "AQG '21", "EKL-Zielwert" = "EKL")),
    Messgrösse = dplyr::recode_factor(parameter, !!!par_plot2)
  ) %>% 
  dplyr::filter(Zeitbasis == "Kurzzeitbelastung")


p1 <-
  d %>% 
  dplyr::filter(Referenz == "LRV" & Zeitbasis == "Langzeitbelastung") %>% 
  mutate(
    Messgrösse = stringr::str_replace(Messgrösse, " JMW", "\nJMW"),
    Messgrösse = stringr::str_replace(Messgrösse, " max. p98%", "\nmax. p98%"),
    Messgrösse = factor(Messgrösse, levels = c("PM10\nJMW", "PM2.5\nJMW", "NO2\nJMW", "O3\nmax. p98%"))
  ) %>%
  ggplot(aes(x = factor(Jahr), y = Messwert_relativ, group = Jahr, color = Standortklasse, fill = Standortklasse)) + 
  geom_hline(yintercept = 1, color = "black", lwd = linesize) +
  # stat_summary(aes(group = Standortklasse), geom = "linerange", position = "jitter", width = 0.4, fun.min = "min", fun.max = "max", fun = "median", na.rm = TRUE, lwd = 2) +
  # stat_summary(aes(group = interaction(Jahr, Standortklasse)), geom = "point", position = "jitter", width = 0.35, na.rm = TRUE) +
  geom_point(shape = 21, size = pointsize, position = "dodge") +
  scale_y_continuous(limits = c(0, 2), expand = c(0.01,0.01), labels = scales::percent_format(), breaks = seq(0, 100, 0.5)) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = alpha(cols, 0.3)) +
  facet_nested(~Zeitbasis+Messgrösse, nest_line = element_line(), resect = unit(1, "mm")) +
  ylab("Verhältnis Messwert : Grenzwert") +
  theme_gray(base_size = basesize) +
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks = element_line(color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    ggh4x.facet.nestline = element_line(colour = "gray40")
  ) +
  theme(legend.position = "none")


p1b <-
  d %>% 
  dplyr::filter(Referenz == "LRV" & Zeitbasis == "Langzeitbelastung") %>% 
  mutate(
    Messgrösse = stringr::str_replace(Messgrösse, " JMW", "\nJMW"),
    Messgrösse = stringr::str_replace(Messgrösse, " max. p98%", "\nmax. p98%"),
    Messgrösse = factor(Messgrösse, levels = c("PM10\nJMW", "PM2.5\nJMW", "NO2\nJMW", "O3\nmax. p98%"))
  ) %>%
  ggplot(aes(x = Messgrösse, y = Messwert_relativ, color = Standortklasse, fill = Standortklasse)) + 
  geom_hline(yintercept = 1, color = "black", lwd = linesize * 1.5) +
  geom_jitter(shape = 21, size = pointsize, width = 0.075) +
  scale_y_continuous(limits = c(0, 2), expand = c(0.01,0.01), labels = scales::percent_format(), breaks = seq(0, 100, 0.5)) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = alpha(cols, 0.3)) +
  facet_wrap(Zeitbasis~.) +
  # facet_nested(~Zeitbasis+Messgrösse, nest_line = element_line(), resect = unit(1, "mm")) +
  ylab("Verhältnis Messwert : Grenzwert") +
  theme_minimal(base_size = basesize) +
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(color = "gray40"),
    axis.ticks.y = element_line(color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    axis.title.x = element_blank(),
    ggh4x.facet.nestline = element_line(colour = "gray40")
  ) +
  theme(legend.position = "none")


p1c <-
  d %>% 
  dplyr::filter(Referenz == "LRV" & Zeitbasis == "Langzeitbelastung") %>% 
  mutate(
    Messgrösse = stringr::str_replace(Messgrösse, " JMW", "\nJMW"),
    Messgrösse = stringr::str_replace(Messgrösse, " max. p98%", "\nmax. p98%"),
    Messgrösse = factor(Messgrösse, levels = c("PM10\nJMW", "PM2.5\nJMW", "NO2\nJMW", "O3\nmax. p98%"))
  ) %>%
  ggplot(aes(x = Standortklasse, y = Messwert_relativ, color = Standortklasse, fill = Standortklasse)) + 
  geom_hline(yintercept = 1, color = "black", lwd = linesize) +
  stat_summary(aes(group = Standortklasse), geom = "linerange", fun.min = "min", fun.max = "max", fun = "median", na.rm = TRUE, lwd = 2) +
  scale_y_continuous(limits = c(0, 2), expand = c(0.01,0.01), labels = scales::percent_format(), breaks = seq(0, 100, 0.5)) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = alpha(cols, 0.3)) +
  facet_nested(~Zeitbasis+Messgrösse, nest_line = element_line(), resect = unit(1, "mm")) +
  ylab("Verhältnis Messwert : Grenzwert") +
  theme_gray(base_size = basesize) +
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(color = "gray40"),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    ggh4x.facet.nestline = element_line(colour = "gray40")
  ) +
  theme(legend.position = "none")




d1 <- 
  d0 %>% 
  mutate(
    Referenzwert = case_when(
      Messgrösse == "O3 SMW > 120 µg/m3" ~ Referenzwert / 100,
      TRUE ~ Referenzwert
    ),
    Messgrösse = stringr::str_replace(Messgrösse, " >", "\n>"),
    Messgrösse = factor(Messgrösse, levels = c("PM10 TMW\n> 50 µg/m3", "NO2 TMW\n> 80 µg/m3", "O3 SMW\n> 120 µg/m3"))
  )

o3_colored <- 
  strip_nested(
    text_x = elem_list_text(color = c(rep("black", 3), "deepskyblue4")),
    by_layer_x = FALSE
  )

p2 <-
  d %>%
  dplyr::filter(Referenz == "LRV" & Zeitbasis == "Kurzzeitbelastung") %>%
  mutate(
    Messwert_relativ = case_when(
      Messgrösse == "O3 SMW > 120 µg/m3" ~ Messwert_relativ / 100,
      TRUE ~ Messwert_relativ
    ),
    Messgrösse = stringr::str_replace(Messgrösse, " >", "\n>"),
    Messgrösse = factor(Messgrösse, levels = c("PM10 TMW\n> 50 µg/m3", "NO2 TMW\n> 80 µg/m3", "O3 SMW\n> 120 µg/m3"))
  ) %>%
  ggplot(aes(x = factor(Jahr), y = Messwert_relativ, group = Jahr, color = Standortklasse, fill = Standortklasse)) +
  geom_hline(data = d1, mapping = aes(yintercept = Referenzwert), color = "black", lwd = linesize, inherit.aes = FALSE) +
  geom_point(shape = 21, size = pointsize, position = "dodge") +
  scale_y_continuous(
    limits = c(0, 8), breaks = seq(0, 10, 2), expand = c(0.01,0.01),
    sec.axis = sec_axis(trans = ~.*100)
  ) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = alpha(cols, 0.3)) +
  facet_nested(~Zeitbasis+Messgrösse, nest_line = element_line(), resect = unit(1, "mm"), strip = o3_colored) +
  ylab("Anzahl Messwerte > Grenzwert") +
  theme_gray(base_size = basesize) +
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks = element_line(color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    ggh4x.facet.nestline = element_line(colour = "gray40"),
    axis.text.y.right = element_text(color = "deepskyblue4"),
    axis.ticks.y.right = element_line(color = "deepskyblue4")
  ) +
  theme(legend.position = "right")


d1 <- 
  d0 %>% 
  mutate(
    Referenzwert = case_when(
      Messgrösse == "O3 SMW > 120 µg/m3" ~ Referenzwert / 100,
      TRUE ~ Referenzwert
    ),
    Messgrösse = stringr::str_replace(Messgrösse, " TMW", "\nTMW"),
    Messgrösse = stringr::str_replace(Messgrösse, " SMW", "\nSMW"),
    Messgrösse = factor(Messgrösse, levels = c("PM10\nTMW > 50 µg/m3", "NO2\nTMW > 80 µg/m3", "O3\nSMW > 120 µg/m3"))
  )

p2b <-
  d %>%
  dplyr::filter(Referenz == "LRV" & Zeitbasis == "Kurzzeitbelastung") %>%
  mutate(
    Messwert_relativ = case_when(
      Messgrösse == "O3 SMW > 120 µg/m3" ~ Messwert_relativ / 100,
      TRUE ~ Messwert_relativ
    ),
    Messgrösse = stringr::str_replace(Messgrösse, " TMW", "\nTMW"),
    Messgrösse = stringr::str_replace(Messgrösse, " SMW", "\nSMW"),
    Messgrösse = factor(Messgrösse, levels = c("PM10\nTMW > 50 µg/m3", "NO2\nTMW > 80 µg/m3", "O3\nSMW > 120 µg/m3"))
  ) %>%
  ggplot(aes(x = as.numeric(Messgrösse), y = Messwert_relativ, color = Standortklasse, fill = Standortklasse)) +
  geom_segment(data = d1, mapping = aes(x = as.numeric(Messgrösse) - 0.33, xend = as.numeric(Messgrösse) + 0.33, y = Referenzwert, yend = Referenzwert), color = "black", lwd = linesize * 1.5, inherit.aes = FALSE) +
  geom_jitter(shape = 21, size = pointsize, width = 0.075) +
  scale_x_continuous(breaks = 1:length(levels(d1$Messgrösse)), labels = levels(d1$Messgrösse)) +
  scale_y_continuous(
    limits = c(0, 8), breaks = seq(0, 10, 2), expand = c(0.01,0.01),
    sec.axis = sec_axis(trans = ~.*100)
  ) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = alpha(cols, 0.3)) +
  facet_wrap(Zeitbasis~.) +
  # facet_nested(~Zeitbasis+Messgrösse, nest_line = element_line(), resect = unit(1, "mm"), strip = o3_colored) +
  ylab("Anzahl Messwerte > Grenzwert") +
  theme_minimal(base_size = basesize) +
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(color = "gray40"),
    axis.ticks.y = element_line(color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1, color = c("black", "black", "deepskyblue4")),
    axis.title.x = element_blank(),
    ggh4x.facet.nestline = element_line(colour = "gray40"),
    axis.text.y.right = element_text(color = "deepskyblue4"),
    axis.ticks.y.right = element_line(color = "deepskyblue4")
  ) +
  theme(legend.position = "right")


# plots$belastung_relativ_LRV <- cowplot::plot_grid(p1, p2, rel_widths = c(2.5,3))
plots$belastung_relativ_LRV <- cowplot::plot_grid(p1b, p2b, rel_widths = c(2.5,3), axis = "tblr", align = "h")







### nur WHO AQG '21
par_plot2 <- c("PM10" = "PM10 JMW", "PM10_nb_d1>45" = "PM10 TMW > 45 µg/m3", "PM2.5" = "PM2.5 JMW", 
               "PM2.5_nb_d1>15" = "PM2.5 TMW > 15 µg/m3", "NO2" = "NO2 JMW", "NO2_nb_d1>25" = "NO2 TMW > 25 µg/m3",
               "O3_mean_d1_max_h8gl_peak_season" = "O3 peak season")

d <-
  grenzwerte %>%
  dplyr::filter(Referenz == "WHO-Empfehlung 2021") %>% 
  bind_rows(tibble(
    parameter = c("PM10_nb_d1>45", "PM2.5_nb_d1>15", "NO2_nb_d1>25", "O3_mean_d1_max_h8gl_peak_season"), 
    Referenzwert = c(3, 3, 1, 60),
    Referenz = "WHO-Empfehlung 2021"
  )) %>% 
  mutate(Zeitbasis = ifelse(stringr::str_detect(parameter, "_nb"), "Kurzzeitbelastung", "Langzeitbelastung")) %>% 
  right_join(bind_rows(dplyr::filter(data, parameter %in% c("PM10", "PM2.5", "NO2") & interval == "y1"), data_WHO), by = "parameter") %>% 
  mutate(
    parameter = dplyr::recode_factor(parameter, !!!par_plot2),
    value = round(value, 2),
    Jahr = year(starttime),
    value_relative = case_when(Zeitbasis == "Langzeitbelastung" ~ value / Referenzwert, TRUE ~ value),
    Referenz = dplyr::recode_factor(Referenz, !!!c("LRV-Grenzwert" = "LRV", "WHO-Empfehlung 2021" = "AQG '21", "EKL-Zielwert" = "EKL"))
  ) %>% 
  dplyr::filter(Jahr %in% years & !is.na(Referenzwert)) %>% 
  dplyr::select(-starttime) %>% 
  dplyr::rename(
    Standort = site2,
    Messwert = value,
    Messwert_relativ = value_relative,
    Messgrösse = parameter,
    Messzeitraum = interval,
    Einheit = unit
  ) %>% 
  mutate(
    Standort = ifelse(is.na(Standort), paste0(as.character(site), "_NABEL"), as.character(Standort)),
    Standort = ifelse(Standort == "Zürich_Kaserne", "Zürich_Kaserne_NABEL", Standort)
  )

d0 <- 
  grenzwerte %>%
  dplyr::filter(Referenz == "WHO-Empfehlung 2021") %>% 
  bind_rows(tibble(
    parameter = c("PM10_nb_d1>45", "PM2.5_nb_d1>15", "NO2_nb_d1>25", "O3_mean_d1_max_h8gl_peak_season"), 
    Referenzwert = c(3, 3, 1, 60),
    Referenz = "WHO-Empfehlung 2021"
  )) %>% 
  mutate(
    Zeitbasis = ifelse(stringr::str_detect(parameter, "_nb"), "Kurzzeitbelastung", "Langzeitbelastung"),
    Referenz = dplyr::recode_factor(Referenz, !!!c("LRV-Grenzwert" = "LRV", "WHO-Empfehlung 2021" = "AQG '21", "EKL-Zielwert" = "EKL")),
    Messgrösse = dplyr::recode_factor(parameter, !!!par_plot2)
  ) %>% 
  dplyr::filter(Zeitbasis == "Kurzzeitbelastung")




p1 <-
  d %>% 
  dplyr::filter(Referenz == "AQG '21" & Zeitbasis == "Langzeitbelastung") %>% 
  mutate(
    Messgrösse = stringr::str_replace(Messgrösse, " JMW", "\nJMW"),
    Messgrösse = stringr::str_replace(Messgrösse, " peak season", "\npeak season"),
    Messgrösse = factor(Messgrösse, levels = c("PM10\nJMW", "PM2.5\nJMW", "NO2\nJMW", "O3\npeak season"))
  ) %>%
  ggplot(aes(x = factor(Jahr), y = Messwert_relativ, group = Jahr, color = Standortklasse, fill = Standortklasse)) + 
  geom_hline(yintercept = 1, color = "black", lwd = linesize) +
  geom_point(shape = 21, size = pointsize, position = "dodge") +
  scale_y_continuous(limits = c(0, 5.5), expand = c(0.01,0.01), labels = scales::percent_format(), breaks = seq(0, 100, 1)) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = alpha(cols, 0.3)) +
  facet_nested(~Zeitbasis+Messgrösse, nest_line = element_line(), resect = unit(1, "mm")) +
  ylab("Verhältnis Messwert : Richtwert") +
  theme_gray(base_size = basesize) +
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks = element_line(color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    ggh4x.facet.nestline = element_line(colour = "gray40")
  ) +
  theme(legend.position = "none")


p1b <-
  d %>% 
  dplyr::filter(Referenz == "AQG '21" & Zeitbasis == "Langzeitbelastung") %>% 
  mutate(
    Messgrösse = stringr::str_replace(Messgrösse, " JMW", "\nJMW"),
    Messgrösse = stringr::str_replace(Messgrösse, " peak season", "\npeak season"),
    Messgrösse = factor(Messgrösse, levels = c("PM10\nJMW", "PM2.5\nJMW", "NO2\nJMW", "O3\npeak season"))
  ) %>%
  dplyr::distinct(Messgrösse, site, Jahr, Messwert, .keep_all = TRUE) %>%
  ggplot(aes(x = Messgrösse, y = Messwert_relativ, color = Standortklasse, fill = Standortklasse)) + 
  geom_hline(yintercept = 1, color = "black", lwd = linesize * 1.5) +
  # geom_point(shape = 21, size = pointsize, position = "dodge") +
  geom_jitter(shape = 21, size = pointsize, width = 0.075) +
  scale_y_continuous(limits = c(0, 5.5), expand = c(0.01,0.01), labels = scales::percent_format(), breaks = seq(0, 100, 1)) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = alpha(cols, 0.3)) +
  facet_wrap(Zeitbasis~.) +
  ylab("Verhältnis Messwert : Richtwert") +
  theme_minimal(base_size = basesize) +
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(color = "gray40"),
    axis.ticks.y = element_line(color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    axis.title.x = element_blank(),
    ggh4x.facet.nestline = element_line(colour = "gray40")
  ) +
  theme(legend.position = "none")


d1 <- 
  d0 %>% 
  mutate(
    Referenzwert = case_when(
      Messgrösse == "PM2.5 TMW > 15 µg/m3" ~ Referenzwert / 100,
      Messgrösse == "NO2 TMW > 25 µg/m3" ~ Referenzwert / 100,
      TRUE ~ Referenzwert
    ),
    Messgrösse = stringr::str_replace(Messgrösse, " >", "\n>"),
    Messgrösse = factor(Messgrösse, levels = c("PM10 TMW\n> 45 µg/m3", "PM2.5 TMW\n> 15 µg/m3", "NO2 TMW\n> 25 µg/m3"))
  )

TMW_colored <- 
  strip_nested(
    text_x = elem_list_text(color = c(rep("black", 2), rep("deepskyblue4", 2))),
    by_layer_x = FALSE
  )


p2 <-
  d %>%
  dplyr::filter(Referenz == "AQG '21" & Zeitbasis == "Kurzzeitbelastung") %>%
  mutate(
    Messwert_relativ = case_when(
      Messgrösse == "PM2.5 TMW > 15 µg/m3" ~ Messwert_relativ / 100,
      Messgrösse == "NO2 TMW > 25 µg/m3" ~ Messwert_relativ / 100,
      TRUE ~ Messwert_relativ
    ),
    Messgrösse = stringr::str_replace(Messgrösse, " >", "\n>"),
    Messgrösse = factor(Messgrösse, levels = c("PM10 TMW\n> 45 µg/m3", "PM2.5 TMW\n> 15 µg/m3", "NO2 TMW\n> 25 µg/m3"))
  ) %>%
  ggplot(aes(x = factor(Jahr), y = Messwert_relativ, group = Jahr, color = Standortklasse, fill = Standortklasse)) +
  geom_hline(data = d1, mapping = aes(yintercept = Referenzwert), color = "black", lwd = linesize, inherit.aes = FALSE) +
  geom_point(shape = 21, size = pointsize, position = "dodge") +
  scale_y_continuous(
    limits = c(0, 4), breaks = seq(0, 100, 1), expand = c(0.01,0.01),
    sec.axis = sec_axis(trans = ~.*100)
  ) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = alpha(cols, 0.3)) +
  facet_nested(~Zeitbasis+Messgrösse, nest_line = element_line(), resect = unit(1, "mm"), strip = TMW_colored) +
  ylab("Anzahl Messwerte > Richtwert") +
  theme_gray(base_size = basesize) +
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks = element_line(color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    ggh4x.facet.nestline = element_line(colour = "gray40"),
    axis.text.y.right = element_text(color = "deepskyblue4"),
    axis.ticks.y.right = element_line(color = "deepskyblue4")
  ) +
  theme(legend.position = "right")


d1 <- 
  d0 %>% 
  mutate(
    Referenzwert = case_when(
      Messgrösse == "PM2.5 TMW > 15 µg/m3" ~ Referenzwert / 100,
      Messgrösse == "NO2 TMW > 25 µg/m3" ~ Referenzwert / 100,
      TRUE ~ Referenzwert
    ),
    Messgrösse = stringr::str_replace(Messgrösse, " TMW", "\nTMW"),
    Messgrösse = factor(Messgrösse, levels = c("PM10\nTMW > 45 µg/m3", "PM2.5\nTMW > 15 µg/m3", "NO2\nTMW > 25 µg/m3"))
  )

p2b <-
  d %>%
  dplyr::filter(Referenz == "AQG '21" & Zeitbasis == "Kurzzeitbelastung") %>%
  mutate(
    Messwert_relativ = case_when(
      Messgrösse == "PM2.5 TMW > 15 µg/m3" ~ Messwert_relativ / 100,
      Messgrösse == "NO2 TMW > 25 µg/m3" ~ Messwert_relativ / 100,
      TRUE ~ Messwert_relativ
    ),
    Messgrösse = stringr::str_replace(Messgrösse, " TMW", "\nTMW"),
    Messgrösse = factor(Messgrösse, levels = c("PM10\nTMW > 45 µg/m3", "PM2.5\nTMW > 15 µg/m3", "NO2\nTMW > 25 µg/m3"))
  ) %>%
  ggplot(aes(x = as.numeric(Messgrösse), y = Messwert_relativ, color = Standortklasse, fill = Standortklasse)) +
  geom_segment(data = d1, mapping = aes(x = as.numeric(Messgrösse) - 0.33, xend = as.numeric(Messgrösse) + 0.33, y = Referenzwert, yend = Referenzwert), color = "black", lwd = linesize * 1.5, inherit.aes = FALSE) +
  geom_jitter(shape = 21, size = pointsize, width = 0.075) +
  scale_x_continuous(breaks = 1:length(levels(d1$Messgrösse)), labels = levels(d1$Messgrösse)) +
  scale_y_continuous(
    limits = c(0, 4), breaks = seq(0, 100, 1), expand = c(0.01,0.01),
    sec.axis = sec_axis(trans = ~.*100)
  ) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = alpha(cols, 0.3)) +
  facet_wrap(Zeitbasis~.) +
  ylab("Anzahl Messwerte > Richtwert") +
  theme_minimal(base_size = basesize) +
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(color = "gray40"),
    axis.ticks.y = element_line(color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1, color = c("black", "deepskyblue4", "deepskyblue4")),
    axis.title.x = element_blank(),
    ggh4x.facet.nestline = element_line(colour = "gray40"),
    axis.text.y.right = element_text(color = "deepskyblue4"),
    axis.ticks.y.right = element_line(color = "deepskyblue4")
  ) +
  theme(legend.position = "right")


# plots$belastung_relativ_WHO <- cowplot::plot_grid(p1, p2, rel_widths = c(2.5,3))
plots$belastung_relativ_WHO <- cowplot::plot_grid(p1b, p2b, rel_widths = c(2.5,3), axis = "tblr", align = "h")










### plot Langzeitreihe N-Einträge Bachtel
### ----------------------------------------------------------------------
data_bachtel <- 
  readr::read_delim("data/Ntot_Frachten_Jahreswerte_Mischwald_2000 - 2021.csv", skip = 2) %>% 
  dplyr::select(`...3`, `aus NH3-Quellen`, `aus NOx-Quellen`) %>% 
  dplyr::rename(year = `...3`) %>% 
  gather(parameter, value, -year) %>% 
  mutate(parameter = factor(parameter, levels = rev(c("aus NH3-Quellen", "aus NOx-Quellen"))))

plots$bachtel <- 
  data_bachtel %>% 
  ggplot(aes(x = year, y = value, fill = parameter)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = clo_bachtel$max, lty = 2, lwd = linesize) +
  geom_hline(yintercept = clo_bachtel$mid, lty = 1, lwd = linesize) +
  geom_hline(yintercept = clo_bachtel$min, lty = 2, lwd = linesize) +
  scale_y_continuous(limits = c(0,40), expand = c(0,0)) +
  scale_fill_manual(name = "Quellbeitrag", values = c("aus NH3-Quellen" = "gold2", "aus NOx-Quellen" = "#607D8B")) +
  ylab("Stickstoffeintrag (kg-N/ha/Jahr)") +
  ggtitle("Bachtel / Mischwald") +
  theme_ts2 +
  theme(
    legend.position = "right",
    panel.grid.minor.y = element_line(),
    axis.line.x = element_line(color = "gray30"),
    axis.title.y = element_text()
  )

# https://www.r-bloggers.com/2018/12/having-bit-of-party-with-material-colour-palette/






### plot Exposition 
### ----------------------------------------------------------------------

### Bevölkerungsexpo
str(data_expo)

refs <- 
  tibble(
    parameter = rep(factor(c("PM10 (μg/m3)", "PM2.5 (μg/m3)", "eBC (μg/m3)", "NO2 (μg/m3)"), 
                           levels = c("PM10 (μg/m3)", "PM2.5 (μg/m3)", "eBC (μg/m3)", "NO2 (μg/m3)")), 2),
    threshold = c(20, 10, 0.1, 30, 15, 5, 0.1, 10),
    Referenz = c("LRV", "LRV", "EKL", "LRV", "AQG '21", "AQG '21", "EKL", "AQG '21")
  ) %>% 
  dplyr::distinct(parameter, threshold, Referenz) 

d <-
  data_expo %>% 
  dplyr::filter(!is.na(mean) & mean != 0 & !is.na(GEM) & R21BTOT > 0) %>%
  mutate() %>% 
  mutate(
    min = ifelse(parameter == "bc_21_mod", min / 100, min / 10),
    q25 = ifelse(parameter == "bc_21_mod", q25 / 100, q25 / 10),
    median = ifelse(parameter == "bc_21_mod", median / 100, median / 10),
    mean = ifelse(parameter == "bc_21_mod", mean / 100, mean / 10),
    q75 = ifelse(parameter == "bc_21_mod", q75 / 100, q75 / 10),
    max = ifelse(parameter == "bc_21_mod", max / 100, max / 10),
    parameter = factor(dplyr::recode(parameter, 
                                     !!!c("bc_21_mod" = "eBC (μg/m3)", "no2_21_mod" = "NO2 (μg/m3)", 
                                          "pm10_21_mod" = "PM10 (μg/m3)", "pm25_21_mod" = "PM2.5 (μg/m3)")), 
                       levels = c("PM10 (μg/m3)", "PM2.5 (μg/m3)", "eBC (μg/m3)", "NO2 (μg/m3)"))
  ) %>% 
  gather(stat, value, -RELI, -GEM, -R21BTOT, -parameter) %>%
  dplyr::filter(stat %in% c("min", "mean", "max")) %>%
  mutate(value = round(value, 1)) %>% 
  group_by(parameter, stat, value) %>%
  dplyr::summarise(R21BTOT = sum(R21BTOT)) %>% 
  group_by(parameter, stat) %>%
  dplyr::arrange(value) %>%
  mutate(
    R21BTOT = cumsum(R21BTOT),
    fraction = R21BTOT / max(R21BTOT)
  ) %>%
  ungroup() %>% 
  dplyr::arrange(parameter, stat, value) # %>% left_join(refs, by = "parameter")

d2 <-
  d %>% 
  dplyr::filter(stat == "min") %>% 
  dplyr::select(-stat, -fraction) %>% 
  dplyr::rename(min = R21BTOT) %>% 
  full_join(dplyr::filter(d, stat == "max"), by = c("parameter", "value")) %>% 
  dplyr::select(-stat, -fraction) %>% 
  dplyr::rename(max = R21BTOT)
d2 <-
  d %>% 
  dplyr::filter(stat == "mean") %>% 
  dplyr::select(-stat) %>% 
  dplyr::rename(mean = R21BTOT) %>% 
  full_join(d2, by = c("parameter", "value")) %>%
  dplyr::filter(parameter != "eBC (μg/m3)") %>% 
  dplyr::arrange(parameter, value) %>% 
  group_by(parameter) %>% 
  mutate(
    min = zoo::na.locf(min, na.rm = FALSE),
    max = zoo::na.locf(max, na.rm = FALSE),
    mean = zoo::na.locf(mean, na.rm = FALSE),
    fraction = zoo::na.locf(fraction, na.rm = FALSE)
  ) %>% 
  ungroup()
  


plots$expo <-
  d2 %>% 
  ggplot(aes(x = value, y = mean, group = parameter)) +
  geom_ribbon(aes(x = value, ymin = min, ymax = max), fill = "lightsteelblue", color = NA) +
  geom_line(size = linesize, color = "steelblue") +
  geom_vline(data = dplyr::filter(refs, parameter != "eBC (μg/m3)"), mapping = aes(xintercept = threshold, linetype = Referenz), size = linesize, show.legend = TRUE) +
  scale_x_continuous(expand = c(0.05,0.05)) +
  scale_y_continuous(limits = c(0, NA), breaks = seq(0, 3000000, 300000), expand = c(0.0,0.0), labels = function(x) format(x, big.mark = "'"), 
                     sec.axis = sec_axis(trans = ~./max(.), labels = scales::percent_format(), name = "Anzahl Einwohner relativ")) +
  scale_linetype_manual(name = "Referenz:", values = rev(c("AQG '21" = 2, "LRV" = 1))) +
  facet_wrap(parameter~., scales = "free_x", nrow = 1, strip.position = "bottom") +
  ylab("Anzahl Einwohner") +
  guides(color = "none") +
  theme_gray(base_size = basesize) +
  theme(
    axis.ticks = element_line(color = "gray60"),
    strip.background = element_blank(),
    strip.placement = "outside",
    axis.text = element_text(color = "gray30"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = basesize, color = "gray30"),
    strip.text = element_text(hjust = 0.5, size = basesize, color = "gray30"),
    legend.title = element_blank(),
    legend.position = "bottom"
  )





### Ökosystem-Expo
# dplyr::filter(data_clo, ndep < 0)
cols_ndep <- c("Hochmoore" = "red3", "Flachmoore" = "goldenrod2", "Trockenwiesen/-weiden" = "steelblue", "Wald" = "olivedrab")

plots$expo_clo <-
  data_clo %>% 
  group_by(ecosystem) %>% 
  dplyr::arrange(ndep) %>% 
  mutate(fraction = 1:length(ndep) / length(ndep)) %>% 
  ungroup() %>% 
  ggplot(aes(x = ndep, y = fraction, group = ecosystem, color = ecosystem)) +
  geom_line(size = linesize) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0,1,0.2), labels = scales::percent_format()) + 
  scale_color_manual(name = "Ökosystem", values = cols_ndep) +
  xlab("Überschreitung Critical Load (kgN/ha/Jahr)") +
  ylab("Anteil an Ökosystem-Gesamtfläche") +
  theme_gray(base_size = basesize) +
  theme(
    axis.ticks = element_line(color = "gray60"),
    axis.text = element_text(color = "gray30"),
    axis.title.x = element_text(size = basesize, color = "gray30"),
    axis.title.y = element_text(size = basesize, color = "gray30"),
    legend.title = element_blank()
  )



# data_clo %>% 
#   ggplot(aes(x = ndep, group = ecosystem, fill = ecosystem)) +
#   stat_bin( breaks = seq(-2,40,2)) +
#   facet_wrap(ecosystem~., scales = "free_y") + 
#   xlab("Überschreitung Critical Load (kgN/ha/Jahr)") +
#   ylab("Anzahl Ökosystemflächen")
 #



















# d %>% 
#   dplyr::filter(Referenz %in% c("LRV", "EKL")) %>% 
#   dplyr::filter(Referenz != "EKL") %>% 
#   mutate(
#     Messwert_relativ = case_when(
#     #   Zeitbasis == "Kurzzeitbelastung" & Messgrösse != "O3 SMW > 120 µg/m3" ~ Messwert_relativ / 2,
#       Messgrösse == "O3 SMW > 120 µg/m3" ~ Messwert_relativ / 200,
#       TRUE ~ Messwert_relativ
#       ),
#     Messgrösse = stringr::str_replace(Messgrösse, ">", "\n>"),
#     Zeitbasis = factor(Zeitbasis, levels = c("Langzeitbelastung", "Kurzzeitbelastung"))
#     ) %>% 
#   ggplot(aes(x = factor(Jahr), y = Messwert_relativ, group = Jahr, color = Standortklasse, fill = Standortklasse)) + 
#   # geom_hline(yintercept = 1, color = "black", lwd = linesize) +
#   geom_point(shape = 21, size = pointsize, position = "dodge") +
#   # scale_y_continuous(
#   #   limits = c(0, NA), expand = c(0.01,0.01), labels = scales::percent_format(), breaks = seq(0, 100, 1), 
#   #   sec.axis = sec_axis(trans = ~.*2, name = "Kurzzeitbelastung\nAnzahl Messwert > Grenzwert")
#   #     ) +
#   # facetted_pos_scales(
#   #   y = list(
#   #     Zeitbasis == "Langzeitbelastung" ~ scale_y_continuous(limits = c(0, 2), labels = scales::percent_format(), breaks = seq(0, 50, 0.5), expand = c(0.01,0.01)),
#   #     Zeitbasis == "Kurzzeitbelastung" ~ scale_y_continuous(limits = c(0, 6), breaks = seq(0, 10, 2), expand = c(0.01,0.01))
#   #   )
#   # ) +
#   # facet_grid(~Messgrösse) +
#   scale_color_manual(values = cols) +
#   scale_fill_manual(values = alpha(cols, 0.3)) +
#   facet_nested(~Zeitbasis+Messgrösse, nest_line = element_line(), resect = unit(1, "mm"), scales = "free_y", independent = "y") +
#   ylab("Langzeitbelastung\nMesswert : Grenzwert") +
#   theme_gray(base_size = basesize) +
#   theme(
#     strip.placement = "outside",
#     strip.background = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     axis.ticks = element_line(color = "gray40"),
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     axis.title.x = element_blank(),
#     ggh4x.facet.nestline = element_line(colour = "gray40")
#   ) +
#   theme(legend.position = "right")
# 
# 
# 
# 
# plots$langzeitbelastung_relativ_LRV <-
#   d %>% 
#   dplyr::filter(Zeitbasis == "Langzeitbelastung" & Referenz %in% c("LRV", "EKL")) %>% 
#   dplyr::filter(Referenz != "EKL") %>%
#   ggplot(aes(x = factor(Jahr), y = Messwert_relativ, group = Jahr, color = Standortklasse, fill = Standortklasse)) + 
#   geom_hline(yintercept = 1, color = "black", lwd = linesize) +
#   geom_point(shape = 21, size = pointsize, position = "dodge") +
#   scale_y_continuous(breaks = seq(0, 100, 0.25), limits = c(0, NA), expand = c(0.01,0.01), labels = scales::percent_format()) +
#   scale_color_manual(values = cols) +
#   scale_fill_manual(values = alpha(cols, 0.3)) +
#   # facet_nested(~Zeitbasis+Messgrösse, nest_line = element_line(), resect = unit(1, "mm")) +
#   facet_grid(~Messgrösse) +
#   ylab("Messwert : Grenzwert") +
#   theme_gray(base_size = basesize) +
#   theme(
#     strip.placement = "outside",
#     strip.background = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     axis.ticks = element_line(color = "gray40"),
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     axis.title.x = element_blank(),
#     ggh4x.facet.nestline = element_line(colour = "gray40")
#   ) +
#   theme(legend.position = "right")
# 
# 
# 
# plots$kurzzeitbelastung_relativ_alternativ_LRV <-
#   d %>%
#   dplyr::filter(Zeitbasis == "Kurzzeitbelastung" & Referenz == "LRV") %>%
#   mutate(Messgrösse = stringr::str_replace(Messgrösse, ">", "\n>")) %>% 
#   ggplot(aes(x = factor(Jahr), y = Messwert, color = Standortklasse, fill = Standortklasse)) +
#   geom_hline(data = mutate(d0, Messgrösse = stringr::str_replace(Messgrösse, ">", "\n>")), mapping = aes(yintercept = Referenzwert), color = "black", lwd = linesize, inherit.aes = FALSE) +
#   geom_point(shape = 21, size = pointsize) +
#   scale_y_continuous(limits = c(0, NA), expand = c(0.01,0.01)) +
#   scale_color_manual(values = cols) +
#   scale_fill_manual(values = alpha(cols, 0.3)) +
#   ylab("Anzahl Messwert > Grenzwert") +
#   # facet_nested(~Zeitbasis+Messgrösse+Referenz, nest_line = element_line(), resect = unit(1, "mm")) +
#   facet_grid(~Messgrösse) +
#   theme_gray(base_size = basesize) +
#   theme(
#     strip.placement = "outside",
#     strip.background = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     axis.ticks = element_line(color = "gray40"),
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     axis.title.x = element_blank(),
#     ggh4x.facet.nestline = element_line(colour = "gray40")
#   ) +
#   scale_y_break(breaks = c(8,20), scales = 0.5, expand = FALSE)
# # plotsplots$kurzzeitbelastung_relativ_alternativ_LRV
# 
# 
# 
# plots$kurzzeitbelastung_relativ1_LRV <-
#   d %>% 
#   dplyr::filter(Zeitbasis == "Kurzzeitbelastung" & Referenz == "LRV") %>%
#   dplyr::filter(Messgrösse != "O3 SMW > 120 µg/m3") %>%
#   ggplot(aes(x = factor(Jahr), y = Messwert, color = Standortklasse, fill = Standortklasse)) + 
#   geom_hline(data = dplyr::filter(d0, parameter != "O3_nb_h1>120"), mapping = aes(yintercept = Referenzwert), color = "black", lwd = linesize, inherit.aes = FALSE) +
#   geom_point(shape = 21, size = pointsize) + 
#   scale_y_continuous(limits = c(0, NA), breaks = seq(0, 20, 1), expand = c(0.01,0.01)) +
#   scale_color_manual(values = cols) +
#   scale_fill_manual(values = alpha(cols, 0.3)) +
#   ylab("Anzahl Messwert > Grenzwert") +
#   # facet_nested(~Zeitbasis+Messgrösse, nest_line = element_line(), resect = unit(1, "mm")) +
#   facet_grid(~Messgrösse) +
#   theme_gray(base_size = basesize) +
#   theme(
#     strip.placement = "outside",
#     strip.background = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     axis.ticks = element_line(color = "gray40"),
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     axis.title.x = element_blank(),
#     ggh4x.facet.nestline = element_line(colour = "gray40")
#   )  +
#   theme(legend.position = "bottom") # + scale_y_break(breaks = c(30,31), scales = 0.33, expand = c(0.05,0.05))
# # plots$kurzzeitbelastung_relativ1_LRV
# 
# 
# 
# plots$kurzzeitbelastung_relativ2_LRV <-
#   d %>% 
#   dplyr::filter(Zeitbasis == "Kurzzeitbelastung" & Referenz == "LRV") %>%
#   dplyr::filter(Messgrösse == "O3 SMW > 120 µg/m3") %>%
#   ggplot(aes(x = factor(Jahr), y = Messwert, color = Standortklasse, fill = Standortklasse)) + 
#   geom_hline(data = dplyr::filter(d0, parameter == "O3_nb_h1>120"), mapping = aes(yintercept = Referenzwert), color = "black", lwd = linesize, inherit.aes = FALSE) +
#   geom_point(shape = 21, size = pointsize) + 
#   scale_y_continuous(limits = c(0, NA), breaks = seq(0,2000,100), expand = c(0.01,0.01)) +
#   scale_color_manual(values = cols) +
#   scale_fill_manual(values = alpha(cols, 0.3)) +
#   ylab("Anzahl Messwert > Grenzwert") +
#   # facet_nested(~Zeitbasis+Messgrösse, nest_line = element_line(), resect = unit(1, "mm")) +
#   facet_grid(~Messgrösse) +
#   theme_gray(base_size = basesize) +
#   theme(
#     strip.placement = "outside",
#     strip.background = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     axis.ticks = element_line(color = "gray40"),
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     axis.title.x = element_blank(),
#     ggh4x.facet.nestline = element_line(colour = "gray40")
#   )  +
#   theme(legend.position = "right", legend.title = element_blank()) # + scale_y_break(breaks = c(30,31), scales = 0.33, expand = c(0.05,0.05))
# 
# 
# 
# 
# ### nur WHO
# plots$langzeitbelastung_relativ_WHO <-
#   d %>% 
#   dplyr::filter(Zeitbasis == "Langzeitbelastung" & Referenz == "WHO '21") %>% 
#   dplyr::filter(Referenz != "EKL") %>%
#   ggplot(aes(x = factor(Jahr), y = Messwert_relativ, group = Jahr, color = Standortklasse, fill = Standortklasse)) + 
#   geom_hline(yintercept = 1, color = "black", lwd = linesize) +
#   geom_point(shape = 21, size = pointsize, position = "dodge") +
#   scale_y_continuous(breaks = seq(0, 100, 1), limits = c(0, NA), expand = c(0.01,0.01), labels = scales::percent_format()) +
#   scale_color_manual(values = cols) +
#   scale_fill_manual(values = alpha(cols, 0.3)) +
#   # facet_nested(~Zeitbasis+Messgrösse, nest_line = element_line(), resect = unit(1, "mm")) +
#   facet_grid(~Messgrösse) +
#   ylab("Messwert : Richtwert") +
#   theme_gray(base_size = basesize) +
#   theme(
#     strip.placement = "outside",
#     strip.background = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     axis.ticks = element_line(color = "gray40"),
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     axis.title.x = element_blank(),
#     ggh4x.facet.nestline = element_line(colour = "gray40")
#   ) +
#   theme(legend.position = "right")
# # ...
# 
# 


### LRV & WHO zusammen

# p <-
#   d %>% 
#   dplyr::filter(Zeitbasis == "Langzeitbelastung") %>% 
#   ggplot(aes(x = factor(Jahr), y = Messwert_relativ, color = Standortklasse, fill = Standortklasse)) + 
#   geom_hline(yintercept = 1) +
#   geom_jitter(width = 0.1, shape = 21, size = 3) + 
#   scale_y_continuous(limits = c(0, NA), expand = c(0.01, 0.01), labels = scales::percent_format()) +
#   # coord_flip() +
#   scale_color_manual(values = cols) +
#   scale_fill_manual(values = alpha(cols, 0.3)) +
#   # facet_wrap(Referenz~Messgrösse, nrow = 1) +
#   facet_nested(~Referenz+Messgrösse, nest_line = element_line(), switch = "x", scales = "free_y") +
#   theme_minimal(base_size = basesize) +
#   theme(
#     strip.placement = "outside",
#     # strip.text.y.left = element_text(angle = 0, hjust = 1),
#     panel.grid.minor = element_blank(),
#     panel.grid.major.x = element_blank(),
#     axis.ticks.x = element_line(color = "gray40"),
#     axis.title.x = element_blank(),
#     axis.line.x = element_line(color = "gray40"),
#     ggh4x.facet.nestline = element_line(colour = "gray40")
#   )
# p


# plots$langzeitbelastung_relativ <- 
#   d %>% 
#   dplyr::filter(Zeitbasis == "Langzeitbelastung") %>% 
#   dplyr::filter(Referenz != "EKL") %>%
#   ggplot(aes(x = factor(Jahr), y = Messwert_relativ, group = Jahr, color = Standortklasse, fill = Standortklasse)) + 
#   geom_hline(yintercept = 1, color = "black", lwd = linesize) +
#   # geom_jitter(width = 0.1, shape = 21, size = 2) + 
#   geom_point(shape = 21, size = pointsize, position = "dodge") +
#   scale_y_continuous(breaks = seq(0, 100, 1), limits = c(0, NA), expand = c(0.01,0.01), labels = scales::percent_format()) +
#   scale_color_manual(values = cols) +
#   scale_fill_manual(values = alpha(cols, 0.3)) +
#   # coord_flip() + 
#   # facet_nested(Messgrösse+Referenz~., nest_line = element_line(), switch = "y", scales = "free_y", resect = unit(1, "mm")) +
#   facet_nested(~Zeitbasis+Messgrösse+Referenz, nest_line = element_line(), resect = unit(1, "mm")) +
#   # facet_grid(.~Messgrösse+Referenz, scales = "free", space = "free") +
#   ylab("Messwert : Referenzwert") +
#   theme_gray(base_size = basesize) +
#   theme(
#     strip.placement = "outside",
#     strip.background = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     axis.ticks = element_line(color = "gray40"),
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     axis.title.x = element_blank(),
#     # axis.line.x = element_line(color = "gray40"),
#     ggh4x.facet.nestline = element_line(colour = "gray40")
#   ) +
#   theme(legend.position = "bottom", legend.title = element_blank())
# # plots$langzeitbelastung_relativ
# 
# 
# 
# 
# plots$kurzzeitbelastung_relativ1 <-
#   d %>% 
#   dplyr::filter(Zeitbasis == "Kurzzeitbelastung") %>%
#   dplyr::filter(Messgrösse != "O3 SMW > 120 µg/m3") %>%
#   ggplot(aes(x = factor(Jahr), y = Messwert, color = Standortklasse, fill = Standortklasse)) + 
#   geom_hline(data = dplyr::filter(d0, parameter != "O3_nb_h1>120"), mapping = aes(yintercept = Referenzwert), color = "black", lwd = linesize, inherit.aes = FALSE) +
#   geom_point(shape = 21, size = pointsize) + 
#   scale_y_continuous(limits = c(0, NA), breaks = seq(0, 20, 1), expand = c(0.01,0.01)) +
#   scale_color_manual(values = cols) +
#   scale_fill_manual(values = alpha(cols, 0.3)) +
#   ylab("Anzahl Messwert > Referenzwert") +
#   facet_nested(~Zeitbasis+Messgrösse+Referenz, nest_line = element_line(), resect = unit(1, "mm")) +
#   # facet_grid(~Messgrösse+Referenz) + 
#   theme_gray(base_size = basesize) +
#   theme(
#     strip.placement = "outside",
#     strip.background = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     axis.ticks = element_line(color = "gray40"),
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     axis.title.x = element_blank(),
#     ggh4x.facet.nestline = element_line(colour = "gray40")
#   )  +
#   theme(legend.position = "bottom", legend.title = element_blank()) # + scale_y_break(breaks = c(30,31), scales = 0.33, expand = c(0.05,0.05))
# # plots$kurzzeitbelastung_relativ1
# 
# 
# 
# plots$kurzzeitbelastung_relativ2 <-
#   d %>% 
#   dplyr::filter(Zeitbasis == "Kurzzeitbelastung") %>%
#   dplyr::filter(Messgrösse == "O3 SMW > 120 µg/m3") %>%
#   ggplot(aes(x = factor(Jahr), y = Messwert, color = Standortklasse, fill = Standortklasse)) + 
#   geom_hline(data = dplyr::filter(d0, parameter == "O3_nb_h1>120"), mapping = aes(yintercept = Referenzwert), color = "black", lwd = linesize, inherit.aes = FALSE) +
#   geom_point(shape = 21, size = pointsize) + 
#   scale_y_continuous(limits = c(0, NA), breaks = seq(0,2000,100), expand = c(0.01,0.01)) +
#   scale_color_manual(values = cols) +
#   scale_fill_manual(values = alpha(cols, 0.3)) +
#   ylab("Anzahl Messwert > Referenzwert") +
#   facet_nested(~Zeitbasis+Messgrösse+Referenz, nest_line = element_line(), resect = unit(1, "mm")) +
#   theme_gray(base_size = basesize) +
#   theme(
#     strip.placement = "outside",
#     strip.background = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.x = element_blank(),
#     axis.ticks = element_line(color = "gray40"),
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     axis.title.x = element_blank(),
#     ggh4x.facet.nestline = element_line(colour = "gray40")
#   )  +
#   theme(legend.position = "bottom", legend.title = element_blank()) # + scale_y_break(breaks = c(30,31), scales = 0.33, expand = c(0.05,0.05))
# # plots$kurzzeitbelastung_relativ2







# p1b <- p1 + theme(legend.position = "none")
# p2b <- p2 + theme(legend.position = "none")
# p4 <- cowplot::plot_grid(p1b, p2b, p3, rel_widths = c(50,20,50), nrow = 1)
# p4


rm(list = c("d", "d0"))
