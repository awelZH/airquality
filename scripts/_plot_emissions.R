# Plots of the air pollutant emissions -> plots_emissions (needs scripts/_plot_setup.R)

plots <- list()


# plotting air pollutant emissions
# ---
# read & plot details of Canton Zürich air pollutant emissions per pollutant, subsector and year (absolute and relative values)
data_emikat <- airquality.methods::read_local_csv(ressources_plotting$emissions$emikat, delim = ";", locale = readr::locale(encoding = "UTF-8"))
pollutants <- setNames(unique(data_emikat$pollutant), unique(data_emikat$pollutant))

# ... for NH3, agriculture last (bottom of the stack) to better illustrate its influence over time
sectors_last <- function(pollutant) if (pollutant == "NH3") "Land- und Forstw." else NULL

# absolute values
plots$emissions$inventory_absolute <-
  lapply(pollutants, function(pollutant) {
    ggplot_emissions(data = dplyr::filter(data_emikat, pollutant == !!pollutant), theme = theme_ts,
                     sectors_last = sectors_last(pollutant))
  })

# relative values
plots$emissions$inventory_relative <-
  lapply(pollutants, function(pollutant) {
    ggplot_emissions(data = dplyr::filter(data_emikat, pollutant == !!pollutant), relative = TRUE, pos = "fill", theme = theme_ts,
                     sectors_last = sectors_last(pollutant))
  })


# read & plot RSD NOx emissions by vehicle type, fuel type and euronorm
data_rsd_per_norm <- airquality.methods::read_local_csv(ressources_plotting$emissions$rsd_norm)

plots$emissions$rsd_norm$NOx <-
  data_rsd_per_norm |> 
  dplyr::filter(vehicle_euronorm != "Euro6c") |> # only few measured vehicles and not relevant for vehicle fleet...
  tidyr::expand(vehicle_type, vehicle_fuel_type, vehicle_euronorm) |>
  dplyr::left_join(data_rsd_per_norm, by = c("vehicle_type", "vehicle_fuel_type", "vehicle_euronorm")) |>
  dplyr::mutate(
    vehicle_type = dplyr::recode_factor(vehicle_type, !!!c("passenger car" = "Personenwagen", "light duty vehicle" = "leichte Nutzfahrzeuge")),
    vehicle_fuel_type = dplyr::recode_factor(vehicle_fuel_type, !!!c("gasoline" = "Benzin", "diesel" = "Diesel")),
    vehicle_euronorm = factor(vehicle_euronorm)
  ) |> 
  ggplot2::ggplot(aes(x = vehicle_euronorm, y = emission, group = vehicle_type, fill = vehicle_type)) +
  ggplot2::geom_bar(stat = "identity", width = 0.75, position = ggplot2::position_dodge()) +
  # ggiraph::geom_bar_interactive(mapping = ggplot2::aes(data_id = vehicle_type, tooltip = round_off(nox_emission, 1)), stat = "identity", width = 0.75, position = ggplot2::position_dodge()) +
  ggplot2::geom_linerange(mapping = aes(ymin = emission - standarderror, ymax = emission + standarderror), color = "gray60", position = ggplot2::position_dodge(width = 0.75)) +
  ggplot2::geom_segment(mapping = aes(x = as.numeric(vehicle_euronorm) - 0.45, xend = as.numeric(vehicle_euronorm) + 0.45, y = nox_emission_threshold_g_per_kg_fuel, yend = nox_emission_threshold_g_per_kg_fuel), color = "red3", linewidth = 1) +
  ggplot2::facet_wrap(vehicle_fuel_type~., strip.position = "top") +
  ggplot2::scale_y_continuous(limits = c(0,25), breaks = seq(0,25,5), expand = c(0.01,0.01), labels = function(x) format(x, big.mark = "'")) +
  ggplot2::scale_fill_manual(name = "Fahrzeugkategorie:", values = c("Personenwagen" = "cadetblue3", "leichte Nutzfahrzeuge" = "darkslategray")) +
  # ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1)) +
  ggplot2::ggtitle(
    label = openair::quickText("Abgasmessungen von Stickoxiden im realen Fahrbetrieb"),
    subtitle = openair::quickText("NOx Emissionen, Mittelwert pro Abgasnorm (g/kg Treibstoff)")
  ) +
  ggplot2::labs(caption = "Daten: Kanton Zürich/AWEL") +
  theme_ts +
  ggplot2::theme(
    strip.background = ggplot2::element_blank(),
    # strip.background = ggplot2::element_rect(color = "gray40"),
    strip.placement = "outside",
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom"
  )

# plots$emissions$rsd_norm$NOx <- ggiraph::girafe(ggobj = plots$emissions$rsd_norm$NOx, width_svg = 6, height_svg = 4)


# read & plot RSD NOx emissions by vehicle model year, vehicle type and fuel type
data_rsd_per_yearmodel <- airquality.methods::read_local_csv(ressources_plotting$emissions$rsd_yearmodel)

plots$emissions$rsd_yearmodel$NOx <-
  data_rsd_per_yearmodel |> 
  tidyr::expand(vehicle_type, vehicle_fuel_type, vehicle_model_year) |> 
  dplyr::left_join(data_rsd_per_yearmodel, by = c("vehicle_type", "vehicle_fuel_type", "vehicle_model_year")) |> 
  dplyr::mutate(
    vehicle_type = dplyr::recode_factor(vehicle_type, !!!c("passenger car" = "Personenwagen", "light duty vehicle" = "leichte Nutzfahrzeuge")),
    vehicle_fuel_type = dplyr::recode_factor(vehicle_fuel_type, !!!c("gasoline" = "Benzin", "diesel" = "Diesel"))
  ) |> 
  ggplot2::ggplot(aes(x = vehicle_model_year, y = emission, group = vehicle_type, fill = vehicle_type)) +
  ggplot2::geom_bar(stat = "identity", width = 0.75, position = ggplot2::position_dodge()) +
  # ggiraph::geom_bar_interactive(mapping = ggplot2::aes(data_id = vehicle_type, tooltip = round_off(nox_emission, 1)), stat = "identity", width = 0.75, position = ggplot2::position_dodge()) +
  ggplot2::geom_linerange(mapping = aes(ymin = emission - standarderror, ymax = emission + standarderror), color = "gray60", position = ggplot2::position_dodge(width = 0.75)) +
  ggplot2::geom_step(mapping = aes(x = vehicle_model_year + 0.475, y = nox_emission_threshold_g_per_kg_fuel), color = "red3", linewidth = 1) +
  ggplot2::facet_wrap(vehicle_fuel_type~., strip.position = "top") +
  ggplot2::scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01), labels = function(x) format(x, big.mark = "'")) +
  ggplot2::scale_fill_manual(name = "Fahrzeugkategorie:", values = c("Personenwagen" = "cadetblue3", "leichte Nutzfahrzeuge" = "darkslategray")) +
  ggplot2::ggtitle(
    label = openair::quickText("Abgasmessungen von Stickoxiden im realen Fahrbetrieb"),
    subtitle = openair::quickText("NOx Emissionen, Mittelwert pro Fahrzeug-Modelljahr (g/kg Treibstoff)")
  ) +
  ggplot2::labs(caption = "Daten: Kanton Zürich/AWEL") +
  theme_ts +
  ggplot2::theme(
    strip.background = ggplot2::element_blank(),
    # strip.background = ggplot2::element_rect(color = "gray40"),
    strip.placement = "outside",
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom"
  )

# plots$emissions$rsd_yearmodel$NOx <- ggiraph::girafe(ggobj = plots$emissions$rsd_yearmodel$NOx, width_svg = 6, height_svg = 4)


# read & plot RSD NOx emission time series (year of measurement) by fuel type
data_rsd_per_yearmeas <- airquality.methods::read_local_csv(ressources_plotting$emissions$rsd_yearmeas)

plots$emissions$rsd_yearmeas$NOx <-
  data_rsd_per_yearmeas |> 
  dplyr::mutate(vehicle_fuel_type = dplyr::recode_factor(vehicle_fuel_type, !!!c("all" = "Benzin & Diesel", "gasoline" = "Benzin", "diesel" = "Diesel"))) |> 
  ggplot2::ggplot(aes(x = year, y = emission)) +
  ggplot2::geom_smooth(mapping = aes(color = vehicle_fuel_type), se = TRUE, span = 0.6, level = 0.95) +
  # ggiraph::geom_smooth_interactive(mapping = aes(color = vehicle_fuel_type, data_id = vehicle_fuel_type, tooltip = round_off(nox_emission, 1)), se = TRUE, span = 0.6, level = 0.95) +
  # ggplot2::geom_point(color = "gray60") +
  ggplot2::scale_x_continuous(expand = c(0.01,0.01)) +
  ggplot2::scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01), labels = function(x) format(x, big.mark = "'")) +
  ggplot2::scale_color_manual(name = "Treibstoff:", values = c("Benzin & Diesel" = "gray40", "Benzin" = "gold3", "Diesel" = "red3")) +
  ggplot2::ggtitle(
    label = openair::quickText("Abgasmessungen von Stickoxiden im realen Fahrbetrieb"),
    subtitle = openair::quickText("NOx Emissionen, Trend der Mittelwerte (g/kg Treibstoff)") # ... "Trend der Mittelwerte Personenwagen und leichte Nutzfahrzeuge", but too long for subtitle
  ) +
  ggplot2::labs(caption = "Daten: Kanton Zürich/AWEL") +
  theme_ts +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom"
  )

# plots$emissions$rsd_yearmeas$NO <- ggiraph::girafe(ggobj = plots$emissions$rsd_yearmeas$NO, width_svg = 6, height_svg = 4) 


# collect plots in a tibble for use in *.qmd
# ---
plots_emissions <-
  dplyr::bind_rows(
    plotlist_to_tibble(plots$emissions$inventory_absolute, "emission", "inventory_absolute"),
    plotlist_to_tibble(plots$emissions$inventory_relative, "emission", "inventory_relative"),
    plotlist_to_tibble(plots$emissions$rsd_norm, "emission", "rsd_norm"),
    plotlist_to_tibble(plots$emissions$rsd_yearmodel, "emission", "rsd_yearmodel"),
    plotlist_to_tibble(plots$emissions$rsd_yearmeas, "emission", "rsd_yearmeas")
  )
