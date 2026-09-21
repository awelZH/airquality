# Plots of the emissions page: emission inventory per sector and subsector, RSD NOx emissions.


#' Plot emission inventory time series as stacked bars, with sectors as legend blocks
#'
#' Every subsector (`subsector_new`) is one colour (`col`); the legend shows one block per sector
#' with the sector as title and its subsectors below ([airquality.methods::add_grouped_legend()]).
#' Order of the blocks and of the subsectors within a block follows `order`.
#'
#' @param data Emission data of one pollutant (`data_emissions.csv`): `year`, `pollutant`,
#'   `metric`, `unit`, `sector`, `subsector_new`, `order`, `col`, `emission`.
#' @param relative Plot shares instead of absolute emissions (use with `pos = "fill"`).
#' @param pos Position of the bars, e.g. `"stack"` or `"fill"`.
#' @param width Width of the bars.
#' @param theme ggplot2 theme.
#' @param sectors_last Sectors moved to the end of the stack and the legend (e.g. to show
#'   agriculture for NH3 at the bottom of the legend).
#'
#' @return A ggplot object.
#'
#' @keywords internal
ggplot_emissions <- function(data, relative = FALSE, pos = "stack", width = 0.8, theme = ggplot2::theme_minimal(),
                             sectors_last = NULL) {

  pollutant <- unique(as.character(data$pollutant))
  metric <- unique(as.character(data$metric))
  unit <- unique(as.character(data$unit))

  if (relative) {
    yscale <- ggplot2::scale_y_continuous(labels = scales::percent_format(), expand = c(0,0))
    sub <- openair::quickText(paste0(pollutant, ", ", metric, " nach Quellgruppen (relativ)"))
  } else {
    yscale <- ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = "'"), expand = c(0,0))
    sub <- openair::quickText(paste0(pollutant, ", ", metric, " nach Quellgruppen (", unit, ")"))
  }

  sectors <- unique(data$sector[order(data$order)])
  group_order <- c(setdiff(sectors, sectors_last), intersect(sectors_last, sectors))
  data <- dplyr::mutate(data, key = airquality.methods::grouped_key(sector, subsector_new, order, group_order = group_order))
  colours <- dplyr::distinct(data, key, col)

  plot <-
    data |>
    ggplot2::ggplot(ggplot2::aes(x = year, y = emission, fill = key)) +
    ggplot2::geom_bar(stat = "identity", position = pos, width = width) +
    # ggiraph::geom_bar_interactive(mapping = ggplot2::aes(data_id = subsector_new, tooltip = round_off(emission, 1)), stat = "identity", position = pos, width = width) +
    yscale +
    ggplot2::scale_fill_manual(values = rlang::set_names(colours$col, colours$key)) +
    theme +
    ggplot2::theme(legend.title = ggplot2::element_blank()) +
    ggplot2::ggtitle(
      label = openair::quickText(paste0("Luftschadstoff-Emissionen ", airquality.methods::longpollutant(pollutant))),
      subtitle = sub
    ) +
    ggplot2::labs(caption = "Daten: Ostluft, Grundlage: EMIS Schweiz")

  # plot <- ggiraph::girafe(ggobj = plot, width_svg = 6, height_svg = 3)

  airquality.methods::add_grouped_legend(plot)
}




#' Translate the vehicle types and fuel types of the RSD outputs into German factor labels
#'
#' @param data RSD data with `vehicle_type` and `vehicle_fuel_type`.
#'
#' @return `data` with both columns as factors (passenger cars and gasoline first).
#'
#' @keywords internal
translate_rsd_vehicles <- function(data) {
  dplyr::mutate(
    data,
    vehicle_type = dplyr::recode_factor(vehicle_type, !!!c("passenger car" = "Personenwagen", "light duty vehicle" = "leichte Nutzfahrzeuge")),
    vehicle_fuel_type = dplyr::recode_factor(vehicle_fuel_type, !!!c("gasoline" = "Benzin", "diesel" = "Diesel"))
  )
}


#' Plot the RSD NOx emissions per vehicle type, fuel type and Euro norm
#'
#' Euro 6c is left out (few measured vehicles, not relevant for the fleet); combinations without data
#' are kept as empty bars. The red segments are the emission limits.
#'
#' @param data `data_nox_vehicle_emissions_rsd_per_norm.csv`.
#' @param theme ggplot2 theme.
#'
#' @return A ggplot object.
#'
#' @keywords internal
plot_rsd_per_norm <- function(data, theme = ggplot2::theme_minimal()) {

  data |>
    dplyr::filter(vehicle_euronorm != "Euro6c") |> # only few measured vehicles and not relevant for vehicle fleet...
    tidyr::expand(vehicle_type, vehicle_fuel_type, vehicle_euronorm) |>
    dplyr::left_join(data, by = dplyr::join_by(vehicle_type, vehicle_fuel_type, vehicle_euronorm)) |>
    translate_rsd_vehicles() |>
    dplyr::mutate(vehicle_euronorm = factor(vehicle_euronorm)) |>
    ggplot2::ggplot(ggplot2::aes(x = vehicle_euronorm, y = emission, group = vehicle_type, fill = vehicle_type)) +
    ggplot2::geom_bar(stat = "identity", width = 0.75, position = ggplot2::position_dodge()) +
    ggplot2::geom_linerange(mapping = ggplot2::aes(ymin = emission - standarderror, ymax = emission + standarderror), color = "gray60", position = ggplot2::position_dodge(width = 0.75)) +
    ggplot2::geom_segment(mapping = ggplot2::aes(x = as.numeric(vehicle_euronorm) - 0.45, xend = as.numeric(vehicle_euronorm) + 0.45, y = nox_emission_threshold_g_per_kg_fuel, yend = nox_emission_threshold_g_per_kg_fuel), color = "red3", linewidth = 1) +
    ggplot2::facet_wrap(vehicle_fuel_type~., strip.position = "top") +
    ggplot2::scale_y_continuous(limits = c(0,25), breaks = seq(0,25,5), expand = c(0.01,0.01), labels = function(x) format(x, big.mark = "'")) +
    ggplot2::scale_fill_manual(name = "Fahrzeugkategorie:", values = c("Personenwagen" = "cadetblue3", "leichte Nutzfahrzeuge" = "darkslategray")) +
    ggplot2::ggtitle(
      label = openair::quickText("Abgasmessungen von Stickoxiden im realen Fahrbetrieb"),
      subtitle = openair::quickText("NOx Emissionen, Mittelwert pro Abgasnorm (g/kg Treibstoff)")
    ) +
    ggplot2::labs(caption = "Daten: Kanton Zürich/AWEL") +
    theme +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.placement = "outside",
      legend.title = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}


#' Plot the RSD NOx emissions per vehicle model year, vehicle type and fuel type
#'
#' Model years without data are kept as empty bars. The red steps are the emission limits.
#'
#' @param data `data_nox_emissions_rsd_per_yearmodel.csv`.
#' @param theme ggplot2 theme.
#'
#' @return A ggplot object.
#'
#' @keywords internal
plot_rsd_per_yearmodel <- function(data, theme = ggplot2::theme_minimal()) {

  data |>
    tidyr::expand(vehicle_type, vehicle_fuel_type, vehicle_model_year) |>
    dplyr::left_join(data, by = dplyr::join_by(vehicle_type, vehicle_fuel_type, vehicle_model_year)) |>
    translate_rsd_vehicles() |>
    ggplot2::ggplot(ggplot2::aes(x = vehicle_model_year, y = emission, group = vehicle_type, fill = vehicle_type)) +
    ggplot2::geom_bar(stat = "identity", width = 0.75, position = ggplot2::position_dodge()) +
    ggplot2::geom_linerange(mapping = ggplot2::aes(ymin = emission - standarderror, ymax = emission + standarderror), color = "gray60", position = ggplot2::position_dodge(width = 0.75)) +
    ggplot2::geom_step(mapping = ggplot2::aes(x = vehicle_model_year + 0.475, y = nox_emission_threshold_g_per_kg_fuel), color = "red3", linewidth = 1) +
    ggplot2::facet_wrap(vehicle_fuel_type~., strip.position = "top") +
    ggplot2::scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01), labels = function(x) format(x, big.mark = "'")) +
    ggplot2::scale_fill_manual(name = "Fahrzeugkategorie:", values = c("Personenwagen" = "cadetblue3", "leichte Nutzfahrzeuge" = "darkslategray")) +
    ggplot2::ggtitle(
      label = openair::quickText("Abgasmessungen von Stickoxiden im realen Fahrbetrieb"),
      subtitle = openair::quickText("NOx Emissionen, Mittelwert pro Fahrzeug-Modelljahr (g/kg Treibstoff)")
    ) +
    ggplot2::labs(caption = "Daten: Kanton Zürich/AWEL") +
    theme +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.placement = "outside",
      legend.title = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}


#' Plot the trend of the RSD NOx emissions per year of measurement and fuel type
#'
#' @param data `data_nox_emissions_rsd_per_yearmeas.csv`.
#' @param theme ggplot2 theme.
#'
#' @return A ggplot object (loess smooth with 95 % confidence band).
#'
#' @keywords internal
plot_rsd_per_yearmeas <- function(data, theme = ggplot2::theme_minimal()) {

  data |>
    dplyr::mutate(vehicle_fuel_type = dplyr::recode_factor(vehicle_fuel_type, !!!c("all" = "Benzin & Diesel", "gasoline" = "Benzin", "diesel" = "Diesel"))) |>
    ggplot2::ggplot(ggplot2::aes(x = year, y = emission)) +
    ggplot2::geom_smooth(mapping = ggplot2::aes(color = vehicle_fuel_type), se = TRUE, span = 0.6, level = 0.95) +
    ggplot2::scale_x_continuous(expand = c(0.01,0.01)) +
    ggplot2::scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01), labels = function(x) format(x, big.mark = "'")) +
    ggplot2::scale_color_manual(name = "Treibstoff:", values = c("Benzin & Diesel" = "gray40", "Benzin" = "gold3", "Diesel" = "red3")) +
    ggplot2::ggtitle(
      label = openair::quickText("Abgasmessungen von Stickoxiden im realen Fahrbetrieb"),
      subtitle = openair::quickText("NOx Emissionen, Trend der Mittelwerte (g/kg Treibstoff)") # ... "Trend der Mittelwerte Personenwagen und leichte Nutzfahrzeuge", but too long for subtitle
    ) +
    ggplot2::labs(caption = "Daten: Kanton Zürich/AWEL") +
    theme +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}
