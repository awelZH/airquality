# Plots of the trends page: relative development of emissions and immissions vs. a reference year.
# The trend data themselves come from scripts/_compile_trend_data.R (work in progress).


# ---- data ----------------------------------------------------------------------------

#' Relative emissions of the canton per pollutant and year, vs. a reference year
#'
#' Sums the emissions over all subsectors (years without any value count as 0) and divides them by
#' the emission of the reference year ([prepare_emission_trends()]).
#'
#' @param data `data_emissions.csv`.
#' @param reference_year Reference year (value 1).
#'
#' @return Tibble with `year`, `pollutant` (German name), `type`, `reference_year`, `class`
#'   ("relative Emission"), `value` and `site` ("Kanton Zürich").
#'
#' @keywords internal
emission_trends_relative <- function(data, reference_year) {
  data |>
    airquality.methods::aggregate_groups(y = "emission", groups = c("year", "pollutant"), nmin = 1) |>
    dplyr::select(year, pollutant, emission = sum) |>
    dplyr::mutate(emission = ifelse(is.na(emission), 0, emission)) |>
    prepare_emission_trends(reference_year_fun = function(x) reference_year) |>
    tidyr::pivot_longer(`relative Emission`, names_to = "class", values_to = "value") |>
    dplyr::mutate(site = "Kanton Zürich")
}


#' Label the types of the aggregated trends as in the legend
#'
#' @param data `data_airquality_trends_relative_aggregated_y1.csv`.
#'
#' @return `data` with `type` "Median Trend", "Median Messwerte" or "Emission".
#'
#' @keywords internal
recode_trend_types <- function(data) {
  dplyr::mutate(data, type = dplyr::recode(type, Trend = "Median Trend", Messwerte = "Median Messwerte", emission = "Emission"))
}


#' Add the metric to the name of ozone, which has two parameters
#'
#' @param data Trend data with `pollutant` and `parameter`.
#'
#' @return `data` with `pollutant` "Ozon, <metric>" for ozone.
#'
#' @keywords internal
label_ozone_metric <- function(data) {
  dplyr::mutate(data, pollutant = dplyr::case_when(pollutant == "Ozon" ~ paste0(pollutant, ", ", airquality.methods::longmetric(parameter)), TRUE ~ pollutant))
}


#' Trend data of the overview: emissions, median trend and median of the measurements
#'
#' @param data_agg `data_airquality_trends_relative_aggregated_y1.csv`.
#'
#' @return `data_agg` filtered and labelled, `type` as factor.
#'
#' @keywords internal
trend_data_overview <- function(data_agg) {
  data_agg |>
    recode_trend_types() |>
    dplyr::filter(type %in% c("Emission", "Median Trend", "Median Messwerte")) |>
    label_ozone_metric() |>
    dplyr::mutate(type = factor(type, levels = c("Emission", "Median Trend", "Median Messwerte")))
}


#' Trend data of the detailed plot: emissions and the relative trend per site
#'
#' @param data_agg `data_airquality_trends_relative_aggregated_y1.csv`.
#' @param data_trends `data_airquality_trends_relative_y1.csv`.
#' @param year_max Last year of the trends per site.
#'
#' @return Tibble of the emissions and the trends per site, labelled, `type` as factor.
#'
#' @keywords internal
trend_data_detailed <- function(data_agg, data_trends, year_max) {
  data_agg |>
    recode_trend_types() |>
    dplyr::filter(type == "Emission") |>
    dplyr::bind_rows(dplyr::filter(data_trends, type == "Trend" & class == "relative Immission" & year <= year_max)) |>
    label_ozone_metric() |>
    dplyr::mutate(
      type = dplyr::recode(type, Trend = "Trend pro Standort"),
      type = factor(type, levels = c("Emission", "Trend pro Standort", "Median Messwerte"))
    )
}


# ---- plots ---------------------------------------------------------------------------

#' Plot the relative emissions of all pollutants vs. the reference year
#'
#' @param data Data as returned by [emission_trends_relative()].
#' @param reference_year Reference year (subtitle).
#' @param linewidth Width of the lines.
#' @param theme ggplot2 theme.
#'
#' @return A ggplot object.
#'
#' @keywords internal
plot_emission_trends_relative <- function(data, reference_year, linewidth = 1, theme = ggplot2::theme_minimal()) {
  data |>
    ggplot2::ggplot(ggplot2::aes(x = year, y = value - 1, color = pollutant)) +
    ggplot2::geom_hline(yintercept = 0, color = "gray80", linetype = 2) +
    ggplot2::geom_vline(data = \(d) dplyr::distinct(d, pollutant, reference_year), mapping = ggplot2::aes(xintercept = reference_year), color = "gray80", linetype = 2) +
    ggplot2::geom_line(linewidth = linewidth) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(), expand = c(0.02,0.02)) +
    ggplot2::scale_color_manual(name = "Schadstoff", values = c(colorspace::sequential_hcl(palette = "Mako", n = 4), colorspace::sequential_hcl(palette = "ag_sunset", n = 4))) +
    theme +
    ggplot2::theme(legend.title = ggplot2::element_blank()) +
    ggplot2::ggtitle(
      label = "Relative Entwicklung Emissionen im Kanton Zürich",
      subtitle = paste0("Veränderung gegenüber dem Jahr ", reference_year)) +
    ggplot2::labs(caption = "Daten: Ostluft, Grundlage: EMIS Schweiz")
}


#' Plot relative trends of emissions and immissions vs. a reference year, one panel per pollutant
#'
#' @param data_trends Data as returned by [trend_data_overview()] or [trend_data_detailed()].
#' @param detailed Show the trend per site (points and thin lines) instead of the medians.
#' @param pt_size,linewdth Size of the points and width of the lines.
#' @param facet_ncol,facet_scale Columns and scales of the panels.
#' @param theme ggplot2 theme.
#' @param titlelab,captionlab Title and caption.
#'
#' @return A ggplot object.
#'
#' @keywords internal
plot_timeseries_trend_relative <- function(data_trends, detailed = FALSE,
                                           pt_size = 1.5, linewdth = 1, facet_ncol = NULL, facet_scale = "free_y", theme = ggplot2::theme_minimal(),
                                           titlelab = NULL, captionlab = NULL
) {

  plot <-
    data_trends |>
    ggplot2::ggplot(ggplot2::aes(x = year, y = value - 1, color = type)) +
    ggplot2::geom_hline(yintercept = 0, color = "gray80", linetype = 2) +
    ggplot2::geom_vline(data = \(d) dplyr::distinct(d, pollutant, reference_year), mapping = ggplot2::aes(xintercept = reference_year), color = "gray80", linetype = 2)

  if (detailed) {

    plot <-
      plot +
      ggplot2::geom_point(data = \(d) dplyr::filter(d, type %in% c("Trend pro Standort")), mapping = ggplot2::aes(size = type, shape = type), fill = "white") +
      ggplot2::geom_line(data = \(d) dplyr::filter(d, type %in% c("Median Messwerte", "Trend pro Standort", "Median Trend", "Emission")), mapping = ggplot2::aes(linewidth = type, group = site))

  } else {

    plot <-
      plot +
      ggplot2::geom_line(mapping = ggplot2::aes(linewidth = type))

  }

  plot  +
    ggplot2::scale_y_continuous(labels = scales::percent_format(), expand = c(0.02,0.02)) +
    ggplot2::scale_color_manual(name = "Grundlage", values = c("Emission" = "gray50", "Median Trend" = "dodgerblue", "Median Messwerte" = "gold3", "Trend pro Standort" = "gray80")) +
    ggplot2::scale_shape_manual(values = c("Median Messwerte" = 21, "Trend pro Standort" = 19)) +
    ggplot2::scale_size_manual(values = c("Median Messwerte" = pt_size, "Trend pro Standort" = pt_size * 0.75)) +
    ggplot2::scale_linewidth_manual(values = c("Emission" = linewdth, "Median Trend" = linewdth, "Median Messwerte" = linewdth * 0.5, "Trend pro Standort" = linewdth * 0.5)) +
    ggplot2::guides(shape = "none", size = "none", linewidth = "none") +
    ggplot2::facet_wrap(pollutant~., axes = "all", ncol = facet_ncol, scales = facet_scale) +
    theme +
    ggplot2::theme(
      strip.text.x = ggplot2::element_text(hjust = 0),
      legend.title = ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    titlelab +
    captionlab
}


#' Split a trend plot into one plot per pollutant, with the legend on the right
#'
#' @param plot A plot of [plot_timeseries_trend_relative()] with all pollutants as panels.
#'
#' @return Named list of ggplot objects, one per pollutant (German name, as in the panels).
#'
#' @keywords internal
plot_trends_per_pollutant <- function(plot) {
  purrr::map(rlang::set_names(unique(plot$data$pollutant)), \(pollutant) {
    plot + dplyr::filter(plot$data, pollutant == !!pollutant) +
      ggplot2::theme(legend.position = "right")
  })
}
