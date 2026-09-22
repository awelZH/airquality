# Plots of the air quality page: monitoring time series per site class, comparison with thresholds,
# nitrogen deposition at the monitoring sites.


# ---- preparing the monitoring data ----------------------------------------------------

#' Prepare the air quality monitoring data for the plots
#'
#' @param data `data_airquality_monitoring_y1.csv`.
#' @param years,parameters Years and parameters to keep.
#' @param siteclass_levels Site classes to keep, in the order of the legend; sites of other classes
#'   (e.g. rural traffic sites) and without a class are dropped.
#'
#' @return `data` filtered, with `siteclass` as factor.
#'
#' @keywords internal
prepare_plot_airquality <- function(data, years, parameters, siteclass_levels) {
  data |>
    dplyr::mutate(siteclass = factor(siteclass, levels = siteclass_levels)) |>
    dplyr::filter(year %in% years, parameter %in% parameters, !is.na(siteclass))
}


#' Keep the nitrogen deposition of sensitive ecosystems with a critical load
#'
#' Drops rows without ecosystem, the settlement site (not a sensitive ecosystem) and rows without critical
#' load (`cln`).
#'
#' @param data `data_ndep_monitoring_y1.csv` or `data_ndep_pars_monitoring_y1.csv`.
#'
#' @return `data` filtered.
#'
#' @keywords internal
prepare_plot_ndep <- function(data) {
  dplyr::filter(data, !is.na(ecosys), ecosys != "Siedlungen", !is.na(cln))
}


#' Sum the nitrogen deposition parameters per source category
#'
#' @param data `data_ndep_pars_monitoring_y1.csv`.
#'
#' @return Tibble with `year`, `site`, `ecosys`, `cln`, `pollutant`, `unit`, `component` (source category,
#'   factor with NOx sources first) and `deposition`; `ecosys` as factor.
#'
#' @keywords internal
prepare_plot_ndep_components <- function(data) {
  data |>
    prepare_plot_ndep() |>
    dplyr::summarise(deposition = sum(deposition), .by = c(year, site, ecosys, cln, pollutant, unit, source_cat)) |>
    dplyr::rename(component = source_cat) |>
    dplyr::mutate(
      component = factor(component, levels = rev(c("aus NH3-Quellen", "aus NOx-Quellen"))),
      ecosys = factor(ecosys, levels = rev(c("Hochmoor", "Flachmoor", "Trockenrasen", "Wald")))
    )
}


# ---- thresholds -------------------------------------------------------------------------

#' Threshold lines of a parameter in the monitoring time series
#'
#' Annual means are compared with the LRV limit and the WHO guideline, the monthly O3 peak with the LRV
#' limit and the O3 peak season with the WHO guideline; eBC has no threshold.
#'
#' @param parameter Parameter, e.g. "NO2".
#' @param threshold_values Threshold values with line styles (`immission_threshold_values` of
#'   `scripts/_plot_setup.R`).
#'
#' @return Threshold lines as returned by [extract_threshold()] (`value = NA` for none). Stops with an
#'   error of class `airquality_plot_error` for other parameters.
#'
#' @keywords internal
timeseries_threshold <- function(parameter, threshold_values) {
  switch(parameter,
         NO2 = extract_threshold(threshold_values, pollutant = "NO2"),
         PM10 = extract_threshold(threshold_values, pollutant = "PM10"),
         PM2.5 = extract_threshold(threshold_values, pollutant = "PM2.5"),
         eBC = list(value = NA),
         `O3_max_98p_m1` = extract_threshold(threshold_values, pollutant = "O3", metric = "typische Spitzenbelastung", source = "LRV Grenzwert"),
         O3_peakseason_mean_d1_max_mean_h8gl = extract_threshold(threshold_values, pollutant = "O3", metric = "mittlere Sommertagbelastung", source = "WHO Richtwert"),
         cli::cli_abort("No threshold rule for parameter {.val {parameter}} in the monitoring time series.", class = "airquality_plot_error")
  )
}


#' Add the LRV and WHO threshold values to air quality data
#'
#' @param data Air quality data with `pollutant` and `metric`.
#' @param threshold_values Threshold values (`metric_description` is matched with `metric`).
#'
#' @return `data` with the columns `LRV Grenzwert` and `WHO Richtwert` (NA without threshold).
#'
#' @keywords internal
combine_thresholds <- function(data, threshold_values) {
  # rows come out in the order of the sorted thresholds (right join), the order the jittered points of the
  # threshold comparison are drawn in
  threshold_values |>
    dplyr::select(source, pollutant, metric = metric_description, interval, threshold) |>
    tidyr::pivot_wider(names_from = source, values_from = threshold) |>
    dplyr::arrange(pollutant, metric, interval) |>
    dplyr::right_join(data, by = dplyr::join_by(pollutant, metric)) |>
    dplyr::select(year, site, pollutant, metric, parameter, interval, unit, concentration, siteclass, `LRV Grenzwert`, `WHO Richtwert`, source)
}


#' Relate air quality and nitrogen deposition to their thresholds
#'
#' Concentrations are divided by the LRV limit and the WHO guideline, nitrogen deposition by the critical
#' load of the ecosystem (site class "empf. Ökosystem").
#'
#' @param data_airquality Air quality data as prepared by [prepare_plot_airquality()].
#' @param data_ndep Nitrogen deposition as prepared by [prepare_plot_ndep()].
#' @param threshold_values Threshold values.
#' @param years Years to keep.
#'
#' @return Tibble with `year`, `pollutant`, `metric`, `siteclass`, `reference` (German label of the
#'   threshold), `value` (relative) and `x` (label of the axis, factor).
#'
#' @keywords internal
threshold_comparison_data <- function(data_airquality, data_ndep, threshold_values, years) {

  ndep <-
    data_ndep |>
    dplyr::filter(year %in% years) |>
    dplyr::mutate(
      value = deposition / cln,
      reference = "value_relative_lrv",
      siteclass = "empf. Ökosystem"
    ) |>
    dplyr::select(year, pollutant, metric, value, reference, siteclass)

  data_airquality |>
    combine_thresholds(threshold_values) |>
    dplyr::mutate(
      value_relative_lrv = concentration / `LRV Grenzwert`,
      value_relative_who = concentration / `WHO Richtwert`
    ) |>
    dplyr::filter(year %in% years) |>
    dplyr::select(year, pollutant, metric, value_relative_lrv, value_relative_who, siteclass) |>
    tidyr::pivot_longer(c(value_relative_lrv, value_relative_who), names_to = "reference", values_to = "value", cols_vary = "slowest") |>
    dplyr::filter(!is.na(value)) |>
    dplyr::bind_rows(ndep) |>
    dplyr::mutate(
      pollutant = dplyr::recode_factor(pollutant, "Ndep" = "Stickstoffeintrag in empf. Ökosysteme"),
      metric = dplyr::recode_factor(metric, "Jahressumme" = ""),
      x = paste0(pollutant, " ", metric),
      x = factor(x, levels = rev(sort(unique(x)))),
      reference = dplyr::recode(reference, !!!c("value_relative_lrv" = "relativ zu Immissionsgrenzwerten bzw. kritischen Eintragsraten:",
                                                "value_relative_who" = "relativ zu Richtwerten der Weltgesundheitsorganisation:"))
    )
}


# ---- plots -------------------------------------------------------------------------------

#' Plot yearly values per site as points, optionally with threshold lines
#'
#' @param data Data with the columns used in `mapping`.
#' @param mapping Aesthetic mapping.
#' @param ylims,ybreaks Limits and breaks of the y axis.
#' @param titlelab,captionlab Title and caption.
#' @param pointshape,pointsize Shape and size of the points.
#' @param threshold Threshold lines as returned by [extract_threshold()]; `value = NA` for none.
#' @param theme ggplot2 theme.
#'
#' @return A ggplot object.
#'
#' @keywords internal
ggplot_timeseries <- function(data, mapping = ggplot2::aes(x = year, y = concentration, color = siteclass), ylims = c(0,NA), ybreaks = ggplot2::waiver(), titlelab = NULL, captionlab = NULL, pointshape = 19, pointsize = 2,
                              threshold = list(value = NA, color = "gray30", label = NULL, labelsize = 4, linetype = 2, linesize = 1),
                              theme = ggplot2::theme_minimal()) {

  plot <-
    ggplot2::ggplot(data, mapping = mapping) +
    ggplot2::geom_point(size = pointsize, shape = pointshape) +
    ggplot2::scale_x_continuous(expand = c(0.01,0.01)) +
    ggplot2::scale_y_continuous(limits = ylims, breaks = ybreaks, expand = c(0.01,0.01)) +
    titlelab +
    captionlab +
    theme

  if (!is.na(sum(threshold$value))){
    text <- tibble::tibble(x = rep(min(data$year), length(threshold$value)), y = threshold$value, label = threshold$labels)
    plot <-
      plot +
      ggplot2::geom_hline(yintercept = threshold$value, color = threshold$color, linetype = threshold$linetype, linewidth = threshold$linesize) +
      ggplot2::geom_text(data = text, mapping = ggplot2::aes(x = x, y = y, label = label), size = threshold$labelsize,
                         hjust = 0, vjust = 0, nudge_y = pmax(0, 0.01 * max(ylims), na.rm = TRUE), inherit.aes = FALSE)
  }

  return(plot)
}


#' Plot the yearly monitoring values per site class, one plot per parameter
#'
#' @param data Air quality data as prepared by [prepare_plot_airquality()].
#' @param parameters Parameters to plot.
#' @param axes Y limits and breaks per parameter (`plot_axes_timeseries` of `scripts/_plot_setup.R`).
#' @param threshold_values Threshold values with line styles.
#' @param colour_scale Colour scale of the site classes.
#' @param pointsize Size of the points.
#' @param theme ggplot2 theme.
#' @param caption Caption.
#'
#' @return Named list of ggplot objects, one per parameter.
#'
#' @keywords internal
plot_monitoring_timeseries <- function(data, parameters, axes, threshold_values, colour_scale = NULL, pointsize = 2,
                                            theme = ggplot2::theme_minimal(),
                                            caption = "Datenabdeckung: Kanton Zürich, Daten: Ostluft & NABEL (BAFU & Empa)") {

  purrr::map(rlang::set_names(parameters), function(parameter) {

    data_plot <- dplyr::filter(data, parameter == !!parameter)
    pollutant <- unique(data_plot$pollutant)
    unit <- unique(data_plot$unit)
    metric <- unique(data_plot$metric)
    axis <- parameter_setting(axes, parameter)

    ggplot_timeseries(data_plot,
                      ylims = axis$ylim, ybreaks = axis$ybreaks,
                      titlelab = ggplot2::ggtitle(
                        label = openair::quickText(paste0("Luftqualitätsmesswerte ", airquality.methods::longpollutant(pollutant))),
                        subtitle = openair::quickText(paste0(pollutant, ", ", metric," (", unit, ")"))
                      ),
                      captionlab = ggplot2::labs(caption = caption),
                      pointsize = pointsize, theme = theme, threshold = timeseries_threshold(parameter, threshold_values)
    ) +
      colour_scale
  })
}


#' Plot monitoring values relative to their thresholds
#'
#' Every category of the panel is drawn, also without values in these years, and the panels reach at
#' least up to `limits`, so the plots of the single windows stay comparable.
#'
#' @param data Data as returned by [threshold_comparison_data()], filtered to the years shown.
#' @param threshold_styles Line styles of the thresholds (`col`, `lty`, `lsz`).
#' @param years Years shown (subtitle).
#' @param limits Maximum per panel, as returned by [threshold_comparison_limits()].
#' @param categories Categories per panel (`reference`, `x`), as returned by [threshold_comparison_categories()].
#' @param colour_scale Colour scale of the site classes.
#' @param pointsize Size of the points.
#' @param jitter_seed Seed of the jittered points, so the figure stays the same between runs.
#' @param theme ggplot2 theme.
#'
#' @return A ggplot object.
#'
#' @keywords internal
plot_threshold_comparison <- function(data, threshold_styles, years, limits, categories, colour_scale = NULL, pointsize = 2, jitter_seed = 1,
                                      theme = ggplot2::theme_minimal()) {

  # an invisible point per panel at its maximum, so all windows of the time series share the same scale
  limits <- dplyr::left_join(limits, dplyr::slice(categories, 1, .by = reference), by = dplyr::join_by(reference))

  data |>
    ggplot2::ggplot(ggplot2::aes(x = x, y = value, color = siteclass)) +
    ggplot2::geom_hline(yintercept = 1, linetype = threshold_styles$lty, color = threshold_styles$col, linewidth = threshold_styles$lsz, show.legend = FALSE) +
    # the categories of the panel and its maximum, without values: they train the axes, they draw nothing.
    # The categories come before the points, so the axis keeps their order also in years without values.
    ggplot2::geom_blank(data = categories, mapping = ggplot2::aes(x = x), inherit.aes = FALSE) +
    ggplot2::geom_blank(data = limits, mapping = ggplot2::aes(x = x, y = value), inherit.aes = FALSE) +
    ggplot2::geom_point(shape = 21, size = pointsize, position = ggplot2::position_jitter(width = 0.2, height = 0, seed = jitter_seed)) +
    ggplot2::facet_wrap(reference~., scales = "free_y", ncol = 1, axes = "all_x") +
    ggplot2::scale_y_continuous(breaks = seq(0,10,1), limits = c(0,NA), labels = scales::percent_format(), expand = c(0.01,0.01)) +
    ggplot2::coord_flip() +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 3)) +
    ggplot2::ggtitle(
      label = openair::quickText("Luftqualitätsmesswerte Referenzwertvergleich"),
      subtitle = paste0("Jahre ", min(years), " bis ", max(years))
    ) +
    ggplot2::labs(caption = "Daten: Ostluft & NABEL (BAFU & Empa)") +
    theme +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.major.x = ggplot2::element_line(),
      strip.text = ggplot2::element_text(hjust = 0)
    ) +
    colour_scale
}


#' Plot the yearly nitrogen deposition as bars per source category, with the critical load
#'
#' @param data Nitrogen deposition as prepared by [prepare_plot_ndep_components()].
#' @param xlim,xbreaks Limits and breaks of the x axis.
#' @param linewidth,colour Width and colour of the critical load line.
#' @param title Title.
#' @param theme ggplot2 theme.
#'
#' @return A ggplot object.
#'
#' @keywords internal
plot_ndep_bars <- function(data, xlim = NULL, xbreaks = ggplot2::waiver(), linewidth = 1, colour = "red3",
                                      title = "Luftqualitätsmesswerte - Stickstoffeintrag in empfindliche Ökosysteme",
                                      theme = ggplot2::theme_minimal()) {

  cln <- dplyr::distinct(data, site, ecosys, cln)

  plot <-
    data |>
    ggplot2::ggplot(ggplot2::aes(x = year, y = deposition, fill = component)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_hline(data = cln, mapping = ggplot2::aes(yintercept = cln, group = site), color = colour, linewidth = linewidth, show.legend = FALSE) +
    ggplot2::scale_x_continuous(limits = xlim, breaks = xbreaks, expand = c(0.01,0.01)) +
    ggplot2::scale_y_continuous(expand = c(0.01,0.01)) +
    ggplot2::scale_fill_manual(values = c("aus NH3-Quellen" = "#2A5676", "aus NOx-Quellen" = "#B696D6")) +
    theme +
    ggplot2::theme(
      strip.text = ggplot2::element_text(hjust = 0),
      legend.title = ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    ggplot2::ggtitle(
      label = openair::quickText(title),
      subtitle = expression("Stickstoffeintrag (kg-N " * ha^-1 * Jahr^-1 * ")")
    )

  return(plot)
}


#' Plot the yearly nitrogen deposition of all sites since 2019, per ecosystem
#'
#' @param data Nitrogen deposition as prepared by [prepare_plot_ndep()].
#' @param colour_scale,fill_scale Colour and fill scales of the ecosystems.
#' @param shape_scale Shape scale of the estimated share (`estimated_class`).
#' @param pointsize Size of the points (drawn 1.5 times larger).
#' @param jitter_seed Seed of the jittered points.
#' @param theme ggplot2 theme.
#'
#' @return A ggplot object.
#'
#' @keywords internal
plot_ndep_sites <- function(data, colour_scale = NULL, fill_scale = NULL, shape_scale = NULL, pointsize = 2, jitter_seed = 1,
                            theme = ggplot2::theme_minimal()) {

  data |>
    dplyr::filter(year >= 2019) |>
    ggplot2::ggplot(ggplot2::aes(x = year, y = deposition, color = ecosys, fill = ecosys, shape = estimated_class)) +
    ggplot2::geom_point(size = pointsize * 1.5, position = ggplot2::position_jitter(width = 0.1, height = 0, seed = jitter_seed)) +
    ggplot2::scale_x_continuous(limits = c(2019,NA), expand = c(0.01,0.01)) +
    ggplot2::scale_y_continuous(limits = c(0,NA), expand = ggplot2::expansion(mult = c(0, 0.02))) +
    colour_scale +
    fill_scale +
    shape_scale +
    ggplot2::ggtitle(
      label = openair::quickText("Luftqualitätsmesswerte - Stickstoffeintrag in empfindliche Ökosysteme seit 2019"),
      subtitle = expression("Stickstoffeintrag (kg-N " * ha^-1 * Jahr^-1 * ")")
    ) +
    ggplot2::labs(caption = "geschätzt: je nach Messprogramm versch. Anteile statistisch geschätzt (NH3 immer gemessen)\nQuelle: Ostluft") +
    theme
}


#' Plot the yearly nitrogen deposition of all sites since 2019 relative to the critical load
#'
#' @inheritParams plot_ndep_sites
#' @param linewidth,colour Width and colour of the line at 100 %.
#'
#' @return A ggplot object.
#'
#' @keywords internal
plot_ndep_sites_vs_cln <- function(data, colour_scale = NULL, linewidth = 1, colour = "red3", pointsize = 2, jitter_seed = 1,
                                   theme = ggplot2::theme_minimal()) {

  data |>
    dplyr::filter(year >= 2019) |>
    ggplot2::ggplot(ggplot2::aes(x = year, y = deposition / cln, color = ecosys)) +
    ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), color = colour, linewidth = linewidth, show.legend = FALSE) +
    ggplot2::geom_point(size = pointsize * 1.5, position = ggplot2::position_jitter(width = 0.1, height = 0, seed = jitter_seed)) +
    ggplot2::scale_x_continuous(limits = c(2019,NA), expand = c(0.01,0.01)) +
    ggplot2::scale_y_continuous(limits = c(0,NA), expand = ggplot2::expansion(mult = c(0, 0.02)), labels = scales::percent_format()) +
    colour_scale +
    ggplot2::ggtitle(
      label = openair::quickText("Luftqualitätsmesswerte - Stickstoffeintrag in empfindliche Ökosysteme seit 2019"),
      subtitle = expression("Stickstoffeintrag vs. kritische Eintragsrate (relativ)")
    ) +
    ggplot2::labs(caption = "Quelle: Ostluft") +
    theme
}


#' Moving windows of years, labelled by their range
#'
#' @param years Years available.
#' @param width Number of years per window.
#'
#' @return Named list of year vectors ("2023–2025" = 2023:2025), oldest window first. Stops with an error
#'   of class `airquality_plot_error` if no full window fits.
#'
#' @keywords internal
year_windows <- function(years, width) {
  ends <- sort(unique(years))
  ends <- ends[ends >= min(ends) + width - 1]
  if (length(ends) == 0) {
    cli::cli_abort("No window of {width} year{?s} fits into {.val {range(years)}}.", class = "airquality_plot_error")
  }

  purrr::map(rlang::set_names(ends, paste0(ends - width + 1, "–", ends)), \(end) seq(end - width + 1, end))
}


#' Largest relative value per reference, over all years
#'
#' @param data Data as returned by [threshold_comparison_data()] for all years.
#'
#' @return Tibble with `reference` and `value` (the maximum), for [plot_threshold_comparison()].
#'
#' @keywords internal
threshold_comparison_limits <- function(data) {
  dplyr::summarise(data, value = max(value, na.rm = TRUE), .by = reference)
}


#' Categories of each panel of the threshold comparison
#'
#' Which categories belong to a panel follows from the thresholds: the nitrogen deposition and the O3 peak
#' have an LRV limit only, the O3 peak season a WHO guideline only.
#'
#' @param data Data as returned by [threshold_comparison_data()] for all years.
#'
#' @return Tibble with `reference` and `x`, for [plot_threshold_comparison()].
#'
#' @keywords internal
threshold_comparison_categories <- function(data) {
  data |>
    dplyr::distinct(reference, x) |>
    dplyr::arrange(reference, x) # the order of the axis
}
