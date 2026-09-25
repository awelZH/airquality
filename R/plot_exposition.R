# Plots of the exposition page: population over thresholds, exposition distributions of the population
# and of sensitive ecosystems, population-weighted means (time series and municipality maps).


# ---- population over thresholds ----------------------------------------------------------

#' Count the inhabitants over the LRV limit, additionally over the WHO guideline, and below both
#'
#' Per parameter and year, from the exposition distribution; each parameter against the thresholds it
#' has (O3: the typical peak `O3_max_98p_m1` only against the LRV limit, the peak season
#' `O3_peakseason_mean_d1_max_mean_h8gl` only against the WHO guideline). "über WHO-Richtwert" counts
#' those over the WHO guideline but not over the LRV limit; "unter Grenz-/Richtwert" is the rest of the
#' population of the canton (small negative remainders are set to 0).
#'
#' @param distribution `data_exposition_distribution_pollutants.csv`.
#' @param weighted_means_canton `data_exposition_weighted_means_canton.csv` (population per year).
#' @param threshold_values Threshold values (`source`, `parameter`, `threshold`).
#'
#' @return Tibble with `pollutant` (German name, with the metric in brackets for pollutants with several
#'   parameters), `parameter`, `year`, `reference` (factor, "unter Grenz-/Richtwert" first) and
#'   `population`; a reference without threshold for the parameter has no row.
#'
#' @keywords internal
population_over_thresholds <- function(distribution, weighted_means_canton, threshold_values) {

  thresholds <-
    threshold_values |>
    dplyr::select(source, parameter, threshold) |>
    tidyr::pivot_wider(names_from = source, values_from = threshold)
  # over a threshold; NA if the parameter has none
  count_over <- function(concentration, population, threshold) {
    if (all(is.na(threshold))) NA_real_ else sum(population[concentration > threshold])
  }

  over <-
    distribution |>
    dplyr::mutate(
      pollutant = airquality.methods::longpollutant(pollutant),
      pollutant = if (dplyr::n_distinct(parameter) > 1) paste0(pollutant, " (", metric, ")") else pollutant,
      .by = pollutant
    ) |>
    dplyr::left_join(thresholds, by = dplyr::join_by(parameter)) |>
    dplyr::summarise(
      `über LRV-Grenzwert` = count_over(concentration, population, `LRV Grenzwert`),
      `über WHO-Richtwert` = count_over(concentration, population, `WHO Richtwert`),
      .by = c(pollutant, parameter, year)
    ) |>
    dplyr::mutate(`über WHO-Richtwert` = `über WHO-Richtwert` - dplyr::coalesce(`über LRV-Grenzwert`, 0)) |>
    tidyr::pivot_longer(c(`über LRV-Grenzwert`, `über WHO-Richtwert`), names_to = "reference", values_to = "population", cols_vary = "slowest") |>
    dplyr::filter(!is.na(population))

  population_total <-
    weighted_means_canton |>
    dplyr::distinct(year, parameter, population) |>
    dplyr::summarise(population_total = sum(population), .by = c(year, parameter))

  below <-
    over |>
    dplyr::summarise(population = sum(population), .by = c(pollutant, parameter, year)) |>
    dplyr::left_join(population_total, by = dplyr::join_by(year, parameter)) |>
    dplyr::mutate(
      population = population_total - population,
      reference = "unter Grenz-/Richtwert"
    ) |>
    dplyr::select(-population_total)

  over |>
    dplyr::bind_rows(below) |>
    dplyr::mutate(
      reference = factor(reference, levels = c("unter Grenz-/Richtwert", "über WHO-Richtwert", "über LRV-Grenzwert")),
      population = pmax(0, population) # small negative numbers possible due to rounding in the fundamental data
    )
}


#' Plot a time series of counts over thresholds as stacked bars
#'
#' @param data Data with `year`, `reference` (fill) and the count column `y`.
#' @param y Name of the count column.
#' @param colours Named fill colours of the `reference` levels.
#' @param title,subtitle,caption Title, subtitle and caption.
#' @param theme ggplot2 theme.
#'
#' @return A ggplot object.
#'
#' @keywords internal
ggplot_over_thresholds <- function(data, y, colours, title, subtitle, caption, theme = ggplot2::theme_minimal()) {

  data |>
    ggplot2::ggplot(ggplot2::aes(x = year, y = .data[[y]])) +
    ggplot2::geom_bar(mapping = ggplot2::aes(fill = reference), stat = "identity", position = "stack", width = 0.8) +
    ggplot2::scale_x_continuous(breaks = seq(1990,2100,5), expand = c(0.01,0.01)) +
    ggplot2::scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = "'"), expand = c(0.01, 0.01)) +
    ggplot2::scale_fill_manual(values = colours) +
    theme +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = "right"
    ) +
    ggplot2::ggtitle(label = title, subtitle = subtitle) +
    ggplot2::labs(caption = caption)
}


#' Plot the time series of the inhabitants over thresholds, one plot per parameter
#'
#' @param data Data as returned by [population_over_thresholds()].
#' @param parameters Parameters, e.g. "NO2", "O3_max_98p_m1".
#' @param colours Named fill colours of the three `reference` levels; the legend shows only the levels
#'   of the parameter.
#' @param theme ggplot2 theme.
#'
#' @return Named list of ggplot objects, one per parameter.
#'
#' @keywords internal
plot_population_over_thresholds <- function(data, parameters, colours, theme = ggplot2::theme_minimal()) {

  purrr::map(rlang::set_names(parameters), function(parameter) {
    data_plot <- dplyr::filter(data, parameter == !!parameter)
    ggplot_over_thresholds(
      data_plot, y = "population", colours = colours,
      title = openair::quickText(paste0("Entwicklung luftschadstoffbelasteter Wohnbevölkerung ", unique(data_plot$pollutant))),
      subtitle = "Anzahl Personen, Wohnbevölkerung im Kanton Zürich",
      caption = "Referenzwerte nach heutigem Stand, Datengrundlage: BAFU & BFS",
      theme = theme
    )
  })
}


#' Plot the share of the inhabitants over thresholds in the last years as doughnuts, one per pollutant
#'
#' @inheritParams plot_population_over_thresholds
#' @param n_years Number of the last years summed up.
#'
#' @return A ggplot object.
#'
#' @keywords internal
plot_population_over_thresholds_share <- function(data, n_years, colours, theme = ggplot2::theme_minimal()) {

  year_max <- max(data$year)

  data |>
    dplyr::filter(year %in% utils::tail(sort(unique(year)), n_years)) |>
    dplyr::summarise(population = sum(population), .by = c(pollutant, reference)) |>
    dplyr::arrange(pollutant, reference) |> # drawing order of the slices
    dplyr::mutate(population_relative = population / sum(population), .by = pollutant) |>
    ggplot2::ggplot(ggplot2::aes(x = 1, y = population_relative, fill = reference)) +
    ggplot2::geom_bar(stat = "identity", width = 0.5) +
    ggplot2::scale_x_continuous(limits = c(0.25,1.25), expand = c(0,0)) +
    ggplot2::scale_y_continuous(limits = c(0,1), labels = scales::percent_format(), expand = c(0,0)) +
    ggplot2::scale_fill_manual(values = colours) +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::facet_wrap(pollutant~., nrow = 1) +
    # the caption is replaced, not merged: a ggtext textbox in the theme does not merge with element_text()
    ggplot2::`%+replace%`(theme, ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0.5, color = "gray40", size = ggplot2::rel(0.66)))) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = ggplot2::rel(1), hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.8), hjust = 0.5)
    ) +
    ggplot2::ggtitle(
      label = "Luftschadstoffbelastete Wohnbevölkerung",
      subtitle = paste0("Anteil Personen im Kanton Zürich in den Jahren ", year_max - n_years + 1, " bis ", year_max)
    ) +
    ggplot2::labs(caption = "Referenzwerte nach heutigem Stand, Datengrundlage: BAFU & BFS")
}


# ---- page section per parameter --------------------------------------------------------------

#' Print the plots of one parameter on the exposition page
#'
#' An optional explanation, then three subsections: the histograms and the cumulative distributions
#' (year sliders), the inhabitants over thresholds, and the population-weighted mean as a tabset "Kanton"
#' (time series) / "Gemeinden" (maps, year slider). For a chunk with `#| output: asis`, or as the content
#' of a tab.
#'
#' @param catalog Plot catalog of the exposition (`plots_exposition`).
#' @param parameter Parameter, e.g. "NO2".
#' @param text Explanation printed first (Markdown), or `NULL`.
#'
#' @return `NULL`, invisibly; called for its output.
#'
#' @keywords internal
print_exposition_parameter <- function(catalog, parameter, text = NULL) {
  heading <- function(title) cat("\n\n#### ", title, "\n\n", sep = "")

  if (!is.null(text)) cat("\n\n", text, "\n\n", sep = "")
  heading("Belastungsverteilung")
  print_year_slider(catalog, "distribution_histogram", parameter)
  print_year_slider(catalog, "distribution_cumulative", parameter)
  heading("Entwicklung luftschadstoffbelastete Bevölkerung")
  print(airquality.methods::get_plot(catalog, "population_over_thresh", parameter))
  heading("mittlere Bevölkerungsbelastung")
  airquality.methods::print_tabset(list(
    Kanton = airquality.methods::get_plot(catalog, "population_weighted_mean", parameter),
    Gemeinden = \() print_year_slider(catalog, "population_weighted_mean_map", parameter)
  ))

  invisible(NULL)
}


# ---- ecosystems over the critical load -------------------------------------------------------

#' Count the sensitive ecosystems over and below the critical load of nitrogen, per year
#'
#' An ecosystem is over the critical load if its maximum exceedance (`ndep_exmax`, class centre) is above
#' 0, as in the distribution plots.
#'
#' @param distribution `data_exposition_distribution_ndep.csv`.
#'
#' @return Tibble with `year`, `reference` (factor, "unter krit. Eintragsrate" first) and `n_ecosys`, both
#'   levels for every year.
#'
#' @keywords internal
ecosystems_over_critical_load <- function(distribution) {
  levels <- c("unter krit. Eintragsrate", "über krit. Eintragsrate")

  distribution |>
    dplyr::mutate(reference = factor(ifelse(ndep_exmax > 0, levels[2], levels[1]), levels = levels)) |>
    dplyr::summarise(n_ecosys = sum(n_ecosys), .by = c(year, reference)) |>
    tidyr::complete(year, reference, fill = list(n_ecosys = 0)) |>
    dplyr::arrange(year, reference)
}


#' Plot the time series of the sensitive ecosystems over the critical load of nitrogen
#'
#' Like [plot_population_over_thresholds()], with the ecosystems instead of the inhabitants.
#'
#' @param data Data as returned by [ecosystems_over_critical_load()].
#' @param colours Named fill colours of the two `reference` levels.
#' @param theme ggplot2 theme.
#'
#' @return A ggplot object.
#'
#' @keywords internal
plot_ecosystems_over_critical_load <- function(data, colours, theme = ggplot2::theme_minimal()) {
  ggplot_over_thresholds(
    data, y = "n_ecosys", colours = colours,
    title = openair::quickText("Entwicklung stickstoffbelasteter empfindlicher Ökosysteme"),
    subtitle = "Anzahl empfindlicher Ökosysteme im Kanton Zürich",
    caption = "krit. Eintragsraten nach heutigem Stand, Daten: BAFU",
    theme = theme
  )
}


# ---- exposition distributions ----------------------------------------------------------------

#' Plot an exposition distribution as histogram
#'
#' @param data Distribution of one year.
#' @param x,y Names of the class and count columns.
#' @param barwidth Width of the bars.
#' @param xlims,xbreaks Limits and breaks of the x axis.
#' @param titlelab,captionlab,xlabel Title, caption and x axis label.
#' @param threshold Threshold lines as returned by [extract_threshold()]; `value = NA` for none.
#' @param fill_scale Fill scale of the classes; `NULL` for one colour.
#' @param theme ggplot2 theme.
#'
#' @return A ggplot object.
#'
#' @keywords internal
ggplot_exposition_histogram <- function(data, x, y, barwidth = 1, xlims = c(0,NA), xbreaks = ggplot2::waiver(), titlelab = NULL, captionlab = NULL, xlabel = NULL,
                             threshold = list(value = NA, label = NULL, labelsize = 4, linetype = 2, linesize = 1),
                             fill_scale = NULL, theme = ggplot2::theme_minimal()) {

  if (is.null(fill_scale)) {
    mapping <- ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y))
    bars <- ggplot2::geom_bar(stat = "identity", color = NA, width = barwidth, fill = "#50586C")
  } else {
    mapping <- ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y), fill = !!rlang::sym(x))
    bars <- ggplot2::geom_bar(stat = "identity", color = NA, width = barwidth)
  }

  plot <-
    ggplot2::ggplot(data, mapping = mapping) +
    bars +
    ggplot2::scale_x_continuous(limits = xlims, breaks = xbreaks, expand = c(0.01,0.01)) +
    ggplot2::scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01), labels = function(x) format(x, big.mark = "'", scientific = FALSE)) +
    fill_scale +
    xlabel +
    titlelab +
    captionlab +
    theme +
    ggplot2::theme(axis.title.x = ggplot2::element_text())

  add_threshold_lines(plot, threshold, legend_title = NULL) # the thresholds are the only legend here
}


#' Plot a relative cumulative exposition distribution
#'
#' @inheritParams ggplot_exposition_histogram
#' @param linewidth Width of the line.
#' @param background Data of all years, drawn as faint lines behind the year shown (one line per `year`);
#'   `NULL` for none.
#' @param background_colour,background_alpha Colour and opacity of these lines.
#'
#' @return A ggplot object.
#'
#' @keywords internal
ggplot_exposition_cumulative <- function(data, x, y, linewidth = 1, xlims = c(0,NA), xbreaks = ggplot2::waiver(), titlelab = NULL, captionlab = NULL, xlabel = NULL,
                                   threshold = list(value = NA, label = NULL, labelsize = 4, linetype = 2, linesize = 1),
                                   background = NULL, background_colour = "gray80", background_alpha = 0.6,
                                   theme = ggplot2::theme_minimal()) {

  # the other years as a faint background, so the year shown is seen in the context of the whole series
  years <- if (!is.null(background)) {
    ggplot2::geom_line(data = background, mapping = ggplot2::aes(group = year), linewidth = linewidth,
                       color = background_colour, alpha = background_alpha)
  }

  plot <-
    ggplot2::ggplot(data, mapping = ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y))) +
    years +
    ggplot2::geom_line(linewidth = linewidth, color = "#50586C") +
    ggplot2::scale_x_continuous(limits = xlims, breaks = xbreaks, expand = c(0.01,0.01)) +
    ggplot2::scale_y_continuous(limits = c(0,1), expand = c(0.01,0.01), labels = scales::percent_format()) +
    xlabel +
    titlelab +
    captionlab +
    theme +
    ggplot2::theme(axis.title.x = ggplot2::element_text())

  add_threshold_lines(plot, threshold, legend_title = NULL) # the thresholds are the only legend here
}


#' Plot the relative cumulative exposition distributions of all years in one plot, one line per year
#'
#' @inheritParams ggplot_exposition_histogram
#' @param title,subtitle,caption Texts of the plot.
#'
#' @return A ggplot object.
#'
#' @keywords internal
ggplot_exposition_cumulative_years <- function(data, x, y, xbreaks, threshold, xlabel, title, subtitle, caption,
                                         theme = ggplot2::theme_minimal()) {

  # the threshold lines first, so the years are drawn over them
  ggplot2::ggplot(data, mapping = ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y), color = factor(year), group = year)) |>
    add_threshold_lines(threshold, legend_title = NULL) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_x_continuous(limits = range(xbreaks), breaks = xbreaks, expand = c(0.01,0.01)) +
    ggplot2::scale_y_continuous(limits = c(0,1), expand = c(0.01,0.01), labels = scales::percent_format()) +
    colorspace::scale_color_discrete_diverging(name = "Jahr", palette = "Blue-Yellow") +
    ggplot2::guides(color = ggplot2::guide_legend(ncol = 2)) +
    xlabel +
    ggplot2::ggtitle(label = title, subtitle = subtitle) +
    ggplot2::labs(caption = caption) +
    theme +
    ggplot2::theme(axis.title.x = ggplot2::element_text()) +
    threshold_legend_spacing() # the complete theme above dropped it
}


#' Plot the population exposition histograms of one parameter, one plot per year
#'
#' @param data `data_exposition_distribution_pollutants.csv`.
#' @param parameter Parameter, e.g. "NO2".
#' @param threshold_values Threshold values with line styles.
#' @param axes Bar width and x breaks per parameter (`plot_axes_exposition` of `report/plots/_plot_setup.R`); the
#'   range of the breaks is the x range.
#' @param sub Area in the subtitle.
#' @param theme ggplot2 theme.
#'
#' @return Named list of ggplot objects, one per year.
#'
#' @keywords internal
plot_exposition_histograms <- function(data, parameter, threshold_values, axes, sub = "im Kanton Zürich", theme = ggplot2::theme_minimal()) {

  data <- dplyr::filter(data, parameter == !!parameter)
  pars <- parameter_setting(axes, parameter)

  purrr::map(rlang::set_names(unique(data$year)), function(year) {

    data_plot <- dplyr::filter(data, year == !!year)
    pollutant <- unique(data_plot$pollutant)
    metric <- unique(data_plot$metric)

    ggplot_exposition_histogram(
      data = data_plot, x = "concentration", y = "population", barwidth = pars$barwidth,
      xlims = range(pars$xbreaks), xbreaks = pars$xbreaks, threshold = extract_threshold(threshold_values, pollutant, metric),
      xlabel = ggplot2::xlab(openair::quickText(paste0(pollutant, " ", metric, " (µg/m3)"))),
      titlelab = ggplot2::ggtitle(
        label = openair::quickText(paste0("Bevölkerungsexposition ", airquality.methods::longpollutant(pollutant))),
        subtitle = paste0("Anzahl Personen, Wohnbevölkerung ",sub," im Jahr ",year)
      ),
      captionlab = ggplot2::labs(caption = "Referenzwerte nach heutigem Stand, Datengrundlage: BAFU & BFS"),
      theme = theme
    ) +
      ggplot2::guides(fill = "none") # the classes need no legend; the thresholds have one
  })
}


#' Plot the relative cumulative population exposition of one parameter: all years, then one plot per year
#'
#' @inheritParams plot_exposition_histograms
#'
#' @return Named list of ggplot objects: "alle" (all years in one plot), then one per year.
#'
#' @keywords internal
plot_exposition_cumulative <- function(data, parameter, threshold_values, axes, sub = "im Kanton Zürich", theme = ggplot2::theme_minimal()) {

  data <- dplyr::filter(data, parameter == !!parameter)
  pars <- parameter_setting(axes, parameter)
  pollutant <- unique(data$pollutant)
  metric <- unique(data$metric)
  threshold <- extract_threshold(threshold_values, pollutant, metric)
  xlabel <- ggplot2::xlab(openair::quickText(paste0(pollutant," ",metric," (µg/m3)")))
  title <- openair::quickText(paste0("Bevölkerungsexposition ", airquality.methods::longpollutant(parameter)))

  plot_all <- ggplot_exposition_cumulative_years(
    data, x = "concentration", y = "population_cum_rel", xbreaks = pars$xbreaks, threshold = threshold, xlabel = xlabel,
    title = title, subtitle = paste0("relativer Anteil (kumuliert), Wohnbevölkerung ", sub),
    caption = "Referenzwerte nach heutigem Stand, Datengrundlage: BAFU & BFS", theme = theme
  )

  plots_years <- purrr::map(rlang::set_names(unique(data$year)), function(year) {
    ggplot_exposition_cumulative(
      data = dplyr::filter(data, year == !!year), x = "concentration", y = "population_cum_rel", linewidth = 1,
      xlims = range(pars$xbreaks), xbreaks = pars$xbreaks, threshold = threshold,
      background = data, xlabel = xlabel,
      titlelab = ggplot2::ggtitle(
        label = title,
        subtitle = openair::quickText(paste0("relativer Anteil (kumuliert), Wohnbevölkerung ",sub," im Jahr ",year))
      ),
      captionlab = ggplot2::labs(caption = "Referenzwerte nach heutigem Stand, Datengrundlage: BAFU & BFS"),
      theme = theme
    )
  })

  c(list(alle = plot_all), plots_years)
}


#' Plot the nitrogen exceedance histograms of the sensitive ecosystems, one plot per year
#'
#' @param data `data_exposition_distribution_ndep.csv`.
#' @param threshold_ndep Threshold line at 0 (critical load) as a list like [extract_threshold()] returns.
#' @param axes Bar width and x breaks per parameter, with an entry "Ndep" (`plot_axes_exposition`).
#' @param sub Area in the subtitle.
#' @param theme ggplot2 theme.
#'
#' @return Named list of ggplot objects, one per year.
#'
#' @keywords internal
plot_ndep_exposition_histograms <- function(data, threshold_ndep, axes, sub = "im Kanton Zürich", theme = ggplot2::theme_minimal()) {

  pars <- parameter_setting(axes, "Ndep")

  purrr::map(rlang::set_names(unique(data$year)), function(year) {
    ggplot_exposition_histogram(
      data = dplyr::filter(data, year == !!year), x = "ndep_exmax", y = "n_ecosys", barwidth = pars$barwidth,
      xlims = range(pars$xbreaks), xbreaks = pars$xbreaks, threshold = threshold_ndep,
      xlabel = ggplot2::xlab(expression("max. Stickstoff-Überschuss im Vergleich zu den kritischen Eintragsraten (kgN " * ha^-1 * Jahr^-1 * ")")),
      titlelab = ggplot2::ggtitle(
        label = openair::quickText("Exposition empfindlicher Ökosysteme durch Stickstoffeinträge"),
        subtitle = paste0("Anzahl empfindlicher Ökosysteme ",sub," im Jahr ", year)
      ),
      captionlab = ggplot2::labs(caption = "krit. Eintragsraten nach heutigem Stand, Daten: BAFU"),
      theme = theme
    )
  })
}


#' Plot the relative cumulative nitrogen exceedance of the sensitive ecosystems: all years, then one plot per year
#'
#' @inheritParams plot_ndep_exposition_histograms
#'
#' @return Named list of ggplot objects: "alle" (all years in one plot), then one per year.
#'
#' @keywords internal
plot_ndep_exposition_cumulative <- function(data, threshold_ndep, axes, sub = "im Kanton Zürich", theme = ggplot2::theme_minimal()) {

  pars <- parameter_setting(axes, "Ndep")
  xlabel <- ggplot2::xlab(expression("max. Stickstoff-Überschuss im Vergleich zu den kritischen Eintragsraten (kgN " * ha^-1 * Jahr^-1 * ")"))
  title <- openair::quickText("Exposition empfindlicher Ökosysteme durch Stickstoffeinträge")

  plot_all <- ggplot_exposition_cumulative_years(
    data, x = "ndep_exmax", y = "n_ecosys_cum_rel", xbreaks = pars$xbreaks, threshold = threshold_ndep, xlabel = xlabel,
    title = title, subtitle = paste0("relativer Anteil empfindlicher Ökosysteme (kumuliert) ", sub),
    caption = "krit. Eintragsraten nach heutigem Stand, Daten: BAFU", theme = theme
  )

  plots_years <- purrr::map(rlang::set_names(unique(data$year)), function(year) {
    ggplot_exposition_cumulative(
      data = dplyr::filter(data, year == !!year), x = "ndep_exmax", y = "n_ecosys_cum_rel", linewidth = 1,
      xlims = range(pars$xbreaks), xbreaks = pars$xbreaks, threshold = threshold_ndep,
      background = data, xlabel = xlabel,
      titlelab = ggplot2::ggtitle(
        label = title,
        subtitle = paste0("relativer Anteil empfindlicher Ökosysteme (kumuliert) ",sub," im Jahr ", year)
      ),
      captionlab = ggplot2::labs(caption = "krit. Eintragsraten nach heutigem Stand, Daten: BAFU"),
      theme = theme
    )
  })

  c(list(alle = plot_all), plots_years)
}


# ---- population-weighted means ---------------------------------------------------------------

#' Plot the population-weighted mean per year and its reduction vs. the base year, one plot per parameter
#'
#' @param data `data_exposition_weighted_means_canton.csv`.
#' @param parameters Parameters to plot.
#' @param theme ggplot2 theme.
#'
#' @return Named list of ggplot objects, one per parameter.
#'
#' @keywords internal
plot_weighted_mean_timeseries <- function(data, parameters, theme = ggplot2::theme_minimal()) {

  data <-
    data |>
    dplyr::mutate(delta_base = pmin(population_weighted_mean - population_weighted_mean_base, 0)) |>
    dplyr::select(year, pollutant, parameter, delta_base, population_weighted_mean, base_year) |>
    tidyr::pivot_longer(c(delta_base, population_weighted_mean), names_to = "scenario", values_to = "population_weighted_mean", cols_vary = "slowest") |>
    dplyr::mutate(scenario = dplyr::recode(scenario, population_weighted_mean = "tatsächliche Belastung", delta_base = paste0("vermindert vs. ", stats::na.omit(unique(base_year)))))

  purrr::map(rlang::set_names(parameters), function(parameter) {

    data_plot <- dplyr::filter(data, parameter == !!parameter)
    pollutant <- unique(data_plot$pollutant)
    ggplot_timeseries_bars(data_plot,
                           mapping = ggplot2::aes(x = year, y = population_weighted_mean, fill = scenario),
                           titlelab = ggplot2::ggtitle(
                             label = openair::quickText(paste0("Bevölkerungsgewichtete Schadstoffbelastung ", airquality.methods::longpollutant(pollutant))),
                             subtitle = openair::quickText(paste0(pollutant,", mittlere Schadstoffbelastung pro Einwohner/in (µg/m3)"))
                           ),
                           captionlab = ggplot2::labs(caption = "Datengrundlage: BAFU & BFS"),
                           theme = theme
    )
  })
}


#' Map the population-weighted means per municipality of one parameter, one map per year
#'
#' @param data `data_exposition_weighted_means_municipalities.csv` joined to the municipality boundaries
#'   (`sf`).
#' @param data_canton `data_exposition_weighted_means_canton.csv` (canton mean in the subtitle).
#' @param parameter Parameter, e.g. "NO2".
#' @param crs Coordinate reference system of the map.
#' @param theme ggplot2 theme.
#'
#' @return Named list of ggplot objects, one per year.
#'
#' @keywords internal
plot_weighted_mean_maps <- function(data, data_canton, parameter, crs, theme = ggplot2::theme_void()) {

  data <- dplyr::filter(data, parameter == !!parameter)

  purrr::map(rlang::set_names(unique(data$year)), function(year) {

    canton <- airquality.methods::round_off(dplyr::pull(dplyr::filter(data_canton, year == !!year & parameter == !!parameter), "population_weighted_mean"), 1)
    data_plot <- dplyr::filter(data, year == !!year)
    pollutant <- unique(data_plot$pollutant)

    data_plot |>
      ggplot2::ggplot(ggplot2::aes(fill = population_weighted_mean)) +
      ggplot2::geom_sf() +
      ggplot2::coord_sf(datum = sf::st_crs(crs)) +
      airquality.methods::immissionscale(parameter) +
      theme +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        plot.subtitle = ggplot2::element_text(hjust = 0.5),
        plot.caption = ggplot2::element_text(hjust = 0.5)
      ) +
      ggplot2::ggtitle(
        label = openair::quickText(paste0("Bevölkerungsgewichtete Schadstoffbelastung ", airquality.methods::longpollutant(pollutant))),
        subtitle = openair::quickText(paste0("Mittlere ",pollutant,"-Belastung pro Einwohner/in im Jahr ",year,"\ngesamt = ",canton," µg/m3"))
      ) +
      ggplot2::labs(caption = "Datengrundlage: BAFU & BFS")
  })
}


#' Table of the inhabitants over thresholds per pollutant and year
#'
#' The counts are formatted with thousands separators; "> WHO-Richtwert" counts all inhabitants over the
#' WHO guideline, including those over the LRV limit; "–" where the parameter has no such threshold.
#'
#' @param data Data as returned by [population_over_thresholds()].
#'
#' @return Tibble with `Jahr`, `Schadstoff`, `< Grenz-/Richtwert`, `> WHO-Richtwert`, `> LRV-Grenzwert`
#'   (character), newest year first.
#'
#' @keywords internal
table_population_over_thresholds <- function(data) {
  big <- function(x) ifelse(is.na(x), "–", format(x, scientific = FALSE, big.mark = "'"))

  data |>
    dplyr::select(year, pollutant, reference, population) |>
    tidyr::pivot_wider(names_from = reference, values_from = population, names_sort = TRUE, names_expand = TRUE) |>
    dplyr::arrange(pollutant, dplyr::desc(year)) |>
    dplyr::mutate(
      `über WHO-Richtwert` = big(dplyr::coalesce(`über LRV-Grenzwert`, 0) + `über WHO-Richtwert`),
      `über LRV-Grenzwert` = big(`über LRV-Grenzwert`),
      `unter Grenz-/Richtwert` = big(`unter Grenz-/Richtwert`)
    ) |>
    dplyr::rename(
      Jahr = year,
      Schadstoff = pollutant,
      "< Grenz-/Richtwert" = `unter Grenz-/Richtwert`,
      "> LRV-Grenzwert" = `über LRV-Grenzwert`,
      "> WHO-Richtwert" = `über WHO-Richtwert`
    )
}
