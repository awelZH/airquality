
#' Plot timeseries yearly using bars
#'
#' @param data
#' @param mapping
#' @param ylims
#' @param ybreaks
#' @param titlelab
#' @param captionlab
#' @param pointshape
#' @param pointsize
#' @param threshold
#' @param theme
#'
#' @keywords internal
ggplot_timeseries <- function(data, mapping = ggplot2::aes(x = year, y = concentration, color = siteclass), ylims = c(0,NA), ybreaks = waiver(), titlelab = NULL, captionlab = NULL, pointshape = 19, pointsize = 2,
                              threshold = list(value = NA, color = "gray30", label = NULL, labelsize = 4, linetype = 2, linesize = 1),
                              theme = ggplot2::theme_minimal()) {

  plot <-
    ggplot2::ggplot(data, mapping = mapping) +
    ggplot2::geom_point(size = pointsize, shape = pointshape) +
    # ggiraph::geom_point_interactive(mapping = ggplot2::aes(data_id = site, tooltip = round_off(value, 1)), size = pointsize, shape = pointshape) +
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

  # plot <- ggiraph::girafe(ggobj = plot, width_svg = 6, height_svg = 4)

  return(plot)
}


#' Plot timeseries using lines
#'
#' @param data
#' @param mapping
#' @param ylims
#' @param ybreaks
#' @param titlelab
#' @param captionlab
#' @param pointshape
#' @param pointsize
#' @param threshold
#' @param theme
#'
#' @keywords internal
ggplot_timeseries_lines <- function(data, mapping = ggplot2::aes(x = year, y = population_weighted_mean), ylims = c(0,NA), ybreaks = waiver(), titlelab = NULL, captionlab = NULL,
                                    theme = ggplot2::theme_minimal()) {

  plot <-
    ggplot2::ggplot(data, mapping = mapping) +
    # ggplot2::geom_bar(stat = "identity", color = NA, fill = "#50586C") +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(breaks = seq(1990,2100,5), expand = c(0.01,0.01)) +
    ggplot2::scale_y_continuous(limits = ylims, breaks = ybreaks, expand = c(0.01,0.01)) +
    colorspace::scale_color_discrete_sequential(name = "Kanton", palette = "Viridis") +
    titlelab +
    captionlab +
    theme

  # plot <- ggiraph::girafe(ggobj = plot, width_svg = 6, height_svg = 4)

  return(plot)
}



#' Plot bar timeseries for yearly population-weighted mean concentration or health-outcome data using ggplot2
#'
#' @param data
#' @param mapping
#' @param ylims
#' @param ybreaks
#' @param titlelab
#' @param captionlab
#' @param pointshape
#' @param pointsize
#' @param threshold
#' @param theme
#'
#' @keywords internal
ggplot_timeseries_bars <- function(data, mapping = ggplot2::aes(x = year, y = population_weighted_mean, fill = scenario), ylims = c(NA,NA), ybreaks = waiver(), titlelab = NULL, captionlab = NULL,
                                   theme = ggplot2::theme_minimal()) {

  plot <-
    ggplot2::ggplot(data, mapping = mapping) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_hline(yintercept = 0, color = "gray30", linetype = 2) +
    ggplot2::scale_x_continuous(breaks = seq(1990,2100,5), expand = c(0.01,0.01)) +
    ggplot2::scale_y_continuous(limits = ylims, breaks = ybreaks, expand = c(0.01,0.01)) +
    scale_fill_manual(name = "Szenario", values = c("#50586C", "#DCE2F0")) +
    titlelab +
    captionlab +
    theme

  # plot <- ggiraph::girafe(ggobj = plot, width_svg = 6, height_svg = 4)

  return(plot)
}





#' Plot exposition distribution histogram using ggplot2
#'
#' @param data
#' @param x
#' @param y
#' @param barwidth
#' @param xlims
#' @param xbreaks
#' @param titlelab
#' @param captionlab
#' @param xlabel
#' @param threshold
#' @param fill_scale
#' @param theme
#'
#' @keywords internal
ggplot_expo_hist <- function(data, x, y, barwidth = 1, xlims = c(0,NA), xbreaks = waiver(), titlelab = NULL, captionlab = NULL, xlabel = NULL,
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

  if (!is.na(sum(threshold$value))){
    text <- tibble::tibble(x = threshold$value, label = threshold$labels)
    plot <-
      plot +
      ggplot2::geom_vline(xintercept = threshold$value, color = threshold$color, linetype = threshold$linetype, linewidth = threshold$linesize) +
      ggplot2::geom_text(data = text, mapping = ggplot2::aes(x = x, y = 0, label = label), size = threshold$labelsize,
                         hjust = 0, vjust = 0, angle = 90, nudge_x = pmin(0, -0.01 * max(xlims), na.rm = TRUE), inherit.aes = FALSE)
  }

  return(plot)
}




### function to
#' Plot relative cumulative exposition distribution using ggplot2
#'
#' @param data
#' @param x
#' @param y
#' @param linewidth
#' @param xlims
#' @param xbreaks
#' @param titlelab
#' @param captionlab
#' @param xlabel
#' @param threshold
#' @param theme
#'
#' @keywords internal
ggplot_expo_cumulative <- function(data, x, y, linewidth = 1, xlims = c(0,NA), xbreaks = waiver(), titlelab = NULL, captionlab = NULL, xlabel = NULL,
                                   threshold = list(value = NA, label = NULL, labelsize = 4, linetype = 2, linesize = 1),
                                   theme = ggplot2::theme_minimal()) {

  plot <-
    ggplot2::ggplot(data, mapping = ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y))) +
    ggplot2::geom_line(linewidth = linewidth, color = "#50586C") +
    ggplot2::scale_x_continuous(limits = xlims, breaks = xbreaks, expand = c(0.01,0.01)) +
    ggplot2::scale_y_continuous(limits = c(0,1), expand = c(0.01,0.01), labels = scales::percent_format()) +
    xlabel +
    titlelab +
    captionlab +
    theme +
    ggplot2::theme(axis.title.x = ggplot2::element_text())

  if (!is.na(sum(threshold$value))){
    text <- tibble::tibble(x = threshold$value, label = threshold$labels)
    plot <-
      plot +
      ggplot2::geom_vline(xintercept = threshold$value, color = threshold$color, linetype = threshold$linetype, linewidth = threshold$linesize) +
      ggplot2::geom_text(data = text, mapping = ggplot2::aes(x = x, y = 0, label = label), size = threshold$labelsize,
                         hjust = 0, vjust = 0, angle = 90, nudge_x = pmin(0, -0.01 * max(xlims), na.rm = TRUE), inherit.aes = FALSE)
  }

  return(plot)
}




#' Plot emission inventory time series as stacked bars, with sectors as legend blocks
#'
#' Every subsector (`subsector_new`) is one colour (`col`); the legend shows one block per sector
#' with the sector as title and its subsectors below ([add_grouped_legend()]). Order of the blocks
#' and of the subsectors within a block follows `order`.
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
  data <- dplyr::mutate(data, key = grouped_key(sector, subsector_new, order, group_order = group_order))
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

  add_grouped_legend(plot)
}


# ---- grouped legend (generic; candidate for airquality.methods) ---------------------

#' Unique key per group and element, for a grouped legend
#'
#' Elements with the same name in different groups (e.g. "verschiedene" in several sectors) get
#' different keys. The factor levels define the order of the stack and of the legend: groups in
#' the order of their first element (or `group_order`), elements within a group by `order`.
#'
#' @param group Group of each element, e.g. the sector.
#' @param key Element, e.g. the subsector.
#' @param order Sort order of the elements (numeric); `NULL` keeps the order of appearance.
#' @param group_order Groups in the wanted order; groups not listed follow in their default order.
#' @param sep Separator between group and element; must not occur in the names.
#'
#' @return Factor with values `"<group><sep><key>"`.
#'
#' @keywords internal
grouped_key <- function(group, key, order = NULL, group_order = NULL, sep = "::") {
  if (any(stringr::str_detect(c(group, key), stringr::fixed(sep)))) {
    cli::cli_abort("Group and element names must not contain the separator {.val {sep}}.")
  }
  order <- order %||% seq_along(group)
  groups <- unique(c(intersect(group_order, group), group[base::order(order)]))

  id <- paste0(group, sep, key)
  levels <- unique(id[base::order(match(group, groups), order)])
  factor(id, levels = levels)
}


#' Use a grouped legend: one block per group, with the group as title
#'
#' Each group of the legend (e.g. a sector) becomes a block with the group as title and its
#' elements (e.g. the subsectors) without the group name, drawn by
#' [legendry::guide_legend_group()]. Works for any discrete scale of `aesthetic` whose breaks are
#' keys from [grouped_key()]. The block titles look like the entries, the keys of a block have no
#' gaps (as in a ggplot legend), and the blocks are set apart by `spacing`.
#'
#' The result is an ordinary ggplot: position, size etc. of the legend follow the theme, also
#' when changed later. Only the default style of the block titles is taken from the theme at the
#' time of the call.
#'
#' @param plot A ggplot object.
#' @param aesthetic Aesthetic with the grouped keys, e.g. `"fill"` or `"colour"`.
#' @param sep Separator used in [grouped_key()].
#' @param spacing Space between the blocks.
#' @param subtitle Text element of the block titles; `NULL` takes the plot's `legend.text`.
#' @param key_spacing Vertical space between the keys of a block.
#'
#' @return The ggplot object with the grouped legend.
#'
#' @keywords internal
add_grouped_legend <- function(plot, aesthetic = "fill", sep = "::", spacing = grid::unit(3, "mm"), subtitle = NULL,
                               key_spacing = grid::unit(0, "pt")) {
  if (is.null(ggplot2::get_guide_data(plot, aesthetic))) {
    cli::cli_abort("The plot has no legend for {.val {aesthetic}}.")
  }

  if (is.null(subtitle)) {
    # style of the entries, but not blank when the legend title is blank (subtitles inherit from it)
    text <- ggplot2::calc_element("legend.text", ggplot2::complete_theme(plot$theme))
    subtitle <- ggplot2::element_text(family = text$family, face = text$face, colour = text$colour,
                                      size = text$size, inherit.blank = FALSE)
  }
  guide <- legendry::guide_legend_group(key = legendry::key_group_split(sep = sep))

  plot +
    do.call(ggplot2::guides, rlang::set_names(list(guide), aesthetic)) +
    ggplot2::theme(legendry.group.spacing = spacing, legendry.legend.subtitle = subtitle,
                   legend.key.spacing.y = key_spacing)
}



#' Wrapper to supply pollutant-specific list of parameters for timeseries plotting
#'
#' @param parameter
#'
#' @keywords internal
timeseriespars <- function(parameter) {
  switch(parameter,
         NO2 = list(ylim = c(0,70), ybreaks = seq(0,70,10), metric = "Jahresmittel", thresh = extract_threshold(immission_threshold_values, pollutant = "NO2")),
         PM10 = list(ylim = c(0,35), ybreaks = seq(0,35,5), metric = "Jahresmittel", thresh = extract_threshold(immission_threshold_values, pollutant = "PM10")),
         PM2.5 = list(ylim = c(0,20), ybreaks = seq(0,20,4), metric = "Jahresmittel", thresh = extract_threshold(immission_threshold_values, pollutant = "PM2.5")),
         eBC = list(ylim = c(0,4), ybreaks = seq(0,4,0.5), metric = "Jahresmittel", thresh = list(value = NA)),
         `O3_max_98p_m1` = list(ylim = c(0,210), ybreaks = seq(0,210,30), metric = "höchstes 98%-Perzentil der Halbstundenmittel eines Monats", thresh = extract_threshold(immission_threshold_values, pollutant = "O3", metric = "typische Spitzenbelastung", source = "LRV Grenzwert")),
         O3_peakseason_mean_d1_max_mean_h8gl = list(ylim = c(0,130), ybreaks = seq(0,120,20), metric = "mittlere tägliche max. 8-Stundenmittel während der Sommersaison", thresh = extract_threshold(immission_threshold_values, pollutant = "O3", metric = "mittlere Sommertagbelastung", source = "WHO Richtwert"))
  )
}



#' Wrapper to supply pollutant-specific list of parameters for exposition plotting
#'
#' @param parameter
#'
#' @keywords internal
expositionpars <- function(parameter) {
  wscale <- 0.9
  switch(parameter,
         NO2 = list(barwidth = 1 * wscale, xbreaks = seq(0,55,5), aggregation = "y1", metric = "mean"),
         `O3_max_98p_m1` = list(barwidth = 2 * wscale, xbreaks = seq(0,180,20), aggregation = "m1", metric = "monthly 98%-percentile of ½ hour mean values ≤ 100 µg/m3"),
         `O3_peakseason_mean_d1_max_mean_h8gl` = list(barwidth = 2 * wscale, xbreaks = seq(0,120,10), aggregation = "peak-season", metric = "mean of daily maximum 8-hour mean concentration in the six consecutive months with the highest six-month running-mean concentration"),
         PM10 = list(barwidth = 0.5 * wscale, xbreaks = seq(0,24,2), aggregation = "y1", metric = "mean"),
         PM2.5 = list(barwidth = 0.5 * wscale, xbreaks = seq(0,18.5,1), aggregation = "y1", metric = "mean"),
         eBC = list(barwidth = 0.05 * wscale, xbreaks = seq(0,2.2,0.2), aggregation = "y1", metric = "mean"),
         Ndep = list(barwidth = 1 * wscale, xlim = c(-5,90), xbreaks = seq(-5,45,5), aggregation = "y1", metric = "sum")
  )
}


#' Wrapper to plot timeseries yearly data using ggplot2 providing pollutant-specific list
#'
#' @param data
#' @param parameters
#' @param cap
#'
#' @keywords internal
plot_pars_monitoring_timeseries <- function(data, parameters, cap = "Datenabdeckung: Kanton Zürich, Daten: Ostluft & NABEL (BAFU & Empa)") {

  plots <-
    lapply(setNames(parameters, parameters), function(parameter) {

      data_plot <- dplyr::filter(data, parameter == !!parameter)
      pollutant <- unique(data_plot$pollutant)
      unit <- unique(data_plot$unit)
      metric <- unique(data_plot$metric)

      ggplot_timeseries(data_plot,
                        ylims = timeseriespars(parameter)$ylim, ybreaks = timeseriespars(parameter)$ybreaks,
                        titlelab = ggplot2::ggtitle(
                          label = openair::quickText(paste0("Luftqualitätsmesswerte ",longpollutant(pollutant))),
                          subtitle = openair::quickText(paste0(pollutant, ", ", metric," (", unit, ")"))
                        ),
                        captionlab = ggplot2::labs(caption = cap),
                        pointsize = pointsize, theme = theme_ts, threshold = timeseriespars(parameter)$thresh
      ) +
        scale_color_siteclass

    })

  return(plots)
}



#' Plot yearly nitrogen deposition timeseries using ggplot2
#'
#' @param data
#' @param xlim
#' @param xbreaks
#' @param linewidth
#' @param color
#' @param title
#'
#' @keywords internal
plot_timeseries_ndep_bars <- function(data, xlim = NULL, xbreaks = waiver(), linewidth = 1, color = "red3", title = "Luftqualitätsmesswerte - Stickstoffeintrag in empfindliche Ökosysteme") {

  cln <- dplyr::distinct(data, site, ecosys, cln)

  plot <-
    data |>
    ggplot2::ggplot(ggplot2::aes(x = year, y = deposition, fill = component)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_hline(data = cln, mapping = ggplot2::aes(yintercept = cln, group = site), color = color, linewidth = linewidth, show.legend = FALSE) +
    # ggplot2::scale_linetype_manual(values = c("critical_load_single" = 1, "critical_load_min" = 2, "critical_load_max" = 2)) +
    ggplot2::scale_x_continuous(limits = xlim, breaks = xbreaks, expand = c(0.01,0.01)) +
    ggplot2::scale_y_continuous(expand = c(0.01,0.01)) +
    ggplot2::scale_fill_manual(values = c("aus NH3-Quellen" = "#2A5676", "aus NOx-Quellen" = "#B696D6")) +
    theme_ts +
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



#' Wrapper to plot pollutant-specific population exposition histograms in a nested list by pollutant & year
#'
#' @param parameter
#' @param data
#'
#' @keywords internal
plot_all_expo_hist <- function(parameter, data, sub = "im Kanton Zürich") {

  data <- dplyr::filter(data, parameter == !!parameter)
  years_exposition <- setNames(unique(data$year), as.character(unique(data$year)))
  plots <- lapply(years_exposition, function(year) {

    data_plot <- dplyr::filter(data, year == !!year & parameter == !!parameter)
    pollutant <- unique(data_plot$pollutant)
    metric <- unique(data_plot$metric)
    thresh <- extract_threshold(immission_threshold_values, pollutant, metric)

    ggplot_expo_hist(
      data = data_plot, x = "concentration", y = "population", barwidth = expositionpars(parameter)$barwidth,
      xlims = range(expositionpars(parameter)$xbreaks), xbreaks = expositionpars(parameter)$xbreaks, threshold = thresh,
      xlabel = ggplot2::xlab(openair::quickText(paste0(pollutant, " ", metric, " (µg/m3)"))),
      titlelab = ggplot2::ggtitle(
        label = openair::quickText(paste0("Bevölkerungsexposition ", longpollutant(pollutant))),
        subtitle = paste0("Anzahl Personen, Wohnbevölkerung ",sub," im Jahr ",year)
      ),
      captionlab = ggplot2::labs(caption = "Datengrundlage: BAFU & BFS"),
      # fill_scale = immissionscale(parameter),
      theme = theme_ts
    ) +
      ggplot2::theme(legend.position = "none")

  })

  return(plots)
}



#' Wrapper to plot pollutant-specific cumulated population exposition distribution in a nested list by pollutant & year
#'
#' @param parameter
#' @param data
#'
#' @keywords internal
plot_all_expo_cumul <- function(parameter, data, sub = "im Kanton Zürich") {

  data <- dplyr::filter(data, parameter == !!parameter)
  years_exposition <- setNames(unique(data$year), as.character(unique(data$year)))
  plots <- lapply(years_exposition, function(year) {

    data_plot <- dplyr::filter(data, year == !!year & parameter == !!parameter)
    pollutant <- unique(data_plot$pollutant)
    metric <- unique(data_plot$metric)
    thresh <- extract_threshold(immission_threshold_values, pollutant, metric)
    ggplot_expo_cumulative(
      data = data_plot, x = "concentration", y = "population_cum_rel", linewidth = 1,
      xlims = range(expositionpars(parameter)$xbreaks), xbreaks = expositionpars(parameter)$xbreaks, threshold = thresh,
      xlabel = ggplot2::xlab(openair::quickText(paste0(pollutant," ",metric," (µg/m3)"))),
      titlelab = ggplot2::ggtitle(
        label = openair::quickText(paste0("Bevölkerungsexposition ",longpollutant(parameter))),
        subtitle = openair::quickText(paste0("relativer Anteil (kumuliert), Wohnbevölkerung ",sub," im Jahr ",year))
      ),
      captionlab = ggplot2::labs(caption = "Datengrundlage: BAFU & BFS"),
      theme = theme_ts
    )

  })

  return(plots)
}


#' Wrapper to plot timeseries of population-weighted mean concentration data using ggplot2 providing pollutant-specific list
#'
#' @param data
#' @param parameters
#' @param version
#' @param id_subareas
#' @param y
#'
#' @keywords internal
plot_pars_popmean_timeseries <- function(data, parameters, version = "overall", id_subareas = NULL, y = "population_weighted_mean") {

  if (version == "overall") {

    data <-
      data |>
      dplyr::mutate(delta_base = pmin(population_weighted_mean - population_weighted_mean_base, 0)) |>
      dplyr::select(year, pollutant, parameter, delta_base, population_weighted_mean, base_year) |>
      tidyr::gather(scenario, population_weighted_mean, -year, -pollutant, -parameter, -base_year) |>
      dplyr::mutate(scenario = dplyr::recode(scenario, population_weighted_mean = "tatsächliche Belastung", delta_base = paste0("vermindert vs. ",na.omit(unique(.data$base_year)))))

    plots <-
      lapply(setNames(parameters, parameters), function(parameter) {

        data_plot <- dplyr::filter(data, parameter == !!parameter)
        pollutant <- unique(data_plot$pollutant)
        ggplot_timeseries_bars(data_plot,
                               mapping = ggplot2::aes(x = year, y = !!rlang::sym(y), fill = scenario),
                               titlelab = ggplot2::ggtitle(
                                 label = openair::quickText(paste0("Bevölkerungsgewichtete Schadstoffbelastung ",longpollutant(pollutant))),
                                 subtitle = openair::quickText(paste0(pollutant,", mittlere Schadstoffbelastung pro Einwohner/in (µg/m3)"))
                               ),
                               captionlab = ggplot2::labs(caption = "Datengrundlage: BAFU & BFS"),
                               theme = theme_ts
        )

      })

  }

  if (version == "subareas") {

    data <-
      data |>
      dplyr::select(!!id_subareas, year, pollutant, parameter, population_weighted_mean) |>
      tidyr::gather(scenario, population_weighted_mean, -!!id_subareas, -year, -pollutant, -parameter)

    plots <-
      lapply(setNames(parameters, parameters), function(parameter) {

        data_plot <- dplyr::filter(data, parameter == !!parameter)
        pollutant <- unique(data_plot$pollutant)
        ggplot_timeseries_lines(data_plot,
                                mapping = ggplot2::aes(x = year, y = !!rlang::sym(y), color = !!rlang::sym(id_subareas)),
                                titlelab = ggplot2::ggtitle(
                                  label = openair::quickText(paste0("Bevölkerungsgewichtete Schadstoffbelastung ",longpollutant(pollutant))),
                                  subtitle = openair::quickText(paste0(pollutant,", mittlere Schadstoffbelastung pro Einwohner/in (µg/m3)"))
                                ),
                                captionlab = ggplot2::labs(caption = "Datengrundlage: BAFU & BFS"),
                                theme = theme_ts
        )


      })
  }

  return(plots)
}


#' Wrapper to plot timeseries of health-outcome preliminary deaths using ggplot2 providing pollutant-specific list
#'
#' @param data
#' @param parameters
#' @param relative
#'
#' @keywords internal
plot_pars_prelim_deaths_timeseries <- function(data, parameters, relative = FALSE) {

  plots <-
    lapply(setNames(parameters, parameters), function(parameter) {

      data <-
        data |>
        dplyr::filter(parameter == !!parameter & outcome_type == "vorzeitige Todesfälle") |>
        dplyr::mutate(
          covid = ifelse(year %in% 2020:2022, "Covid-19", "normal")
        )

      if (relative) {
        mppng <- ggplot2::aes(x = year, y = outcome / population * 10^5, fill = scenario, alpha = covid)
        sub <- "Anzahl vorzeitige Todesfälle pro 100'000 Einwohner/innen pro Jahr"
        uncertainty <- ggplot2::geom_linerange(ggplot2::aes(ymin = outcome_lower / population * 10^5, ymax = outcome_upper / population * 10^5 + outcome_delta_min_conc / population * 10^5), color = "gray20")
      } else {
        mppng <- ggplot2::aes(x = year, y = outcome, fill = scenario, alpha = covid)
        sub <- "Anzahl vorzeitige Todesfälle pro Jahr"
        uncertainty <- ggplot2::geom_linerange(ggplot2::aes(ymin = outcome_lower, ymax = outcome_upper + outcome_delta_min_conc), color = "gray20")
      }

      plot <-
        data |>
        ggplot_timeseries_bars(
          mapping = mppng,
          titlelab = ggplot2::ggtitle(
            label = openair::quickText(paste0("Vorzeitige Todesfälle durch ",longpollutant(parameter))),
            subtitle = sub
          ),
          captionlab = ggplot2::labs(caption = "Datengrundlage: BAFU & BFS & Statistisches Amt Kanton Zürich"),
          theme = theme_ts
        ) +
        uncertainty +
        ggplot2::scale_alpha_manual(name = "Aussergewöhnliches", values = c("normal" = 1, "Covid-19" = 0.25))

      return(plot)
    })

  return(plots)
}


#' Wrapper to plot pollutant-specific population weighted mean maps by municipality in a nested list by pollutant & year
#'
#' @param parameter
#' @param data
#' @param data_canton
#'
#' @keywords internal
plot_all_popweighmean_maps <- function(parameter, data, data_canton) {

  data <- dplyr::filter(data, parameter == !!parameter)
  years_exposition <- setNames(unique(data$year), as.character(unique(data$year)))
  plots <- lapply(years_exposition, function(year) {

    canton <- round_off(dplyr::pull(dplyr::filter(data_canton, year == !!year & parameter == !!parameter), "population_weighted_mean"), 1)
    data_plot <- dplyr::filter(data, year == !!year & parameter == !!parameter)
    pollutant <- unique(data_plot$pollutant)

    plot <-
      data_plot |>
      ggplot2::ggplot(ggplot2::aes(fill = population_weighted_mean)) +
      ggplot2::geom_sf() +
      # ggiraph::geom_sf_interactive(mapping = ggplot2::aes(data_id = gemeindename, tooltip = paste0(gemeindename, ", ", round_off(population_weighted_mean, 1)))) +
      ggplot2::coord_sf(datum = sf::st_crs(crs)) +
      immissionscale(parameter) +
      theme_map +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        plot.subtitle = ggplot2::element_text(hjust = 0.5),
        plot.caption = ggplot2::element_text(hjust = 0.5)
      ) +
      ggplot2::ggtitle(
        label = openair::quickText(paste0("Bevölkerungsgewichtete Schadstoffbelastung ",longpollutant(pollutant))),
        subtitle = openair::quickText(paste0("Mittlere ",pollutant,"-Belastung pro Einwohner/in im Jahr ",year,"\ngesamt = ",canton," µg/m3"))
      ) +
      ggplot2::labs(caption = "Datengrundlage: BAFU & BFS")

    # plot <- ggiraph::girafe(ggobj = plot, width_svg = 6, height_svg = 5, options = list(ggiraph::opts_hover_inv(css = "opacity:0.5;")))

    return(plot)
  })

  return(plots)
}



#' Wrapper to plot sensitive ecosystem nitrogen deposition exposition histograms in a list by year
#'
#' @param data
#' @param threshold_ndep
#'
#' @keywords internal
plot_all_expo_hist_ndep <- function(data, threshold_ndep, sub = "im Kanton Zürich") {

  years_exposition <- setNames(unique(data$year), as.character(unique(data$year)))

  plots <- lapply(years_exposition, function(year) {

    ggplot_expo_hist(
      data = dplyr::filter(data, year == !!year), x = "ndep_exmax", y = "n_ecosys", barwidth = expositionpars("Ndep")$barwidth,
      xlims = range(expositionpars("Ndep")$xbreaks), xbreaks = expositionpars("Ndep")$xbreaks, threshold = threshold_ndep,
      xlabel = ggplot2::xlab(expression("max. Stickstoff-Überschuss im Vergleich zu den kritischen Eintragsraten (kgN " * ha^-1 * Jahr^-1 * ")")),
      titlelab = ggplot2::ggtitle(
        label = openair::quickText("Exposition empfindlicher Ökosysteme durch Stickstoffeinträge"),
        subtitle = paste0("Anzahl empfindlicher Ökosysteme ",sub," im Jahr ", year)
      ),
      captionlab = ggplot2::labs(caption = "Daten: BAFU"),
      # fill_scale = immissionscale("Ndep"),
      theme = theme_ts
    )

  })

  return(plots)
}



#' Wrapper to plot sensitive ecosystem nitrogen deposition cumulative exposition distribution in a list by year
#'
#' @param data
#' @param threshold_ndep
#'
#' @keywords internal
plot_all_expo_cumul_ndep <- function(data, threshold_ndep, sub = "im Kanton Zürich") {

  years_exposition <- setNames(unique(data$year), as.character(unique(data$year)))

  plots <- lapply(years_exposition, function(year) {

    ggplot_expo_cumulative(
      data = dplyr::filter(data, year == !!year), x = "ndep_exmax", y = "n_ecosys_cum_rel", linewidth = 1,
      xlims = range(expositionpars("Ndep")$xbreaks), xbreaks = expositionpars("Ndep")$xbreaks, threshold = threshold_ndep,
      xlabel = ggplot2::xlab(expression("max. Stickstoff-Überschuss im Vergleich zu den kritischen Eintragsraten (kgN " * ha^-1 * Jahr^-1 * ")")),
      titlelab = ggplot2::ggtitle(
        label = openair::quickText("Exposition empfindlicher Ökosysteme durch Stickstoffeinträge"),
        subtitle = paste0("relativer Anteil empfindlicher Ökosysteme (kumuliert) ",sub," im Jahr ", year)
      ),
      captionlab = ggplot2::labs(caption = "Daten: BAFU"),
      theme = theme_ts
    )

  })

  return(plots)
}



#' Merge air pollution dataset with corresponding threshold limit values dataset
#'
#' @param data
#' @param threshold_values
#'
#' @keywords internal
combine_thresholds <- function(data, threshold_values) {

  data <-
    threshold_values |>
    dplyr::select(source, pollutant, metric_description, interval, threshold) |>
    dplyr::rename(metric = metric_description) |>
    tidyr::spread(source, threshold) |>
    dplyr::right_join(data, by = c("pollutant", "metric")) |>
    dplyr::select(year, site, pollutant, metric, parameter, interval, unit, concentration, siteclass, `LRV Grenzwert`, `WHO Richtwert`, source)

  return(data)
}



#' Restructures list of plots to a tibble including plots
#'
#' @param plotlist
#' @param type
#' @param source
#'
#' @keywords internal
plotlist_to_tibble <- function(plotlist, type, source) {

  if (!is.na(extract_year(names(plotlist[[1]][1]))) | names(plotlist[[1]][1]) == "alle") {

    plottibble <-
      plotlist |>
      names() |>
      purrr::map(function(x) {
        plotlist[[x]] |>
          tibble::enframe(name = "pollutant", value = "plot") |>
          dplyr::mutate(
            pollutant = x,
            type = !!type,
            source = !!source,
            year = names(plotlist[[x]])
          )
      }) |>
      dplyr::bind_rows()

  } else {

    plottibble <-
      plotlist |>
      tibble::enframe(name = "pollutant", value = "plot") |>
      dplyr::mutate(
        type = !!type,
        source = !!source,
        year = "various"
      )

  }

  return(plottibble)
}



#' Plot pre-compiled relative trends of emissions and immissions vs. a reference year for various pollutants
#'
#' @param data_trends
#' @param detailed
#' @param pt_size
#' @param linewdth
#' @param facet_ncol
#' @param facet_scale
#' @param theme
#' @param titlelab
#' @param captionlab
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
    ggplot2::geom_vline(data = . %>% dplyr::distinct(pollutant, reference_year), mapping = ggplot2::aes(xintercept = reference_year), color = "gray80", linetype = 2)

  if (detailed) {

    plot <-
      plot +
      ggplot2::geom_point(data = . %>% dplyr::filter(type %in% c("Trend pro Standort")), mapping = ggplot2::aes(size = type, shape = type), fill = "white") +
      ggplot2::geom_line(data = . %>% dplyr::filter(type %in% c("Median Messwerte", "Trend pro Standort", "Median Trend", "Emission")), mapping = ggplot2::aes(linewidth = type, group = site))
    # geom_point(mapping = aes(size = n), shape = 21, fill = "white") +
    # scale_size_binned(name = "Anzahl\nMessorte", breaks = c(-Inf,4,6,8,Inf), range = c(0.25,3)) +

  } else {

    plot <-
      plot +
      ggplot2::geom_line(mapping = ggplot2::aes(linewidth = type))

  }

  plot <-
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

  return(plot)
}


#' Extract one plot from a plot tibble
#'
#' @param plots_df Tibble of plots as built by [plotlist_to_tibble()] (e.g. `plots_exposition`).
#' @param filter_expr Filter condition as a string, evaluated on `plots_df`; the first match is returned.
#'
#' @return A ggplot.
#'
#' @keywords internal
get_plot <- function(plots_df, filter_expr = "pollutant == 'NOx' & source == 'inventory_absolute'") {

  plot <-
    plots_df |>
    dplyr::filter(!!rlang::parse_expr(filter_expr)) |>
    dplyr::pull(plot)

  return(plot[[1]])
}


#' Build the code chunk of one tabset panel per year for a Quarto page
#'
#' Adapted from Heiss, Andrew. 2024. "Guide to Generating and Rendering Computational Markdown Content
#' Programmatically with Quarto." https://doi.org/10.59350/pa44j-cc302. The chunk prints
#' `plots$plot[[id]]`, so the page must hold its plot tibble in `plots`.
#'
#' @param id Row number of the plot in `plots`.
#' @param year Year (panel title).
#' @param pollutant,source,type Parts of the chunk label.
#'
#' @return Character, the markdown of the panel.
#'
#' @keywords internal
build_panel <- function(id, year, pollutant, source, type = "exposition") {

  source <- stringr::str_replace(source, "_", "-")
  chunk_label <- glue::glue("{tolower(type)}-{tolower(pollutant)}-{tolower(source)}-{year}")

  output <-
    glue::glue(
      "##### <<year>>
      ```{r}
      #| label: <<chunk_label>>
      plots$plot[[<<id>>]]
      ```", .open = "<<", .close = ">>"
    )

  return(output)
}
