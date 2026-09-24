# Building blocks shared by the report pages: bar time series, plot catalog, year slider and tabsets.
# Topic plots live in R/plot_<topic>.R (emissions, monitoring, exposition, outcomes, trends).


#' Plot a yearly time series as bars, e.g. population-weighted means or health outcomes
#'
#' @param data Data with the columns used in `mapping`.
#' @param mapping Aesthetic mapping (x = year, y = value, fill = scenario).
#' @param ylims,ybreaks,ylabels Limits, breaks and labels of the y axis.
#' @param titlelab,captionlab Title and caption, e.g. `ggplot2::ggtitle()` and `ggplot2::labs(caption = )`.
#' @param theme ggplot2 theme.
#'
#' @return A ggplot object.
#'
#' @keywords internal
ggplot_timeseries_bars <- function(data, mapping = ggplot2::aes(x = year, y = population_weighted_mean, fill = scenario), ylims = c(NA,NA),
                                   ybreaks = ggplot2::waiver(), ylabels = ggplot2::waiver(), titlelab = NULL, captionlab = NULL,
                                   theme = ggplot2::theme_minimal()) {

  plot <-
    ggplot2::ggplot(data, mapping = mapping) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_hline(yintercept = 0, color = "gray30", linetype = 2) +
    ggplot2::scale_x_continuous(breaks = seq(1990,2100,5), expand = c(0.01,0.01)) +
    ggplot2::scale_y_continuous(limits = ylims, breaks = ybreaks, labels = ylabels, expand = c(0.01,0.01)) +
    ggplot2::scale_fill_manual(name = "Szenario", values = c("#50586C", "#DCE2F0")) +
    titlelab +
    captionlab +
    theme

  return(plot)
}


#' Axis labels with an apostrophe as thousands separator
#'
#' @param x Numbers.
#'
#' @return Character vector, e.g. "25'000".
#'
#' @keywords internal
label_big_mark <- function(x) {
  format(x, big.mark = "'", scientific = FALSE, trim = TRUE)
}


#' Collect plots in a catalog for the Quarto pages
#'
#' The caller says what the names of the list mean, so the catalog does not guess its structure.
#'
#' @param figures A ggplot, a list of ggplots named by parameter or year, or a list of such lists
#'   (names of the outer list = parameter, of the inner lists = year).
#' @param plot Name of the plot, e.g. "distribution_histogram".
#' @param names_to What the names of the list levels are: `character()` for a single plot, `"parameter"`,
#'   `"year"` or `c("parameter", "year")`.
#'
#' @return Tibble with `plot`, `parameter` and `year` (character, `NA` if not applicable) and the list
#'   column `figure`, one row per plot.
#'
#' @keywords internal
plot_catalog <- function(figures, plot, names_to = character()) {

  if (length(names_to) == 0) {
    return(tibble::tibble(plot = plot, parameter = NA_character_, year = NA_character_, figure = list(figures)))
  }
  if (!rlang::is_named(figures)) {
    cli::cli_abort("{.arg figures} of {.val {plot}} must be a named list (names = {names_to[1]}).")
  }

  figures |>
    purrr::imap(\(figure, name) {
      plot_catalog(figure, plot, names_to[-1]) |>
        dplyr::mutate("{names_to[1]}" := name)
    }) |>
    purrr::list_rbind()
}


#' Rows of a plot catalog matching plot, parameter and year
#'
#' @param catalog Plot catalog as built by [plot_catalog()].
#' @param plot Name of the plot.
#' @param parameter,year Parameter and year; `NULL` to keep all.
#'
#' @return The matching rows. Stops with an error of class `airquality_plot_error` if there are none,
#'   naming the available plots.
#'
#' @keywords internal
catalog_entries <- function(catalog, plot, parameter = NULL, year = NULL) {

  match <- catalog$plot == plot
  if (!is.null(parameter)) match <- match & catalog$parameter %in% parameter
  if (!is.null(year)) match <- match & catalog$year %in% as.character(year)

  if (!any(match)) abort_plot_match(catalog, plot, parameter, year, 0)
  catalog[match, ]
}


#' Get one plot from a plot catalog
#'
#' @inheritParams catalog_entries
#' @param parameter,year Parameter and year of the plot; `NULL` if the plot has none.
#'
#' @return A ggplot. Stops with an error of class `airquality_plot_error` unless exactly one plot matches.
#'
#' @keywords internal
get_plot <- function(catalog, plot, parameter = NULL, year = NULL) {

  entries <- catalog_entries(catalog, plot, parameter, year)
  if (nrow(entries) != 1) abort_plot_match(catalog, plot, parameter, year, nrow(entries))

  entries$figure[[1]]
}


# error for get_plot() and catalog_entries(): how many plots match, and which ones exist
abort_plot_match <- function(catalog, plot, parameter, year, n) {
  available <- catalog[catalog$plot == plot, ]
  wanted <- paste(c(plot, parameter, year), collapse = " / ")
  cli::cli_abort(
    c(
      "{n} plot{?s} match {.val {wanted}}, expected exactly one.",
      i = if (nrow(available) == 0) {
        "Available plots: {.val {unique(catalog$plot)}}."
      } else {
        "Available for {.val {plot}} (parameter / year): {.val {unique(paste(available$parameter, available$year, sep = ' / '))}}."
      }
    ),
    class = "airquality_plot_error",
    call = rlang::caller_env(2)
  )
}


#' Print the plots of one parameter per year as a year slider (Quarto page)
#'
#' For a chunk with `#| output: asis`. Writes a `.year-slider` div with one `.year-panel` div per year and
#' prints each plot as an ordinary figure into it; `report/year-slider.html` turns the panels into a slider,
#' starting at the last year. A plot of all years (`year == "alle"`) is not part of the slider: it gets a
#' tab of its own, next to a tab with the slider. Years are sorted by their last year, so labels of year
#' ranges ("2023–2025") work as well.
#'
#' @param catalog Plot catalog as built by [plot_catalog()].
#' @param plot Name of the plot.
#' @param parameter Parameter; `NULL` if the plot has none.
#'
#' @return `NULL`, invisibly; called for its output.
#'
#' @keywords internal
print_year_slider <- function(catalog, plot, parameter = NULL) {

  entries <- catalog_entries(catalog, plot, parameter)
  if (anyNA(entries$year)) cli::cli_abort("Plot {.val {plot}} has entries without year.", class = "airquality_plot_error")

  all_years <- entries[entries$year == "alle", ]
  entries <- entries[entries$year != "alle", ]
  entries <- entries[order(as.numeric(stringr::str_extract(entries$year, "[0-9]{4}$"))), ]

  if (nrow(all_years) == 0) return(print_slider_panels(entries))

  print_tabset(list(
    "alle Jahre" = all_years$figure[[1]],
    "einzelne Jahre" = \() print_slider_panels(entries)
  ))
}


# the panels of one slider: one .year-panel per year inside a .year-slider, starting at the last year
print_slider_panels <- function(entries) {

  cat("\n\n::: {.year-slider data-start=\"", entries$year[nrow(entries)], "\"}\n\n", sep = "")
  purrr::walk2(entries$year, entries$figure, \(year, figure) {
    cat("::: {.year-panel data-year=\"", year, "\"}\n\n", sep = "")
    print(figure)
    cat("\n\n:::\n\n")
  })
  cat(":::\n\n")

  invisible(NULL)
}


#' Print plots as a tabset (Quarto page)
#'
#' For a chunk with `#| output: asis`.
#'
#' @param figures Named list of ggplots, or of functions printing the content of a tab (e.g. a slider);
#'   the names are the tab titles.
#'
#' @return `NULL`, invisibly; called for its output.
#'
#' @keywords internal
print_tabset <- function(figures) {

  cat("\n\n::: {.panel-tabset}\n\n")
  purrr::iwalk(figures, \(figure, title) {
    cat("##### ", title, "\n\n", sep = "")
    if (is.function(figure)) figure() else print(figure)
    cat("\n\n")
  })
  cat(":::\n\n")

  invisible(NULL)
}


#' Setting of one parameter from a list of settings per parameter
#'
#' @param settings Named list, one element per parameter (e.g. `plot_axes_timeseries` of
#'   `report/plots/_plot_setup.R`).
#' @param parameter Parameter.
#'
#' @return The element of `parameter`. Stops with an error of class `airquality_plot_error` if there is
#'   none, naming the parameters with a setting.
#'
#' @keywords internal
parameter_setting <- function(settings, parameter) {
  if (!parameter %in% names(settings)) {
    cli::cli_abort(
      "No plot setting for parameter {.val {parameter}}; settings exist for {.val {names(settings)}}.",
      class = "airquality_plot_error"
    )
  }
  settings[[parameter]]
}


#' Add threshold lines to a plot, named in the legend
#'
#' The lines are drawn from their labels, so ggplot2 puts them into the legend (line type per label,
#' colours through `override.aes`) instead of writing the labels into the panel.
#'
#' @param plot A ggplot object.
#' @param threshold Threshold lines as returned by [extract_threshold()] (`value`, `labels`, `color`,
#'   `linetype`, `linesize`); nothing is added for `value = NA`.
#' @param direction Whether the lines are vertical (value on the x axis) or horizontal.
#' @param legend_position Position of this legend; the other legends of the plot keep theirs.
#' @param legend_title Title of this legend; `NULL` where it is the only legend of the plot and the
#'   labels speak for themselves.
#'
#' @return The plot.
#'
#' @keywords internal
add_threshold_lines <- function(plot, threshold, direction = c("vertical", "horizontal"), legend_position = "bottom",
                                legend_title = "Referenz") {

  direction <- rlang::arg_match(direction)
  if (is.na(sum(threshold$value))) return(plot)

  lines <- tibble::tibble(label = factor(threshold$labels, levels = threshold$labels), value = threshold$value)
  line <- if (direction == "vertical") {
    ggplot2::geom_vline(data = lines, mapping = ggplot2::aes(xintercept = value, linetype = label),
                        color = threshold$color, linewidth = threshold$linesize)
  } else {
    ggplot2::geom_hline(data = lines, mapping = ggplot2::aes(yintercept = value, linetype = label),
                        color = threshold$color, linewidth = threshold$linesize)
  }

  plot +
    line +
    threshold_legend(threshold$labels, threshold$color, threshold$linetype, legend_position, legend_title)
}


#' Legend of the threshold lines
#'
#' The line type carries the label, so the legend names the thresholds; the colours come back through
#' `override.aes`, because the colour aesthetic usually belongs to the data of the plot.
#'
#' @param labels,colours,linetypes Label, colour and line type per threshold.
#' @param position Position of this legend; the other legends of the plot keep theirs.
#' @param title Title of this legend; `NULL` for none, which also moves the legend closer to the axis.
#'
#' @return A list of ggplot2 scales and guides, to be added to a plot.
#'
#' @keywords internal
threshold_legend <- function(labels, colours, linetypes, position = "bottom", title = "Referenz") {
  list(
    ggplot2::scale_linetype_manual(name = title, values = rlang::set_names(linetypes, labels)),
    # the title comes with this guide, also in plots whose theme hides the legend titles
    ggplot2::guides(linetype = ggplot2::guide_legend(
      position = position,
      override.aes = list(color = colours),
      theme = ggplot2::theme(legend.title = if (is.null(title)) ggplot2::element_blank() else ggplot2::element_text())
    )),
    if (is.null(title)) threshold_legend_spacing()
  )
}


#' Space between the axis title and a threshold legend without title
#'
#' Add it after a complete theme, which would drop the setting again.
#'
#' @return A ggplot2 theme.
#'
#' @keywords internal
threshold_legend_spacing <- function() {
  ggplot2::theme(legend.box.spacing = ggplot2::unit(4, "pt"))
}
