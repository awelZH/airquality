# Building blocks shared by the report pages: bar time series, plot tibbles for the Quarto pages.
# Topic plots live in R/plot_<topic>.R (emissions, monitoring, exposition, outcomes, trends).


#' Plot a yearly time series as bars, e.g. population-weighted means or health outcomes
#'
#' @param data Data with the columns used in `mapping`.
#' @param mapping Aesthetic mapping (x = year, y = value, fill = scenario).
#' @param ylims,ybreaks Limits and breaks of the y axis.
#' @param titlelab,captionlab Title and caption, e.g. `ggplot2::ggtitle()` and `ggplot2::labs(caption = )`.
#' @param theme ggplot2 theme.
#'
#' @return A ggplot object.
#'
#' @keywords internal
ggplot_timeseries_bars <- function(data, mapping = ggplot2::aes(x = year, y = population_weighted_mean, fill = scenario), ylims = c(NA,NA),
                                   ybreaks = ggplot2::waiver(), titlelab = NULL, captionlab = NULL, theme = ggplot2::theme_minimal()) {

  plot <-
    ggplot2::ggplot(data, mapping = mapping) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_hline(yintercept = 0, color = "gray30", linetype = 2) +
    ggplot2::scale_x_continuous(breaks = seq(1990,2100,5), expand = c(0.01,0.01)) +
    ggplot2::scale_y_continuous(limits = ylims, breaks = ybreaks, expand = c(0.01,0.01)) +
    ggplot2::scale_fill_manual(name = "Szenario", values = c("#50586C", "#DCE2F0")) +
    titlelab +
    captionlab +
    theme

  # plot <- ggiraph::girafe(ggobj = plot, width_svg = 6, height_svg = 4)

  return(plot)
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


#' Build the code chunk of one tabset panel per year for a Quarto page
#'
#' Adapted from Heiss, Andrew. 2024. "Guide to Generating and Rendering Computational Markdown Content
#' Programmatically with Quarto." https://doi.org/10.59350/pa44j-cc302. The chunk prints
#' `plots$figure[[id]]`, so the page must hold its plot catalog in `plots`.
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
      plots$figure[[<<id>>]]
      ```", .open = "<<", .close = ">>"
    )

  return(output)
}
