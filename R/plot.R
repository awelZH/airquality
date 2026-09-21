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


#' Restructure a list of plots into a tibble of plots for the Quarto pages
#'
#' @param plotlist Named list of plots (one per pollutant), or a named list of named lists (one plot per
#'   pollutant and year, or "alle").
#' @param type,source Values of the columns `type` and `source`.
#'
#' @return Tibble with `pollutant`, `plot`, `type`, `source` and `year` ("various" for a flat list).
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
