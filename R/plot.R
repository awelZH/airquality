# Building blocks shared by the report pages: bar time series, plot tibbles for the Quarto pages.
# Topic plots live in R/plot_emissions.R, R/plot_monitoring.R and R/plot_exposition.R; the plots of the
# health outcomes and trends below are work in progress and unchanged (see notes/plan_phase2b.md).


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
