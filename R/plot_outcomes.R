# Plots of the health outcomes page: premature deaths attributable to air pollution.


#' Plot the premature deaths per year and scenario, one plot per parameter
#'
#' Bars per scenario, with the uncertainty range from the lower bound up to the upper bound plus the
#' deaths below the minimum concentration (`outcome_delta_min_conc`). Covid years are drawn transparent.
#'
#' @param data `data_health_outcomes.csv`.
#' @param parameters Parameters to plot.
#' @param relative Deaths per 100'000 inhabitants instead of absolute numbers.
#' @param covid_years Years marked as exceptional (Covid-19).
#' @param theme ggplot2 theme.
#'
#' @return Named list of ggplot objects, one per parameter.
#'
#' @keywords internal
plot_premature_deaths <- function(data, parameters, relative = FALSE, covid_years = 2020:2022,
                                  theme = ggplot2::theme_minimal()) {

  if (relative) {
    mppng <- ggplot2::aes(x = year, y = outcome / population * 10^5, fill = scenario, alpha = covid)
    sub <- "Anzahl vorzeitige Todesfälle pro 100'000 Einwohner/innen pro Jahr"
    uncertainty <- ggplot2::geom_linerange(ggplot2::aes(ymin = outcome_lower / population * 10^5, ymax = outcome_upper / population * 10^5 + outcome_delta_min_conc / population * 10^5), color = "gray20")
  } else {
    mppng <- ggplot2::aes(x = year, y = outcome, fill = scenario, alpha = covid)
    sub <- "Anzahl vorzeitige Todesfälle pro Jahr"
    uncertainty <- ggplot2::geom_linerange(ggplot2::aes(ymin = outcome_lower, ymax = outcome_upper + outcome_delta_min_conc), color = "gray20")
  }

  purrr::map(rlang::set_names(parameters), function(parameter) {

    data |>
      dplyr::filter(parameter == !!parameter & outcome_type == "vorzeitige Todesfälle") |>
      dplyr::mutate(covid = ifelse(year %in% covid_years, "Covid-19", "normal")) |>
      ggplot_timeseries_bars(
        mapping = mppng,
        titlelab = ggplot2::ggtitle(
          label = openair::quickText(paste0("Vorzeitige Todesfälle durch ", airquality.methods::longpollutant(parameter))),
          subtitle = sub
        ),
        captionlab = ggplot2::labs(caption = "Datengrundlage: BAFU & BFS & Statistisches Amt Kanton Zürich"),
        theme = theme
      ) +
      uncertainty +
      ggplot2::scale_alpha_manual(name = "Aussergewöhnliches", values = c("normal" = 1, "Covid-19" = 0.25))
  })
}
