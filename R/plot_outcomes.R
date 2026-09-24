# Plots of the health outcomes page: premature deaths and years of life lost attributable to air pollution.


# labels per outcome type: start of the title and the counted unit in the subtitle
outcome_labels <- list(
  "vorzeitige Todesfälle" = list(title = "Vorzeitige Todesfälle durch ", unit = "vorzeitige Todesfälle"),
  "verlorene Lebensjahre" = list(title = "Verlorene Lebensjahre durch ", unit = "verlorene Lebensjahre")
)


#' Plot a health outcome per year and scenario, one plot per parameter
#'
#' Bars per scenario, with the uncertainty range from the lower bound up to the upper bound plus the
#' outcome below the minimum concentration (`outcome_delta_min_conc`). Covid years are drawn transparent.
#'
#' @param data `data_health_outcomes.csv`.
#' @param parameters Parameters to plot.
#' @param outcome_type Outcome type, "vorzeitige Todesfälle" or "verlorene Lebensjahre".
#' @param relative Outcome per 100'000 inhabitants instead of absolute numbers.
#' @param covid_years Years marked as exceptional (Covid-19).
#' @param uncertainty_scenario Scenario the uncertainty ranges belong to; the avoided outcome gets none.
#' @param theme ggplot2 theme.
#'
#' @return Named list of ggplot objects, one per parameter. Stops with an error of class
#'   `airquality_plot_error` for an unknown outcome type.
#'
#' @keywords internal
plot_health_outcomes <- function(data, parameters, outcome_type = "vorzeitige Todesfälle", relative = FALSE,
                                 covid_years = 2020:2022, uncertainty_scenario = "tatsächliche Belastung",
                                 theme = ggplot2::theme_minimal()) {

  labels <- outcome_labels[[outcome_type]]
  if (is.null(labels)) {
    cli::cli_abort("Unknown outcome type {.val {outcome_type}}; known: {.val {names(outcome_labels)}}.", class = "airquality_plot_error")
  }

  actual <- function(data) dplyr::filter(data, scenario == uncertainty_scenario)

  if (relative) {
    mppng <- ggplot2::aes(x = year, y = outcome / population * 10^5, fill = scenario, alpha = covid)
    sub <- paste0("Anzahl ", labels$unit, " pro 100'000 Einwohner/innen pro Jahr")
    uncertainty <- ggplot2::geom_linerange(data = actual, ggplot2::aes(ymin = outcome_lower / population * 10^5, ymax = outcome_upper / population * 10^5 + outcome_delta_min_conc / population * 10^5), color = "gray20")
  } else {
    mppng <- ggplot2::aes(x = year, y = outcome, fill = scenario, alpha = covid)
    sub <- paste0("Anzahl ", labels$unit, " pro Jahr")
    uncertainty <- ggplot2::geom_linerange(data = actual, ggplot2::aes(ymin = outcome_lower, ymax = outcome_upper + outcome_delta_min_conc), color = "gray20")
  }

  purrr::map(rlang::set_names(parameters), function(parameter) {

    data |>
      dplyr::filter(parameter == !!parameter & outcome_type == !!outcome_type) |>
      dplyr::mutate(covid = ifelse(year %in% covid_years, "Covid-19", "normal")) |>
      ggplot_timeseries_bars(
        mapping = mppng,
        titlelab = ggplot2::ggtitle(
          label = openair::quickText(paste0(labels$title, airquality.methods::longpollutant(parameter))),
          subtitle = sub
        ),
        captionlab = ggplot2::labs(caption = "Datengrundlage: BAFU & BFS & Statistisches Amt Kanton Zürich"),
        theme = theme
      ) +
      uncertainty +
      ggplot2::scale_alpha_manual(name = "Aussergewöhnliches", values = c("normal" = 1, "Covid-19" = 0.25))
  })
}


#' Plot the years of life lost per premature death, one plot per parameter
#'
#' For the actual exposure: the years of life lost divided by the premature deaths of the same year
#' (central estimates; the ratio hardly depends on the bound of the relative risk, so no range).
#'
#' @inheritParams plot_health_outcomes
#' @param scenario Scenario of the ratio.
#'
#' @return Named list of ggplot objects, one per parameter.
#'
#' @keywords internal
plot_life_years_per_death <- function(data, parameters, covid_years = 2020:2022, scenario = "tatsächliche Belastung",
                                      theme = ggplot2::theme_minimal()) {

  ratio <-
    data |>
    dplyr::filter(scenario == !!scenario, outcome_type %in% names(outcome_labels)) |>
    dplyr::select(year, parameter, scenario, outcome_type, outcome) |>
    tidyr::pivot_wider(names_from = outcome_type, values_from = outcome) |>
    dplyr::filter(!is.na(`verlorene Lebensjahre`), !is.na(`vorzeitige Todesfälle`)) |>
    dplyr::mutate(
      life_years_per_death = `verlorene Lebensjahre` / `vorzeitige Todesfälle`,
      covid = ifelse(year %in% covid_years, "Covid-19", "normal")
    )

  purrr::map(rlang::set_names(parameters), function(parameter) {

    dplyr::filter(ratio, parameter == !!parameter) |>
      ggplot_timeseries_bars(
        mapping = ggplot2::aes(x = year, y = life_years_per_death, fill = scenario, alpha = covid),
        titlelab = ggplot2::ggtitle(
          label = openair::quickText(paste0("Verlorene Lebensjahre pro Todesfall durch ", airquality.methods::longpollutant(parameter))),
          subtitle = "verlorene Lebensjahre pro vorzeitigen Todesfall"
        ),
        captionlab = ggplot2::labs(caption = "Datengrundlage: BAFU & BFS & Statistisches Amt Kanton Zürich"),
        theme = theme
      ) +
      ggplot2::scale_alpha_manual(name = "Aussergewöhnliches", values = c("normal" = 1, "Covid-19" = 0.25)) +
      ggplot2::guides(fill = "none") # one scenario only
  })
}
