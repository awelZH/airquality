# Plots of the health outcomes page: premature deaths and years of life lost attributable to air pollution.


# labels per outcome type: start of the title, the counted unit in the subtitle and whether the subtitle
# gives the long-term mean of the years of life lost per premature death
outcome_labels <- list(
  "vorzeitige Todesfälle" = list(title = "Vorzeitige Todesfälle durch ", unit = "vorzeitige Todesfälle", per_death = FALSE),
  "verlorene Lebensjahre" = list(title = "Verlorene Lebensjahre in der Bevölkerung durch ", unit = "verlorene Lebensjahre", per_death = TRUE)
)


#' Plot a health outcome per year and scenario, one plot per parameter
#'
#' Bars per scenario, with the uncertainty range from the lower bound up to the upper bound plus the
#' outcome below the minimum concentration (`outcome_delta_min_conc`). Covid years are drawn transparent.
#' The years of life lost give in a second subtitle line the long-term mean of the years lost per
#' premature death ([mean_life_years_per_death()], rounded to whole years).
#'
#' @param data `data_health_outcomes.csv`.
#' @param parameters Parameters to plot.
#' @param outcome_type Outcome type, "vorzeitige Todesfälle" or "verlorene Lebensjahre".
#' @param relative Outcome per 100'000 inhabitants instead of absolute numbers.
#' @param covid_years Years marked as exceptional (Covid-19).
#' @param uncertainty_scenario Scenario the uncertainty ranges belong to; the avoided outcome gets none.
#' @param ylabels Labels of the y axis, e.g. [label_big_mark()].
#' @param theme ggplot2 theme.
#'
#' @return Named list of ggplot objects, one per parameter. Stops with an error of class
#'   `airquality_plot_error` for an unknown outcome type.
#'
#' @keywords internal
plot_health_outcomes <- function(data, parameters, outcome_type = "vorzeitige Todesfälle", relative = FALSE,
                                 covid_years = 2020:2022, uncertainty_scenario = "tatsächliche Belastung",
                                 ylabels = ggplot2::waiver(), theme = ggplot2::theme_minimal()) {

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

  per_death <- if (labels$per_death) mean_life_years_per_death(data, scenario = uncertainty_scenario)

  purrr::map(rlang::set_names(parameters), function(parameter) {

    if (labels$per_death && !is.na(per_death[parameter])) {
      sub <- paste0(sub, "\n(Langzeit-Mittel: ", round(per_death[[parameter]]), " verlorene Lebensjahre pro vorzeitigem Todesfall)")
    }

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
        ylabels = ylabels,
        theme = theme
      ) +
      uncertainty +
      ggplot2::scale_alpha_manual(name = "Aussergewöhnliches", values = c("normal" = 1, "Covid-19" = 0.25))
  })
}


#' Long-term mean of the years of life lost per premature death, per parameter
#'
#' The mean over the years of the yearly ratio years of life lost / premature deaths, for one scenario
#' (central estimates).
#'
#' @param data `data_health_outcomes.csv`.
#' @param scenario Scenario of the ratio.
#'
#' @return Named numeric vector, one mean per parameter with both outcomes.
#'
#' @keywords internal
mean_life_years_per_death <- function(data, scenario = "tatsächliche Belastung") {

  ratio <-
    data |>
    dplyr::filter(scenario == !!scenario, outcome_type %in% c("verlorene Lebensjahre", "vorzeitige Todesfälle")) |>
    dplyr::select(year, parameter, outcome_type, outcome) |>
    tidyr::pivot_wider(names_from = outcome_type, values_from = outcome) |>
    dplyr::filter(!is.na(`verlorene Lebensjahre`), !is.na(`vorzeitige Todesfälle`)) |>
    dplyr::summarise(ratio = mean(`verlorene Lebensjahre` / `vorzeitige Todesfälle`), .by = parameter)

  rlang::set_names(ratio$ratio, ratio$parameter)
}
