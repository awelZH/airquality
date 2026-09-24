# Health outcomes attributable to air pollution: input preparation (deaths, population by age), estimates
# with healthiar and the output of scripts/_compile_outcomes.R.


#' Drop years whose mortality data are not complete yet
#'
#' The mortality of the current year is delivered piece by piece, so its deaths are far below a full
#' year and the estimated premature deaths of that year would be far too low. A year counts as complete
#' when its deaths reach `min_share` of the median year.
#'
#' @param data Deaths per year, e.g. one row per year with the number of deaths.
#' @param min_share Share of the median year a year needs to count as complete.
#' @param year,count Columns holding the year and the number of deaths.
#'
#' @return `data` without the incomplete years; the dropped years are reported.
#'
#' @keywords internal
drop_incomplete_years <- function(data, min_share, year = year, count = deaths) {

  data <- dplyr::mutate(data, .complete = {{ count }} >= min_share * stats::median({{ count }}))
  dropped <- dplyr::pull(dplyr::filter(data, !.data$.complete), {{ year }})

  if (length(dropped) > 0) {
    cli::cli_inform("Dropping {length(dropped)} year{?s} without complete mortality data: {.val {dropped}}.")
  }

  data |>
    dplyr::filter(.data$.complete) |>
    dplyr::select(-".complete")
}


# ---- input preparation ---------------------------------------------------------------

#' Prepare the natural deaths per year, sex and age
#'
#' Keeps the deaths by disease (`tukat == "krankheitsbedingt"`). Cells with fewer than 4 deaths are
#' suppressed (`NA`); category 290 is renamed to 291 and its deaths are spread evenly over the suppressed
#' cells of the same year and sex (behaviour of the former `prepare_mortality()`).
#'
#' @param data Deaths per year (`jahr`), sex (`geschlecht`), age (`alterkat`), cause (`tukat`) and number
#'   (`anzahl`).
#'
#' @return Tibble with `year`, `sex` ("male", "female"), `age` and `deaths`.
#'
#' @keywords internal
prepare_mortality <- function(data) {

  check_columns(data, c("jahr", "geschlecht", "alterkat", "tukat", "anzahl"), "mortality data")

  data <-
    data |>
    dplyr::filter(tukat == "krankheitsbedingt") |>
    dplyr::mutate(alterkat = ifelse(alterkat == 290, 291, alterkat))

  spread <-
    data |>
    dplyr::summarise(
      suppressed = sum(is.na(anzahl) & alterkat != 291),
      remainder = sum(anzahl[alterkat == 291]),
      .by = c(jahr, geschlecht)
    )

  data |>
    dplyr::filter(alterkat != 291) |>
    dplyr::left_join(spread, by = dplyr::join_by(jahr, geschlecht)) |>
    dplyr::mutate(anzahl = ifelse(is.na(anzahl), remainder / suppressed, anzahl)) |>
    dplyr::transmute(
      year = jahr,
      sex = recode_sex(geschlecht),
      age = alterkat,
      deaths = anzahl
    )
}


#' Prepare the population per year, sex and age of the canton
#'
#' @param data Population per municipality (and further groups) with `jahr`, `geschlecht`,
#'   `einjahresaltersklasse` and `anzahl`.
#'
#' @return Tibble with `year`, `sex` ("male", "female"), `age` and `population`.
#'
#' @keywords internal
prepare_population_by_age <- function(data) {

  check_columns(data, c("jahr", "geschlecht", "einjahresaltersklasse", "anzahl"), "population by age")

  data |>
    dplyr::summarise(population = sum(anzahl), .by = c(jahr, geschlecht, einjahresaltersklasse)) |>
    dplyr::transmute(
      year = jahr,
      sex = recode_sex(geschlecht),
      age = einjahresaltersklasse,
      population
    )
}


# German sex labels of the input data to "male" / "female"
recode_sex <- function(sex) {
  dplyr::case_match(sex, c("männlich", "Mann") ~ "male", c("weiblich", "Frau") ~ "female")
}


#' Sum the deaths per year from a minimum age
#'
#' Over the ages of the population: ages without deaths count 1 death, ages without population are
#' dropped (behaviour of the former script).
#'
#' @param mortality Deaths as returned by [prepare_mortality()].
#' @param population Population as returned by [prepare_population_by_age()].
#' @param min_age Youngest age counted.
#'
#' @return Tibble with `year` and `deaths`.
#'
#' @keywords internal
deaths_per_year <- function(mortality, population, min_age) {

  mortality |>
    dplyr::right_join(population, by = dplyr::join_by(year, sex, age)) |>
    dplyr::filter(age >= min_age) |>
    dplyr::mutate(deaths = dplyr::coalesce(deaths, 1)) |>
    dplyr::summarise(deaths = sum(deaths), .by = year)
}


# ---- estimates -----------------------------------------------------------------------

#' Estimate the premature deaths per year and parameter, for the actual exposure and the base year
#'
#' With [healthiar::attribute_health()] (relative risk, cut-off at the lower concentration threshold of
#' the metadata) and the range from [healthiar::summarize_uncertainty()] (Monte Carlo).
#'
#' @param expo Population-weighted means of the canton (`data_exposition_weighted_means_canton.csv`),
#'   one row per year and parameter; a missing base-year mean counts as the actual one.
#' @param deaths Deaths per year as returned by [deaths_per_year()].
#' @param meta Outcome metadata, one row per parameter (`crf`, `crf_lower`, `crf_upper`,
#'   `crf_conc_increment`, `lower_conc_threshold`).
#' @param erf_shape Shape of the exposure-response function.
#' @param n_sim Number of Monte Carlo simulations.
#'
#' @return Tibble with `year`, `parameter`, `scenario` ("actual", "base"), `outcome`, `outcome_lower` and
#'   `outcome_upper`, for the years and parameters in all three inputs.
#'
#' @keywords internal
estimate_premature_deaths <- function(expo, deaths, meta, erf_shape = "log_linear", n_sim = 500) {

  cases <-
    expo |>
    dplyr::select(year, parameter, exp_actual = population_weighted_mean, exp_base = population_weighted_mean_base) |>
    dplyr::inner_join(deaths, by = dplyr::join_by(year)) |>
    dplyr::inner_join(meta, by = dplyr::join_by(parameter)) |>
    dplyr::mutate(exp_base = dplyr::coalesce(exp_base, exp_actual))

  purrr::map(seq_len(nrow(cases)), function(i) {
    case <- cases[i, ]
    impact <- healthiar::attribute_health(
      geo_id_micro = c("actual", "base"),
      approach_risk = "relative_risk",
      exp_central = c(case$exp_actual, case$exp_base),
      erf_shape = erf_shape,
      rr_central = case$crf, rr_lower = case$crf_lower, rr_upper = case$crf_upper,
      rr_increment = case$crf_conc_increment,
      cutoff_central = case$lower_conc_threshold,
      bhd_central = case$deaths
    )
    healthiar::summarize_uncertainty(impact, n_sim = n_sim)$uncertainty_main |>
      dplyr::select(scenario = geo_id_micro, estimate = impact_ci, impact) |>
      tidyr::pivot_wider(names_from = estimate, values_from = impact) |>
      dplyr::transmute(
        year = case$year, parameter = case$parameter, scenario,
        outcome = central_estimate, outcome_lower = lower_estimate, outcome_upper = upper_estimate
      )
  }) |>
    purrr::list_rbind()
}


#' Shape estimates into the output: actual burden and burden avoided vs. the base year
#'
#' The avoided burden is actual minus base where it is negative (central value and each bound on its
#' own); without reduction the central value is 0 and the bounds are `NA`.
#'
#' @param estimates Estimates with `year`, `parameter`, `scenario` ("actual", "base"), `outcome`,
#'   `outcome_lower`, `outcome_upper`.
#' @param expo Population-weighted means of the canton (pollutant, metric, population, base year).
#' @param outcome_type Label of the outcome, e.g. "vorzeitige Todesfälle".
#'
#' @return Tibble in the schema of `data_health_outcomes.csv`.
#'
#' @keywords internal
outcome_scenarios <- function(estimates, expo, outcome_type) {

  avoided <- function(actual, base) ifelse(actual - base < 0, actual - base, NA)

  wide <- tidyr::pivot_wider(estimates, names_from = scenario, values_from = c(outcome, outcome_lower, outcome_upper))

  scenarios <-
    dplyr::bind_rows(
      dplyr::transmute(wide, year, parameter, scenario = "actual",
                       outcome = outcome_actual, outcome_lower = outcome_lower_actual, outcome_upper = outcome_upper_actual),
      dplyr::transmute(wide, year, parameter, scenario = "avoided",
                       outcome = dplyr::coalesce(avoided(outcome_actual, outcome_base), 0),
                       outcome_lower = avoided(outcome_lower_actual, outcome_lower_base),
                       outcome_upper = avoided(outcome_upper_actual, outcome_upper_base))
    )

  expo |>
    dplyr::mutate(base_year = ifelse(is.na(base_year) & is.na(population_weighted_mean_base), year, base_year)) |>
    dplyr::select(year, pollutant, metric, parameter, base_year, population) |>
    dplyr::inner_join(scenarios, by = dplyr::join_by(year, parameter), relationship = "one-to-many") |>
    dplyr::mutate(
      scenario = ifelse(scenario == "actual", "tatsächliche Belastung", paste0("vermieden vs. ", base_year)),
      outcome_type = !!outcome_type,
      outcome_delta_min_conc = 0 # prepared (burden between the lowest concentration and the cut-off), not computed
    ) |>
    dplyr::select(year, pollutant, metric, parameter, outcome_type, population, scenario, outcome, outcome_lower,
                  outcome_upper, outcome_delta_min_conc)
}
