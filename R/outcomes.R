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
#' Keeps the deaths by disease (`tukat == "krankheitsbedingt"`) of the single ages; category 290 (the
#' deaths aged 0-29, summed) is dropped. Cells with fewer than 4 deaths are suppressed (`NA`) and get
#' `suppressed` deaths.
#'
#' @param data Deaths per year (`jahr`), sex (`geschlecht`), age (`alterkat`), cause (`tukat`) and number
#'   (`anzahl`).
#' @param suppressed Deaths assumed in a suppressed cell (1 to 3 deaths).
#'
#' @return Tibble with `year`, `sex` ("male", "female"), `age` and `deaths`.
#'
#' @keywords internal
prepare_mortality <- function(data, suppressed) {

  check_columns(data, c("jahr", "geschlecht", "alterkat", "tukat", "anzahl"), "mortality data")

  data |>
    dplyr::filter(tukat == "krankheitsbedingt", alterkat != 290) |>
    dplyr::mutate(anzahl = dplyr::coalesce(anzahl, suppressed)) |>
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
  dplyr::case_when(sex %in% c("männlich", "Mann") ~ "male", sex %in% c("weiblich", "Frau") ~ "female")
}


#' Sum the deaths per year from a minimum age
#'
#' @param mortality Deaths as returned by [prepare_mortality()].
#' @param min_age Youngest age counted.
#'
#' @return Tibble with `year` and `deaths`.
#'
#' @keywords internal
deaths_per_year <- function(mortality, min_age) {

  mortality |>
    dplyr::filter(age >= min_age) |>
    dplyr::summarise(deaths = sum(deaths), .by = year)
}


# ---- estimates -----------------------------------------------------------------------

#' Estimate the premature deaths per year and parameter, for the actual exposure and the base year
#'
#' With [healthiar::attribute_health()] (relative risk, cut-off at the lower concentration threshold of
#' the metadata). Deterministic: the estimate with the central relative risk, the range with its lower
#' and upper 95 % bound (the only uncertain input).
#'
#' @param expo Population-weighted means of the canton (`data_exposition_weighted_means_canton.csv`),
#'   one row per year and parameter; a missing base-year mean counts as the actual one.
#' @param deaths Deaths per year as returned by [deaths_per_year()].
#' @param meta Outcome metadata, one row per parameter (`crf`, `crf_lower`, `crf_upper`,
#'   `crf_conc_increment`, `lower_conc_threshold`).
#' @param erf_shape Shape of the exposure-response function.
#'
#' @return Tibble with `year`, `parameter`, `scenario` ("actual", "base"), `outcome`, `outcome_lower` and
#'   `outcome_upper`, for the years and parameters in all three inputs.
#'
#' @keywords internal
estimate_premature_deaths <- function(expo, deaths, meta, erf_shape = "log_linear") {

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
    impact_by_scenario(impact$health_main) |>
      dplyr::mutate(year = case$year, parameter = case$parameter, .before = 1)
  }) |>
    purrr::list_rbind()
}


# the central impact and its range per scenario (geo_id_micro) from the main results of healthiar
impact_by_scenario <- function(health_main) {
  health_main |>
    dplyr::select(scenario = geo_id_micro, erf_ci, impact) |>
    tidyr::pivot_wider(names_from = erf_ci, values_from = impact) |>
    dplyr::select(scenario, outcome = central, outcome_lower = lower, outcome_upper = upper)
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


# ---- years of life lost --------------------------------------------------------------

#' Prepare the life tables: deaths and mid-year population per year, sex and single age
#'
#' The mid-year population is the mean of the year-end populations of the previous and the same year;
#' the first year, without previous year, takes its year-end population. Ages from `max_age` are
#' condensed into the last age group, as the life table is closed there. Ages without deaths get 0.
#'
#' @param mortality Deaths as returned by [prepare_mortality()].
#' @param population Year-end population as returned by [prepare_population_by_age()].
#' @param min_age Youngest age of the life table.
#' @param max_age Last age group (this age and older).
#'
#' @return Tibble with `year`, `sex`, `age`, `deaths` and `population`, for the years of the population.
#'   Stops with an error of class `airquality_input_error` if an age has more deaths than inhabitants.
#'
#' @keywords internal
lifetable_data <- function(mortality, population, min_age, max_age) {

  condense <- function(data, value) {
    data |>
      dplyr::filter(age >= min_age) |>
      dplyr::mutate(age = pmin(age, max_age)) |>
      dplyr::summarise("{value}" := sum(.data[[value]]), .by = c(year, sex, age))
  }

  population <- condense(population, "population")
  midyear <-
    population |>
    dplyr::left_join(dplyr::mutate(population, year = year + 1), by = dplyr::join_by(year, sex, age), suffix = c("", "_previous")) |>
    dplyr::mutate(population = ifelse(is.na(population_previous), population, (population + population_previous) / 2)) |>
    dplyr::select(-population_previous)

  result <-
    midyear |>
    dplyr::left_join(condense(mortality, "deaths"), by = dplyr::join_by(year, sex, age)) |>
    dplyr::mutate(deaths = dplyr::coalesce(deaths, 0)) |>
    dplyr::select(year, sex, age, deaths, population) |>
    dplyr::arrange(year, sex, age)

  implausible <- dplyr::filter(result, deaths > population)
  if (nrow(implausible) > 0) {
    cli::cli_abort("{nrow(implausible)} age group{?s} of the life tables {?has/have} more deaths than inhabitants, e.g. {implausible$year[1]} {implausible$sex[1]} {implausible$age[1]}.",
                   class = "airquality_input_error")
  }
  result
}


#' Estimate the years of life lost per year and parameter, for the actual exposure and the base year
#'
#' With the life table of [healthiar::attribute_lifetable()]: the years of life lost by the exposure of
#' one year (`approach_exposure = "single_year"`, without newborns) from `min_age`, summed over the
#' sexes. Deterministic like [estimate_premature_deaths()]: the range from the 95 % bounds of the
#' relative risk.
#'
#' @inheritParams estimate_premature_deaths
#' @param lifetable Life tables as returned by [lifetable_data()].
#' @param min_age Youngest age affected by the exposure.
#' @param max_age Last age group of the life tables.
#'
#' @return Tibble with `year`, `parameter`, `scenario` ("actual", "base"), `outcome`, `outcome_lower` and
#'   `outcome_upper`, for the years and parameters in all three inputs.
#'
#' @keywords internal
estimate_life_years_lost <- function(expo, lifetable, meta, min_age, max_age, erf_shape = "log_linear") {

  cases <-
    expo |>
    dplyr::select(year, parameter, exp_actual = population_weighted_mean, exp_base = population_weighted_mean_base) |>
    dplyr::semi_join(lifetable, by = dplyr::join_by(year)) |>
    dplyr::inner_join(meta, by = dplyr::join_by(parameter)) |>
    dplyr::mutate(exp_base = dplyr::coalesce(exp_base, exp_actual))

  purrr::map(seq_len(nrow(cases)), function(i) {
    case <- cases[i, ]
    table <- dplyr::filter(lifetable, year == case$year)
    yll <- function(exposure) {
      # zero deaths occur at single young ages; healthiar warns but computes them correctly
      withCallingHandlers(
        healthiar::attribute_lifetable(
          health_outcome = "yll", approach_exposure = "single_year", approach_newborns = "without_newborns",
          age_group = table$age, sex = table$sex, bhd_central = table$deaths, population = table$population,
          year_of_analysis = case$year, min_age = min_age, max_age = max_age,
          exp_central = exposure, cutoff_central = case$lower_conc_threshold, erf_shape = erf_shape,
          rr_central = case$crf, rr_lower = case$crf_lower, rr_upper = case$crf_upper,
          rr_increment = case$crf_conc_increment
        )$health_main,
        warning = function(w) if (grepl("Zeros in bhd_", conditionMessage(w))) invokeRestart("muffleWarning")
      )
    }
    dplyr::bind_rows(
      dplyr::mutate(yll(case$exp_actual), geo_id_micro = "actual"),
      dplyr::mutate(yll(case$exp_base), geo_id_micro = "base")
    ) |>
      impact_by_scenario() |>
      dplyr::mutate(year = case$year, parameter = case$parameter, .before = 1)
  }) |>
    purrr::list_rbind()
}
