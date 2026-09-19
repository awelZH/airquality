#' calculate specific health outcome
#'
#' @param conc_increment
#' @param crf_per_concunit
#' @param deathrate_per_person
#' @param population
#'
#' @keywords internal
calc_outcome <- function(conc_increment, crf, crf_conc_increment, cases) {

  # see:
  # Castro, A., Kutlar Joss, M., Röösli, M. (2023). Quantifizierung des Gesundheitsnutzens der neuen
  # Luftqualitätsleitlinien der Weltgesundheitsorganisation in der Schweiz. Im Auftrag vom Bundesamt für Umwelt.

  CB <- conc_increment
  C0 <- 0 # set to 0 here since concentration increment is already directly provided by function input
  CA <- crf_conc_increment
  EEA <- crf
  GD <- cases

  EEB <- exp(log(EEA) * (CB - C0) / CA)

  A <- GD * (1 - 1 / EEB)

  return(A)
}


#' Calculate range of health outcomes from input-dataset (most likely value, lower and upper confidence intervals crf)
#'
#' @param data
#' @param conc_threshold
#'
#' @keywords internal
calculate_all_outcomes <- function(data, conc_threshold = "lower_conc_threshold") {

  data <-
    data |>
    dplyr::mutate(
      conc_incr = pmax(0, population_weighted_mean - !!rlang::sym(conc_threshold)),
      outcome = calc_outcome(conc_incr, crf, crf_conc_increment, number_of_deaths),
      outcome_lower = calc_outcome(conc_incr, crf_lower, crf_conc_increment, number_of_deaths),
      outcome_upper = calc_outcome(conc_incr, crf_upper, crf_conc_increment, number_of_deaths),
    ) |>
    dplyr::select(-conc_incr)

  return(data)
}

#' Get year of health-outcome base scenario: either provided year or a provided function
#'
#' @param base
#' @param ...
#'
#' @keywords internal
get_base_scenario_year <- function(base = "min", ...) {

  if (is.character(base)) {
    fun <- function(x) get(base)(x, ...)
  } else {
    fun <- function(x) base
  }

  return(fun)
}
