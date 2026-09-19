
#' Calculate population weighted mean concentration for exposition assessment
#'
#' @param concentration
#' @param population
#'
#' @keywords internal
calc_population_weighted_mean <- function(concentration, population) {sum(concentration * population, na.rm = TRUE) / sum(population, na.rm = TRUE)}
