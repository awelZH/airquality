#' Merge components of nitrogen deposition to broader source categories in nitrogen deposition dataset
#'
#' @param data
#'
#' @keywords internal
simplify_nitrogen_parameters <- function(data) {

  data <-
    data |>
    dplyr::mutate(
      parameter = dplyr::case_when(
        stringr::str_detect(parameter, "NO3") | stringr::str_detect(parameter, "NO2") ~ "aus NOx-Quellen",
        stringr::str_detect(parameter, "NH3") | stringr::str_detect(parameter, "NH4") ~ "aus NH3-Quellen",
        TRUE ~ parameter
      ),
      parameter =  factor(parameter, levels = c("aus NOx-Quellen", "aus NH3-Quellen", "N-Deposition")),
      # ecosystem_category = paste0("empfindliches Ökosystem: ", ecosystem_category),
      site_short = stringr::str_remove(site_short, "_Wald"),
      site = stringr::str_remove(site, "_Wald"),
      site = stringr::str_replace(site, "_", "-")
    ) |>
    dplyr::rename(
      site_long = site,
      site = site_short
    )

  return(data)
}


#' Calculate population weighted mean concentration for exposition assessment
#'
#' @param concentration
#' @param population
#'
#' @keywords internal
calc_population_weighted_mean <- function(concentration, population) {sum(concentration * population, na.rm = TRUE) / sum(population, na.rm = TRUE)}
