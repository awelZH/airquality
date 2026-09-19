#' Aggregates sf multipolygon into one bounding polygon
#'
#' @param map
#'
#' @export
aggregate_map <- function(map) {

  map <-
    map |>
    sf::st_union() |>
    sf::st_boundary() |>
    sf::st_cast("POLYGON")

  return(map)
}


#' Aggregates monitoring nitrogen deposition data
#'
#' @param data
#'
#' @description
#' Simplifies nitrogen deposition components to broader source categories and aggregates nitrogen deposition input dataset
#' per year, site, ecosystem category and source category.
#'
#' @export
aggregate_nitrogen_deposition <- function(data) {

  data <- simplify_nitrogen_parameters(data)

  estimate <-
    data |>
    dplyr::filter(parameter == "N-Deposition") |>
    dplyr::select(year, site, ecosystem_category, estimate)

  data <-
    data |>
    dplyr::group_by(year, site, site_long, source, siteclass, ecosystem_category, critical_load_min, critical_load_single, critical_load_max, component = parameter, unit) |>
    dplyr::summarise(deposition = sum(value)) |>
    dplyr::ungroup() |>
    dplyr::left_join(estimate, by = c("year", "site", "ecosystem_category")) |>
    dplyr::mutate(
      metric = "Jahreseintrag",
      estimate = dplyr::case_when(component == "N-Deposition" ~ estimate, TRUE ~ NA)
    ) |>
    dplyr::select(year, site, site_long, siteclass, ecosystem_category, component, metric, deposition, unit, dplyr::everything())

  return(data)
}


#' Wrangle and aggregate results from statistical meteo-normalisation of monitoring data as relative trends
#'
#' @param trends
#' @param reference_year_fun
#' @param nmin_sites_fun
#'
#' @export
aggregate_trend_results <- function(trends, reference_year_fun, nmin_sites_fun) {

  trends <-
    trends |>
    dplyr::mutate(reference_year = reference_year_fun(parameter)) |>
    dplyr::filter(year == reference_year) |>
    dplyr::select(-year, -n) |>
    dplyr::rename(value_refyear = value) |>
    dplyr::right_join(trends, by = c("site", "parameter", "type")) |>
    dplyr::mutate(
      "relative Immission" = value / value_refyear,
      `relative Immission` = ifelse(site == "Zch_Schimmelstrasse" & year < 2009, NA, `relative Immission`) # due to strong traffic changes at this site
    ) |>
    dplyr::select(year, site, parameter, type, value, `relative Immission`, reference_year)

  trends_agg <-
    trends |>
    dplyr::group_by(year, parameter, type) |>
    dplyr::summarise(
      n = sum(!is.na(`relative Immission`)),
      `relative Immission` = median(`relative Immission`, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      reference_year = reference_year_fun(parameter),
      pollutant = shortpollutant(parameter),
      pollutant = longpollutant(pollutant),
      metric = longparameter(parameter),
      nmin = nmin_sites_fun(pollutant),
      `relative Immission` = ifelse(n < nmin, NA, `relative Immission`)
    ) |>
    dplyr::select(-nmin)

  trends <-
    trends |>
    dplyr::mutate(
      pollutant = shortpollutant(parameter),
      pollutant = longpollutant(pollutant),
      metric = longparameter(parameter)
    )

  return(list(all = trends, agg = trends_agg))
}

