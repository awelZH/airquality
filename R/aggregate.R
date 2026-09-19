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

