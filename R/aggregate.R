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


#' Aggregates emission budget data
#'
#' @param data
#' @param subsector_new
#' @param cols
#'
#' @description
#' Merges subsectors with small emissions into one common (or pre-defined) category and sums up emissions accordingly.
#'
#' @export
aggregate_emmissions <- function(data, subsector_new = NULL,
                                 cols = c("Dienstleistungen" = "Gold", "Haushalte" =  "Green", "Industrie" = "Blue",
                                          "Land- und Forstw." = "Purple", "Verkehr" = "Gray", "natürl. Emissionen" = "natural")
){

  if (is.null(subsector_new)) {
    groups <- groups_emission_subsector(data)
    data <- dplyr::left_join(data, groups, by = c("pollutant", "sector", "subsector"))
  } else {
    data <- dplyr::left_join(data, subsector_new, by = "subsector")
    data <-
      data |>
      dplyr::mutate(
        subsector_new = dplyr::case_when(
          is.na(subsector_new) ~ subsector,
          TRUE ~ subsector_new
        )
      )
  }

  group_vars <- c("year", "pollutant", "unit", "sector", "subsector_new")
  data <-
    data |>
    aggregate_groups(y = "emission", groups = group_vars, nmin = 1) |>  #! test possibility here: sum of emissions needs to match that of original data_emikat
    dplyr::rename(emission = sum) |>
    dplyr::select(tidyr::all_of(c(group_vars,"emission"))) |>
    dplyr::mutate(
      metric = "Jahresmenge",
      source = "Ostluft & BAFU"
    ) |>
    dplyr::select(year, pollutant, metric, unit, sector, subsector_new, emission, source) |>
    dplyr::filter(!is.na(emission) & emission > 0)

  data <-
    data |>
    dplyr::group_by(sector, subsector_new) |>
    dplyr::summarise(emission = sum(emission, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::arrange(sector, dplyr::desc(emission)) |>
    dplyr::distinct(sector, subsector_new) |>
    dplyr::mutate(
      rootcol = dplyr::recode(sector, !!!cols),
      order = 1:dplyr::n()
    ) |>
    dplyr::group_by(sector) |>
    dplyr::mutate(col = pal_emissions(dplyr::n(), unique(rootcol))) |>
    dplyr::ungroup() |>
    dplyr::right_join(data, by = c("sector", "subsector_new")) |>
    dplyr::arrange(year, pollutant, sector, dplyr::desc(emission))

  return(data)
}



#' Aggregates RSD NOx emissions to specified groups
#'
#' @param data
#' @param rsd_auxiliary
#' @param groups
#'
#' @description
#' Combines measurement data with auxiliary metadata, filters dataset using provided filter criteria
#' and calculates mean values of specified vehicle groups.
#'
#' @export
aggregate_rsd_nox <- function(data, rsd_auxiliary, groups = c("vehicle_type", "vehicle_fuel_type", "vehicle_euronorm")){

  if (!("year" %in% groups)) {

    rsd_meta <-
      rsd_auxiliary$meta |>
      dplyr::select(-source, -remark) |>
      tidyr::spread(parameter, value)

  } else {

    rsd_meta <- NULL

  }

  rsd_filters <- rsd_auxiliary$filters

  data_aggregated <-
    aggregate_rsd(
      data,
      rsd_meta,
      y = "nox_emission",
      groups = groups,
      nmin = rsd_filters$min[rsd_filters$parameter == "nmin"]
    ) |>
    dplyr::rename(emission = nox_emission) |>
    dplyr::mutate(
      pollutant = "NOx",
      metric = "Mittelwert"
    ) |>
    dplyr::select(pollutant, metric, !!groups, emission, unit, dplyr::everything())


  return(data_aggregated)
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

