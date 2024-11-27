#' Aggregates sf multipolygon into one bounding polygon
#'
#' @param map 
#'
#' @return
#' @export
#'
#' @examples
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
#' 
#' @description
#' Merges subsectors with small emissions into one common category and sums up emissions accordingly.
#' 
#' @return
#' @export
#'
#' @examples
aggregate_emmissions <- function(data){
  
  groups <- groups_emission_subsector(data)
  data <- dplyr::left_join(
    data, 
    groups, 
    by = c("pollutant", "sector", "subsector")
  )
  
  group_vars <- c("year", "pollutant", "unit", "sector", "subsector_new")
  data <- 
    data |> 
    aggregate_groups(y = "emission", groups = group_vars, nmin = 1) |> 
    dplyr::rename(emission = sum) |> 
    dplyr::select(tidyr::all_of(c(group_vars,"emission"))) |> 
    dplyr::mutate(source = "Ostluft & BAFU") |> 
    dplyr::filter(!is.na(emission) & emission > 0) #! test possibility here: sum of emissions needs to match that of original data_emikat
  
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
#' @return
#' @export
#'
#' @examples
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
    ) 
  
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
#' @return
#' @export
#'
#' @examples
aggregate_nitrogen_deposition <- function(data) {
  
  data <- simplify_nitrogen_parameters(data)
  
  estimate <- 
    data |> 
    dplyr::filter(parameter == "N-Deposition") |> 
    dplyr::select(year, site, ecosystem_category, estimate)
  
  data <- 
    data |> 
    dplyr::group_by(year, site, site_long, source, siteclass, ecosystem_category, critical_load_min, critical_load_single, critical_load_max, parameter, unit) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::ungroup() |>
    dplyr::left_join(estimate, by = c("year", "site", "ecosystem_category")) |> 
    dplyr::mutate(estimate = dplyr::case_when(parameter == "N-Deposition" ~ estimate, TRUE ~ NA))
  
  return(data)
}


#' Aggregates air pollutant population exposition dataset
#'
#' @param data 
#'
#' @description
#' Calculates sum of inhabitant population per pollutant concentration bins and years;
#' derives population per bin, cumulative population per bin and relative cumulative population per bin.
#' 
#' @return
#' @export
#'
#' @examples
aggregate_population_exposition_distrib <- function(data) { 
  
  data <-
    data |> 
    dplyr::group_split(pollutant) |> 
    purrr::map(bin_concentration) |> 
    dplyr::bind_rows() |> 
    dplyr::group_by(year, pollutant, concentration) |> 
    dplyr::summarise(population = sum(population)) |>
    dplyr::ungroup() |> 
    dplyr::arrange(year, pollutant, concentration) |> 
    dplyr::group_by(year, pollutant) |> 
    dplyr::mutate(
      population_cum = cumsum(population),
      population_cum_rel = population_cum / sum(population)
    ) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(source = "BAFU & BFS")
  
  return(data)
}


#' Aggregates sensitive ecosystem exposition towards reactive atmospheric nitrogen deposition dataset
#'
#' @param data 
#'
#' @description
#' Calculates number of sensitive ecosystems per bin of maximum nitrogen deposition exceeding ecosystem-specific Critical Loads and year;
#' number of sensitive ecosystems per bin, cumulative number of sensitive ecosystems per bin and relative cumulative number of sensitive ecosystems per bin.
#' 
#' @return
#' @export
#'
#' @examples
aggregate_ndep_exposition_distrib <- function(data) { 
  
  data <-
    data |> 
    na.omit() |> 
    dplyr::mutate(ndep_exmax = floor(ndep_exmax) + 0.5) |> # abgerundet auf 1, Klassenmitte
    dplyr::group_by(year, ndep_exmax) |>
    dplyr::summarise(n_ecosys = dplyr::n()) |>
    dplyr::group_by(year) |>
    dplyr::arrange(ndep_exmax) |>
    dplyr::mutate(
      n_ecosys_cum = cumsum(n_ecosys),
      n_ecosys_cum_rel = cumsum(n_ecosys) / sum(n_ecosys),
      source = "BAFU"
    ) |> 
    dplyr::ungroup() |> 
    dplyr::arrange(year, ndep_exmax)
  
  return(data)
}


#' Aggregates to mean population-weighted concentrations from air pollutant and inhabitant population dataset
#'
#' @param data 
#' @param groups 
#'
#' @return
#' @export
#'
#' @examples
aggregate_population_weighted_mean <- function(data, groups = c("year", "pollutant")) {
  
  data_pop_weighted <-
    data |> 
    dplyr::group_by_at(dplyr::vars(groups)) |> 
    dplyr::summarise(population_weighted_mean = calc_population_weighted_mean(concentration, population)) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(
      unit = "μg/m3",
      source = "BAFU & BFS"
    )
  
  return(data_pop_weighted)
}



