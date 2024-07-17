aggregate_emissions <- function(data, fun = sum, groups = c("year", "pollutant", "unit", "sector", "subsector")) {
  
  data <-
    data %>% 
    dplyr::group_by_at(dplyr::vars(groups)) %>%
    dplyr::summarise(emission = fun(emission)) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(emission > 0)
  
  return(data)
}


groups_emission_subsector <- function(data, threshold_fraction = 0.02, index = 1:3) {
  
  # mean emissions per category over all years
  
  groups <- aggregate_emissions(data, mean, c("pollutant", "sector", "subsector"))
  
  # recode subsectors with mean emissions less than the "big x" into subsector "sonstige"
  
  groups <- 
    groups %>% 
    dplyr::group_by(pollutant, sector) %>% 
    dplyr::mutate(
      subsector_new = dplyr::case_when(
        emission < min(sort(emission, decreasing = TRUE)[index]) ~ "sonstige",
        TRUE ~ subsector
      ),
      subsector_new = paste0(sector, " / ", subsector_new)
    ) 
  
  # aggregate emissions accordingly
  
  groups2 <- aggregate_emissions(groups, sum, c("pollutant", "subsector_new"))
  
  # second iteration: if subsector emission < overall emission * threshold fraction, then also recode into "sonstige"
  
  groups2 <- 
    groups2 %>% 
    dplyr::group_by(pollutant) %>% 
    dplyr::mutate(
      subsector_new = dplyr::case_when(
        subsector_new != "weitere" & emission < threshold_fraction * sum(emission)  ~ "sonstige",
        TRUE ~ subsector_new
      )
    ) 
  
  # aggregate accordingly
  
  groups2 <- aggregate_emissions(groups, sum, c("pollutant", "subsector_new"))
  
  # restructure and prepare as lookup table for use outside this function
  
  groups2 <- dplyr::filter(groups2, !stringr::str_detect(subsector_new, "sonstige"))
  
  groups <-
    groups %>% 
    dplyr::select(-emission) %>% 
    dplyr::left_join(groups2, by = c("pollutant", "subsector_new")) %>% 
    dplyr::arrange(pollutant, sector, dplyr::desc(emission)) %>% 
    dplyr::mutate(subsector_new = factor(subsector_new, levels = unique(.data$subsector_new))) %>% 
    dplyr::select(-emission)
  
  return(groups)
}



# aggregate RSD data calculating: n, percentiles, median, mean, standard deviation, standard error
aggregate_groups_rsd <- function(data, y, groups = c("vehicle_type", "vehicle_fuel_type", "vehicle_euronorm"),
                                 nmin = 100, perc = list(ymin = 0.05, lower = 0.25, middle = 0.5, upper = 0.75, ymax = 0.95)) {
  
  data <-
    data %>%
    dplyr::group_by_at(dplyr::vars(groups)) %>%
    dplyr::summarise(
      n = length(na.omit(!!rlang::sym(y))),
      min = quantile(!!rlang::sym(y), perc$ymin, na.rm = TRUE),
      lower = quantile(!!rlang::sym(y), perc$lower, na.rm = TRUE),
      middle = quantile(!!rlang::sym(y), perc$middle, na.rm = TRUE),
      upper = quantile(!!rlang::sym(y), perc$upper, na.rm = TRUE),
      max = quantile(!!rlang::sym(y), perc$ymax, na.rm = TRUE),
      mean = mean(!!rlang::sym(y), na.rm = TRUE),
      standarddeviation = sd(!!rlang::sym(y), na.rm = TRUE),
      standarderror = standarddeviation / sqrt(n)
    ) %>%
    ungroup()
  
  data_all <-
    data %>% 
    dplyr::select(tidyr::all_of(groups)) %>% 
    dplyr::distinct_all() %>% 
    tidyr::expand(tidyr::crossing(!!!rlang::syms(groups)))
  
  data <- dplyr::left_join(data_all, data, by = groups) 
  data <- dplyr::mutate_at(data, c("min", "lower", "middle", "upper", "max", "mean", "standarddeviation", "standarderror"), list(~ifelse(n < nmin, NA, .)))
  data <- dplyr::mutate_at(data, c("min", "lower", "middle", "upper", "max", "mean", "standarddeviation", "standarderror"), list(~ifelse(is.nan(.), NA, .)))
  data <- dplyr::mutate_at(data, c("min", "lower", "middle", "upper", "max", "mean", "standarddeviation", "standarderror"), list(~ifelse(is.infinite(.), NA, .)))
  data$n <- ifelse(is.na(data$n), 0, data$n)
  
  return(data)
}



aggregate_nox_rsd <- function(data, meta, nmin = 50, groups = c("vehicle_type", "vehicle_fuel_type", "vehicle_euronorm")) {
  
  data <- 
    data %>% 
    aggregate_groups_rsd(y = "NOx_emission", groups = groups, nmin = nmin) %>% 
    dplyr::rename(NOx_emission = mean) %>% 
    dplyr::mutate(
      unit = "g/kg fuel",
      source = "Kanton Zürich/AWEL"
    )
  
  if(is.null(meta)) {
    data <- dplyr::select(data, !!c(groups, "NOx_emission", "unit", "n", "standarderror", "source"))
  } else {
    data <- 
      data %>% 
      dplyr::left_join(meta, by = groups) %>% 
      dplyr::select(!!c(groups, "NOx_emission", "unit", "n", "standarderror", "NOx_emission_threshold_g_per_kg_fuel", "source"))
  } 
  
  return(data)
}





