# TODO: see issue #34

# read datasets ...
# ---
# =>  read effektschätzer & lower thresholds from local metadata
outcomes_meta <- read_local_csv(filter_ressources(ressources, 24), locale = readr::locale(encoding = "UTF-8"))

# => get Canton Zurich yearly mortality rates from opendata.swiss
data_deaths <- read_opendataswiss(filter_ressources(ressources, 25), source = "Statistisches Amt Kanton Zürich")

# prepare datasets ...
# ---

# => only look at all-cause natural mortality
outcomes_meta <-
  outcomes_meta |> 
  dplyr::filter(health_outcome == "all-cause natural mortality") |> 
  dplyr::select(-lower_conc_threshold_source, -crf_source, -comment)

# => 
data_deaths <-
  data_deaths |> 
  dplyr::filter(GEBIET_NAME == "Zürich - ganzer Kanton") |> 
  dplyr::rename(
    year = INDIKATOR_JAHR,
    parameter = INDIKATOR_NAME,
    value = INDIKATOR_VALUE,
    unit_deathrate = EINHEIT_KURZ
  ) |> 
  dplyr::mutate(
    parameter = stringr::str_remove(parameter, stringr::fixed(" [pro 1000 Einw.]")),
    factor_deathrate = 1 / readr::parse_number(unit_deathrate)
  ) |> 
  dplyr::select(year, parameter, value, factor_deathrate) |> 
  tidyr::spread(parameter, value) |> 
  dplyr::rename(deathrate = Sterberate)


# =>
data_outcomes <-
  data_expo_pop |> 
  dplyr::filter(pollutant %in% unique(outcomes_meta$pollutant)) |>
  dplyr::arrange(year, pollutant, concentration) |>
  dplyr:::select(-source) |> 
  dplyr::left_join(outcomes_meta, by = "pollutant") |> 
  dplyr::left_join(data_deaths, by = "year")

data_outcomes <-
  data_outcomes |> 
  dplyr::group_by(year, pollutant) |> 
  dplyr::mutate(
    population_weighted_mean = calc_population_weighted_mean(concentration, population),
    population_total = sum(population),
    lowest_conc_threshold = get(unique(.data$lowest_conc_threshhold))(concentration) 
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    lowest_conc_threshold = ifelse(lowest_conc_threshold > lower_conc_threshold, lower_conc_threshold, lowest_conc_threshold),
    conc_increment_lower = ifelse(concentration < lower_conc_threshold, NA, concentration - lower_conc_threshold),
    conc_increment_lowest = ifelse(concentration < lowest_conc_threshold, NA, concentration - lowest_conc_threshold),
    outcome = conc_increment_lower / crf_factor * (crf - 1) * deathrate * factor_deathrate * population,
    outcome_lower_confint = conc_increment_lower / crf_factor * (crf_lower_confint - 1) * deathrate * factor_deathrate * population,
    outcome_upper_confint = conc_increment_lower / crf_factor * (crf_upper_confint - 1) * deathrate * factor_deathrate * population,
    outcome_lowest = conc_increment_lowest / crf_factor * (crf - 1) * deathrate * factor_deathrate * population,
    outcome_lowest_lower_confint = conc_increment_lowest / crf_factor * (crf_lower_confint - 1) * deathrate * factor_deathrate * population,
    outcome_lowest_upper_confint = conc_increment_lowest / crf_factor * (crf_upper_confint - 1) * deathrate * factor_deathrate * population
  ) |> 
  dplyr::group_by(year, pollutant, health_outcome, population_total, population_weighted_mean) |> 
  dplyr::summarise(
    outcome = sum(outcome, na.rm = TRUE),
    outcome_lower_confint = sum(outcome_lower_confint, na.rm = TRUE),
    outcome_upper_confint = sum(outcome_upper_confint, na.rm = TRUE),
    outcome_lowest = sum(outcome_lowest, na.rm = TRUE),
    outcome_lowest_lower_confint = sum(outcome_lowest_lower_confint, na.rm = TRUE),
    outcome_lowest_upper_confint = sum(outcome_lowest_upper_confint, na.rm = TRUE)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(source = factor("BAFU & BFS & Statistisches Amt Kanton Zürich"))


# aggregate dataset ...
# ---
# => 


# get population exposition from scripts

# calc attributable outcomes by PM2.5

# ...





# write output datasets & clean up:
# ---
# write_local_csv(d..., file = "inst/extdata/output/....csv")
# rm(list = c("data_expo_pop", ...))
