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
  dplyr::select(-lower_conc_threshold_source, -crf_source, -comment, -threshold_unit, -crf_unit, -min_conc_threshold_source)

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
data_expo_base <- 
  data_expo_pop |> 
  dplyr::filter(pollutant %in% unique(outcomes_meta$pollutant) & year == get_base_scenario_year(unique(outcomes_meta$base_scenario_year))(.data$year)) |> 
  dplyr:::select(x, y, pollutant, concentration) |> 
  dplyr::rename(concentration_base = concentration)

data_outcomes <-
  data_expo_pop |> 
  dplyr::filter(pollutant %in% unique(outcomes_meta$pollutant)) |>
  dplyr:::select(-source) |> 
  dplyr::left_join(outcomes_meta, by = "pollutant") |> 
  dplyr::left_join(data_deaths, by = "year") |> 
  dplyr::left_join(data_expo_base, by = c("x", "y", "pollutant")) |> # FIXME: join yields some NA in conc_base for years other than base_year => why? grid should have remained the same, does it?
  dplyr::group_by(year, pollutant) |> 
  dplyr::mutate(
    population_total = sum(population),
    min_conc_base_threshold = get(unique(.data$min_conc_threshold))(concentration_base, na.rm = TRUE),
    min_conc_threshold = get(unique(.data$min_conc_threshold))(concentration, na.rm = TRUE)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    min_conc_threshold = ifelse(min_conc_threshold > lower_conc_threshold, lower_conc_threshold, min_conc_threshold),
    min_conc_base_threshold = ifelse(min_conc_base_threshold > lower_conc_threshold, lower_conc_threshold, min_conc_base_threshold),
    conc_increment_lower = ifelse(concentration < lower_conc_threshold, NA, concentration - lower_conc_threshold),
    conc_base_increment_lower = ifelse(concentration_base < lower_conc_threshold, NA, concentration_base - lower_conc_threshold),
    conc_increment_min = ifelse(concentration < min_conc_threshold, NA, concentration - min_conc_threshold),
    outcome = calc_outcome(conc_increment_lower, crf, crf_factor, deathrate * factor_deathrate, population),
    outcome_lower_confint = calc_outcome(conc_increment_lower, crf_lower_confint, crf_factor, deathrate * factor_deathrate, population),
    outcome_upper_confint = calc_outcome(conc_increment_lower, crf_upper_confint, crf_factor, deathrate * factor_deathrate, population),
    outcome_min = calc_outcome(conc_increment_min, crf, crf_factor, deathrate * factor_deathrate, population),
    outcome_min_lower_confint = calc_outcome(conc_increment_min, crf_lower_confint, crf_factor, deathrate * factor_deathrate, population),
    outcome_min_upper_confint = calc_outcome(conc_increment_min, crf_upper_confint, crf_factor, deathrate * factor_deathrate, population),
    outcome_base = calc_outcome(conc_base_increment_lower, crf, crf_factor, deathrate * factor_deathrate, population),
    source = factor("BAFU & BFS & Statistisches Amt Kanton Zürich")
  )



# aggregate dataset ...
# ---
# => 
data_outcomes <-
  aggregate_outcomes(data_outcomes,
                     vars = c("outcome", "outcome_lower_confint", "outcome_upper_confint", "outcome_min", 
                              "outcome_min_lower_confint", "outcome_min_upper_confint", "outcome_base"),
                     groups = c("year", "pollutant", "outcome_type", "population_total", "base_scenario_year", "source")
  ) |> 
  dplyr::mutate(
    delta_base = outcome - outcome_base,
    base_scenario_year = get_base_scenario_year(unique(base_scenario_year))(.data$year)
    ) |>
  dplyr::select(-outcome_base) 




data_outcomes |> 
  dplyr::select(year, base_scenario_year, pollutant, outcome_type, outcome, outcome_lower_confint, outcome_min_upper_confint, delta_base) |> 
  tidyr::gather(type, value, -year, -base_scenario_year, -pollutant, -outcome_type, -outcome_lower_confint, -outcome_min_upper_confint) |> 
  dplyr::mutate(type = dplyr::recode(type, outcome = "aktuell", delta_base = paste0("vermieden vs. ",unique(.data$base_scenario_year)))) |> 
  ggplot(aes(x = year, y = value, fill = type)) + 
  geom_bar(stat = "identity") + 
  geom_hline(yintercept = 0, color = "gray30") +
  geom_linerange(aes(ymin = outcome_lower_confint, ymax = outcome_min_upper_confint), color = "gray50") + 
  facet_grid(pollutant~outcome_type) + 
  ylab("absolut")


data_outcomes |> 
  dplyr::select(year, base_scenario_year, population_total, pollutant, outcome_type, outcome, outcome_lower_confint, outcome_min_upper_confint, delta_base) |> 
  tidyr::gather(type, value, -year, -base_scenario_year, -population_total, -pollutant, -outcome_type, -outcome_lower_confint, -outcome_min_upper_confint) |> 
  dplyr::mutate(type = dplyr::recode(type, outcome = "aktuell", delta_base = paste0("vermieden vs. ",unique(.data$base_scenario_year)))) |> 
  ggplot(aes(x = year, y = value / population_total * 10000, fill = type)) + 
  geom_bar(stat = "identity") + 
  geom_hline(yintercept = 0, color = "gray30") +
  geom_linerange(aes(ymin = outcome_lower_confint / population_total * 10000, ymax = outcome_min_upper_confint / population_total * 10000)) +
  facet_grid(pollutant~outcome_type) + 
  ylab("pro 10'000 Einwohner/innen")


# write output datasets & clean up:
# ---
# write_local_csv(d..., file = "inst/extdata/output/....csv")
# rm(list = c("data_expo_pop", ...))
