# TODO: see issue #34

# read datasets ...
# ---
# =>  read effektschätzer & lower thresholds from local metadata
outcomes_meta <- 
  filter_ressources(ressources, 24) |> 
  read_local_csv(locale = readr::locale(encoding = "UTF-8")) |> 
  dplyr::select(-lower_conc_threshold_source, -crf_source, -comment, -threshold_unit, -crf_unit, -min_conc_threshold_source)

# => get Canton Zurich yearly mortality rates from opendata.swiss
data_deaths <- read_opendataswiss(filter_ressources(ressources, 25), source = "Statistisches Amt Kanton Zürich")




# ...testing
# d <- read_opendataswiss_json("https://ckan.opendata.swiss/api/3/action/package_show?id=todesfalle-nach-institutionellen-gliederungen-geschlecht-staatsangehorigkeit-kategorie-zivilsta5", source = "BFS", file_filter = "api/v1/de")
# 
# 
# read_opendataswiss_json <- function(url, source, file_filter = ".csv"){
#   browser()
# 
#   read_url <- get_opendataswiss_metadata("https://ckan.opendata.swiss/api/3/action/package_show?id=todesfalle-nach-institutionellen-gliederungen-geschlecht-staatsangehorigkeit-kategorie-zivilsta5", file_filter)
#   data <- jsonlite::read_json(read_url, simplifyVector = F)
# 
# 
#   # req <- httr2::request(read_url)
#   # req_data <- httr2::req_perform(req)
#   # metadata <- httr2::resp_body_json(req_data)$result
#   # links <- unlist(purrr::map(metadata$resources, function(x) x$url))
# 
# 
#   #   data$variables[[1]]
#   #
#   #   data$variables$valueTexts[[1]]
#   #   data$variables$values[[1]]
#   #   data$variables$values[[2]]
#   #
#   #
#   #   data %>% as.tbl_json %>% gather_array
#   # as_tibble(data)
#   # enframe(unlist(data))
#   #
#   #   gather_object(data$variables)
#   #   spread_values(data$variables)
#   #   spread_all(data$variables)
#   #   as.tbl_json(data$variables)
#   #   tidyjson::spread_all(read_url)
#   #   tidyjson::as_data_frame.tbl_json(data$variables)
# 
#   data <- dplyr:: mutate(data, source = source)
# 
#   return(data)
# }









# prepare datasets ...
# ---

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
  tidyr::gather(scenario, value, -year, -base_scenario_year, -pollutant, -outcome_type, -outcome_lower_confint, -outcome_min_upper_confint) |> 
  dplyr::mutate(scenario = dplyr::recode(scenario, outcome = "aktuell", delta_base = paste0("vermieden vs. ",unique(.data$base_scenario_year)))) |> 
  ggplot(aes(x = year, y = value, fill = scenario)) + 
  geom_bar(stat = "identity") + 
  geom_hline(yintercept = 0, color = "gray30", linetype = 2) +
  geom_linerange(aes(ymin = outcome_lower_confint, ymax = outcome_min_upper_confint), color = "gray0") + 
  scale_y_continuous(breaks = seq(-2000,2000,200), labels = function(x) format(x, big.mark = "'"), expand = c(0.01,0.01)) +
  facet_grid(.~outcome_type) + 
  ylab("absolut") +
  scale_fill_manual(name = "Szenario", values = c("#50586C", "#DCE2F0")) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks = element_line(color = "gray30"),
    axis.line.x = element_line(color = "gray30")
  )


data_outcomes |> 
  dplyr::select(year, base_scenario_year, population_total, pollutant, outcome_type, outcome, outcome_lower_confint, outcome_min_upper_confint, delta_base) |> 
  tidyr::gather(scenario, value, -year, -base_scenario_year, -population_total, -pollutant, -outcome_type, -outcome_lower_confint, -outcome_min_upper_confint) |> 
  dplyr::mutate(scenario = dplyr::recode(scenario, outcome = "aktuell", delta_base = paste0("vermieden vs. ",unique(.data$base_scenario_year)))) |> 
  ggplot(aes(x = year, y = value / population_total * 10000, fill = scenario)) + 
  geom_bar(stat = "identity") + 
  geom_hline(yintercept = 0, color = "gray30", linetype = 2) +
  geom_linerange(aes(ymin = outcome_lower_confint / population_total * 10000, ymax = outcome_min_upper_confint / population_total * 10000), color = "gray20") +
  scale_y_continuous(breaks = seq(-20,120,1), expand = c(0.01,0.01)) +
  facet_grid(.~outcome_type) + 
  ylab("pro 10'000 Einwohner/innen") +
  scale_fill_manual(name = "Szenario", values = c("#50586C", "#DCE2F0")) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks = element_line(color = "gray30"),
    axis.line.x = element_line(color = "gray30")
  )


# write output datasets & clean up:
# ---
# write_local_csv(d..., file = "inst/extdata/output/....csv")
# rm(list = c("data_expo_pop", ...))
