# TODO: see issue #34



# read datasets ...
# ---
# =>  read effektschätzer & lower thresholds from local metadata
outcomes_meta <- 
  filter_ressources(ressources, 24) |> 
  read_local_csv(locale = readr::locale(encoding = "UTF-8")) |> 
  dplyr::select(-lower_conc_threshold_source, -min_conc_threshold, -crf_source, -base_scenario_year, -comment, -threshold_unit, -crf_unit, -min_conc_threshold_source)

# => get Canton Zurich yearly mortality rates from opendata.swiss
data_deaths <- read_opendataswiss(filter_ressources(ressources, 25), source = "Statistisches Amt Kanton Zürich")

# => read population weighted mean data
data_expo_weighmean <- read_local_csv("inst/extdata/output/data_exposition_weighted_means_canton.csv") 





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



# => prepare input dataset
data <- 
  data_expo_weighmean |> 
  dplyr::filter(pollutant %in% unique(outcomes_meta$pollutant)) |>
  dplyr:::select(-source, -unit, -concentration_max, -concentration_mean, -concentration_median) |> 
  tidyr::gather(scenario, population_weighted_mean, -year, -pollutant, -base_year, -population, -concentration_min) |> 
  dplyr::left_join(outcomes_meta, by = "pollutant") |> 
  dplyr::left_join(data_deaths, by = "year") |> 
  dplyr::mutate(
    scenario = dplyr::recode(scenario, population_weighted_mean = "aktuell", population_weighted_mean_base = paste0("vermieden vs. ",na.omit(unique(.data$base_year)))),
    concentration_min = ifelse(stringr::str_detect(scenario, "vermieden"), NA, concentration_min),
    deathrate_per_person = deathrate * factor_deathrate
  ) |> 
  dplyr::select(-base_year, -deathrate, -factor_deathrate) |> 
  dplyr::filter(!is.na(scenario))

data <- 
  data |> 
  dplyr::filter(scenario == "aktuell") |> 
  dplyr::mutate(min_conc_threshold = pmin(concentration_min, lower_conc_threshold)) |> 
  prepare_outcome(conc_threshold = "min_conc_threshold") |> 
  dplyr::select(year, pollutant, scenario, outcome_type, outcome) |>
  dplyr::rename(outcome_min_conc = outcome) |> 
  dplyr::right_join(data, by = c("year", "pollutant", "scenario", "outcome_type")) |> 
  prepare_outcome() |> 
  dplyr::mutate(outcome_delta_min_conc = outcome_min_conc - outcome) |> 
  dplyr::select(year, pollutant, population, scenario, outcome_type, outcome, outcome_lower, outcome_upper, outcome_delta_min_conc)

data <- 
  data |> 
  dplyr::select(year, pollutant, scenario, outcome_type, outcome) |> 
  tidyr::spread(scenario, outcome) |> 
  dplyr::mutate(`vermieden vs. 2015` = pmin(0, aktuell - `vermieden vs. 2015`)) |> 
  dplyr::select(-aktuell) |> 
  tidyr::gather(scenario, outcome, -year, -pollutant, -outcome_type) |> 
  dplyr::bind_rows(dplyr::filter(data, scenario == "aktuell")) |> 
  dplyr::arrange(pollutant, scenario, year, outcome_type) |> 
  dplyr::select(year, pollutant, outcome_type,  population, scenario, outcome, outcome_lower, outcome_upper, outcome_delta_min_conc)





data |> 
  ggplot(aes(x = year, y = outcome, fill = scenario)) + 
  geom_bar(stat = "identity") + 
  geom_hline(yintercept = 0, color = "gray30", linetype = 2) +
  geom_linerange(aes(ymin = outcome_lower, ymax = outcome_upper + outcome_delta_min_conc), color = "gray0") + 
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
    axis.line.x = element_line(color = "gray30"),
    axis.title = element_blank()
  ) +
  facet_wrap(pollutant~., ncol = 1)




# write output datasets & clean up:
# ---
# write_local_csv(d..., file = "inst/extdata/output/....csv")
# rm(list = c("data_expo_pop", ...))
