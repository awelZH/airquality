# health outcomes: premature deaths and years of life lost attributable to the population-weighted means of
# the canton (functions in R/outcomes.R; notes/plan_outcomes.md) -> data_health_outcomes.csv

pipeline_outcomes <- tar_plan(
  # concentration-response functions and cut-off concentrations
  tar_target(outcomes_file_meta, filter_ressources(setup_ressources, 24), format = "file"),
  outcomes_meta = airquality.methods::read_local_csv(outcomes_file_meta, locale = readr::locale(encoding = "UTF-8")),

  # natural deaths per year, sex and age (non-public, data/restricted/; stops with a hint if missing)
  tar_target(outcomes_file_mortality, restricted_file(filter_ressources(setup_ressources, 29)), format = "file"),
  outcomes_mortality =
    airquality.methods::read_local_csv(outcomes_file_mortality, delim = ",", locale = readr::locale(encoding = "UTF-8")) |>
    prepare_mortality(suppressed = outcomes_suppressed_deaths),

  # year-end population per year, sex and age (for the life tables; about 70 MB): the version of the resource
  # is checked on every run, the download only runs when it changed
  tar_target(outcomes_population_state, opendataswiss_state(filter_ressources(setup_ressources, 28)), cue = tar_cue("always")),
  outcomes_population_raw = {
    outcomes_population_state
    airquality.methods::read_opendataswiss(filter_ressources(setup_ressources, 28), source = "Statistisches Amt Kanton Zürich")
  },
  outcomes_population = prepare_population_by_age(outcomes_population_raw),

  # deaths per year from outcomes_min_age; the mortality data of the current year arrive piece by piece
  outcomes_deaths =
    deaths_per_year(outcomes_mortality, min_age = outcomes_min_age) |>
    drop_incomplete_years(min_share = outcomes_min_year_share),
  # life tables of the same years
  outcomes_lifetable =
    lifetable_data(outcomes_mortality, outcomes_population, min_age = outcomes_min_age,
                   max_age = outcomes_lifetable_max_age) |>
    dplyr::filter(year %in% outcomes_deaths$year),

  # premature deaths and years of life lost, actual and avoided vs. the base year
  outcomes_premature_deaths =
    estimate_premature_deaths(expo_pop_means_canton, outcomes_deaths, outcomes_meta, erf_shape = outcomes_erf_shape) |>
    outcome_scenarios(expo_pop_means_canton, outcome_type = "vorzeitige Todesfälle"),
  outcomes_life_years_lost =
    estimate_life_years_lost(expo_pop_means_canton, outcomes_lifetable, outcomes_meta, min_age = outcomes_min_age,
                             max_age = outcomes_lifetable_max_age, erf_shape = outcomes_erf_shape) |>
    outcome_scenarios(expo_pop_means_canton, outcome_type = "verlorene Lebensjahre"),

  tar_target(
    outcomes_out,
    write_output(dplyr::bind_rows(outcomes_premature_deaths, outcomes_life_years_lost), "data_health_outcomes.csv",
                 dir = path_output),
    format = "file"
  )
)
