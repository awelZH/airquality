# Derive selected health outcomes per year from the population-weighted means of the canton
# -> inst/extdata/output/data_health_outcomes.csv (functions in R/outcomes.R)
# TODO: mortality data from opendata.swiss (ressources.csv) once they are published there


# read datasets ...
# ---
# => outcome metadata (concentration-response functions, cut-off concentrations)
outcomes_meta <-
  filter_ressources(ressources, 24) |>
  airquality.methods::read_local_csv(locale = readr::locale(encoding = "UTF-8"))

# => natural deaths per year, sex and age in the Canton of Zurich (Statistisches Amt Kanton Zürich & BFS)
data_mortality <-
  airquality.methods::read_local_csv("inst/extdata/tod_nat_gatu.csv", delim = ",", locale = readr::locale(encoding = "UTF-8")) |>
  prepare_mortality()

# => population per year, sex and age in the Canton of Zurich
data_population <-
  airquality.methods::read_opendataswiss(filter_ressources(ressources, 28), source = "Statistisches Amt Kanton Zürich") |>
  prepare_population_by_age()

# => population-weighted means of the canton
data_expo_weighmean <- airquality.methods::read_local_csv("inst/extdata/output/data_exposition_weighted_means_canton.csv", locale = readr::locale(encoding = "UTF-8"))


# estimate health outcomes ...
# ---
# => deaths per year from outcomes_min_age; the mortality data of the current year arrive piece by piece
data_deaths <-
  deaths_per_year(data_mortality, data_population, min_age = outcomes_min_age) |>
  drop_incomplete_years(min_share = outcomes_min_year_share)

# => premature deaths, actual and avoided vs. the base year
data_outcomes <-
  estimate_premature_deaths(data_expo_weighmean, data_deaths, outcomes_meta, erf_shape = outcomes_erf_shape) |>
  outcome_scenarios(data_expo_weighmean, outcome_type = "vorzeitige Todesfälle")


# write output datasets & clean up:
# ---
airquality.methods::write_local_csv(data_outcomes, file = "inst/extdata/output/data_health_outcomes.csv")
rm(list = c("outcomes_meta", "data_mortality", "data_population", "data_expo_weighmean", "data_deaths", "data_outcomes"))
