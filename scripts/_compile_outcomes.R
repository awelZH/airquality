# Derive selected health outcomes per year from population-weighted mean data
# TODO: use upcoming R-package from SwissTPH et al. for calculations instead of own functions ...
# TODO: finalise ressources.csv


# read datasets ...
# ---
# =>  read input-metadata (crf, lower threshold concentration etc)
outcomes_meta <- 
  filter_ressources(ressources, 24) |> 
  read_local_csv(locale = readr::locale(encoding = "UTF-8")) |> 
  dplyr::select(-lower_conc_threshold_source, -min_conc_threshold, -crf_source, -comment, -threshold_unit, -crf_unit, -min_conc_threshold_source)

# => get Canton Zurich yearly mortality cases from opendata.swiss
# TODO: OGD dataset ...
# data_deathrates <- read_opendataswiss(filter_ressources(ressources, 25), source = "Statistisches Amt Kanton Zürich")
data_mortality <- read_local_csv("inst/extdata/tod_nat_gatu.csv", delim = ",", locale = readr::locale(encoding = "UTF-8"))

# => read Swiss life-expectancy data (BFS Kohortensterbetafeln)
data_life_exp <- read_bfs_life_expectancy_data()

# => read population weighted mean data
data_expo_weighmean <- read_local_csv("inst/extdata/output/data_exposition_weighted_means_canton.csv", locale = readr::locale(encoding = "UTF-8")) 

# prepare datasets ...
# ---
# => number of deaths in Canton Zürich
data_mortality <- prepare_mortality(data_mortality )  #FIXME once original dataset is adjusted

# => add life expectancy in Switzerland
data_life_exp <- prepare_life_expectancy_data(data_life_exp)
data_mortality <- 
  data_life_exp |> 
  dplyr::select(-source) |> 
  dplyr::right_join(data_mortality, by = c("sex", "year_of_birth", "age")) 

# => derive preliminary deaths
data_preliminary_deaths <- prepare_preliminary_deaths(data_expo_weighmean, data_mortality, outcomes_meta)

# => derive lifeyears lost / decrease in life expectancy
# TODO ...

# => combine
# TODO ...
data_outcomes <- data_preliminary_deaths


# write output datasets & clean up:
# ---
write_local_csv(data_outcomes, file = "inst/extdata/output/data_health_outcomes.csv")
rm(list = c("outcomes_meta", "data_mortality", "data_life_exp", "data_expo_weighmean", "data_outcomes", "data_preliminary_deaths"))


