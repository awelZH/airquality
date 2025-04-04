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

# => get Canton Zurich yearly mortality rates from opendata.swiss
# TODO: use better dataset ...
data_deathrates <- read_opendataswiss(filter_ressources(ressources, 25), source = "Statistisches Amt Kanton Zürich")

# => read Swiss life-expectancy data
data_life_exp <- read_bfs_life_expectancy_data()

# => read population weighted mean data
data_expo_weighmean <- read_local_csv("inst/extdata/output/data_exposition_weighted_means_canton.csv", locale = readr::locale(encoding = "UTF-8")) 

# prepare datasets ...
# ---
# => rate of deaths in Canton Zürich
data_deathrates <- prepare_deathrate(data_deathrates)

# => life expectancy in Switzerland
data_life_exp <- prepare_life_expectancy_data(data_life_exp)

# => prepare input dataset and derive preliminary deaths
data_outcomes <- prepare_outcomes(data_expo_weighmean, data_deathrates, outcomes_meta)

# => prepare input dataset and derive years of life lost
# TODO ...
# from here: https://opendata.swiss/de/dataset/kohortensterbetafeln-fur-die-schweiz-1876-2030-nach-geburtsjahrgang-geschlecht-und-alter1









# => combine
# TODO ...


# write output datasets & clean up:
# ---
write_local_csv(data_outcomes, file = "inst/extdata/output/data_health_outcomes.csv")
rm(list = c("outcomes_meta", "data_deathrates", "data_expo_weighmean", "data_outcomes"))


