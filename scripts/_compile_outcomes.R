# Derive selected health outcomes per year from population-weighted mean data


# read datasets ...
# ---
# =>  read input-metadata (crf, lower threshold concentration etc)
outcomes_meta <- 
  filter_ressources(ressources, 24) |> 
  read_local_csv(locale = readr::locale(encoding = "UTF-8")) |> 
  dplyr::select(-lower_conc_threshold_source, -min_conc_threshold, -crf_source, -base_scenario_year, -comment, -threshold_unit, -crf_unit, -min_conc_threshold_source)

# => get Canton Zurich yearly mortality rates from opendata.swiss
data_deathrates <- read_opendataswiss(filter_ressources(ressources, 25), source = "Statistisches Amt Kanton Zürich")

# => read population weighted mean data
data_expo_weighmean <- read_local_csv("inst/extdata/output/data_exposition_weighted_means_canton.csv") 


# prepare datasets ...
# ---
# => rate of deaths in Canton Zürich
data_deathrates <- prepare_deathrate(data_deathrates)

# => prepare input dataset and derive preliminary deaths
data_outcomes <- prepare_outcomes(data_expo_weighmean, data_deathrates, outcomes_meta)

# => prepare input dataset and derive years of life lost
# TODO ...

# => combine
# TODO ...


# write output datasets & clean up:
# ---
write_local_csv(data_outcomes, file = "inst/extdata/output/data_health_outcomes.csv")
rm(list = c("outcomes_meta", "data_deathrates", "data_expo_weighmean", "data_outcomes"))












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


