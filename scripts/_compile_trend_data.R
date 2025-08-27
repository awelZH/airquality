#TODO: work in progress!

# years to consider for analysis and later plotting
years <- 1990:(lubridate::year(Sys.Date()) - 1)  

# reference year for relative trends
reference_year <- 2015 # base_scenario_year

# read pre-compiled emission data
data_emikat <- airquality.methods::read_local_csv("inst/extdata/output/data_emissions.csv", delim = ";", locale = readr::locale(encoding = "UTF-8"))

# read pre-compiled airquality monitoring data
data_monitoring_aq <- airquality.methods::read_local_csv("inst/extdata/output/data_airquality_monitoring_y1.csv", locale = readr::locale(encoding = "UTF-8"))

# ...
nmin <- 1

# compile data
emissions <-
  data_emikat |>
  airquality.methods::aggregate_groups(y = "emission", groups = c("year", "pollutant"), nmin = 1) |>
  dplyr::select(year, pollutant, sum) |>
  dplyr::rename(emission = sum) |>
  dplyr::mutate(
    emission = ifelse(is.na(emission), 0, emission),
    pollutant = dplyr::recode(pollutant, NOx = "NO2 | NOx")
  )

data_trends <-
  data_monitoring_aq |>
  dplyr::filter(metric == "Jahresmittel" & pollutant != "O3") |>
  airquality.methods::aggregate_groups(y = "concentration", groups =  c("year", "pollutant"), nmin = nmin) |>
  dplyr::select(year, pollutant, middle) |>
  dplyr::rename(concentration_median = middle) |>
  dplyr::mutate(pollutant = dplyr::recode(pollutant, NO2 = "NO2 | NOx")) |>
  dplyr::left_join(emissions, by = c("year", "pollutant"))

data_trends <-
  data_trends |>
  dplyr::filter(year == !!reference_year) |>
  dplyr::select(-year) |>
  dplyr::rename(
    concentration_refyear = concentration_median,
    emission_refyear = emission
  ) |>
  dplyr::right_join(data_trends, by = c("pollutant")) |>
  dplyr::mutate(
    "relative Immission" = concentration_median / concentration_refyear - 1,
    "relative Emission" = emission / emission_refyear - 1,
    pollutant = airquality.methods::longpollutant(pollutant)
  ) |>
  dplyr::select(year, pollutant, `relative Immission`, `relative Emission`) |>
  tidyr::gather(parameter, value, -year, -pollutant) |> 
  dplyr::filter(year %in% !!years)


# write output datasets & clean up:
# ---
airquality.methods::write_local_csv(data_trends, file = "inst/extdata/output/data_trends_emission_immission_relative.csv")
rm(list = c("data_monitoring_aq", "data_emikat", "years", "reference_year", "nmin"))
