# compiling trends of air pollutants based on emission data and monitoring data for the Canton of Zürich.
# for monitoring data, meteo-normalised trends (using package 'rmweather') are also derived.
# output: respective trends relative to one pre-defined point in time


# settings ...
# ---
# => see scripts/_setup.R: trend_years, trend_cantons, trend_parameters, trend_vars_d1, trend_yearmin_per_site,
#    trend_reference_year(), trend_nmin_sites()
# TODO: also include NHx on different time-interval than d1
# TODO: ... remove data_monitoring_ndep from airquality.data when ndep-analysis is online at github. Integrate future github dataset instead


# get monitoring datasets from airquality.data (see here: https://github.com/awelZH/airquality.data) 
# ---
# get pre-compiled airquality monitoring data as daily averages and yearly values
data_monitoring_aq <- airquality.data::data_monitoring_aq_d1
data_monitoring_aq_y1 <- airquality.methods::read_local_csv("inst/extdata/output/data_airquality_monitoring_y1.csv", locale = readr::locale(encoding = "UTF-8"))

# get local pre-compiled emission data
data_emikat <- airquality.methods::read_local_csv("inst/extdata/output/data_emissions.csv", delim = ";", locale = readr::locale(encoding = "UTF-8"))

# get pre-compiled airquality-network meteorological data as daily averages
data_monitoring_met_d1 <- airquality.data::data_monitoring_met_d1


# prepare data
# ---
data_monitoring_aq <- dplyr::filter(data_monitoring_aq, lubridate::year(starttime) %in% !!trend_years & canton %in% !!trend_cantons)
data_monitoring_met_d1 <- dplyr::filter(data_monitoring_met_d1, parameter %in% !!trend_vars_d1)
data_trends <- prepare_data_trends(data_monitoring_aq, data_monitoring_met_d1, cantons = trend_cantons)


# trend analysis and result aggregation (takes a while) for d1 data
# ---
fun <- function(x) {
  print(x)
  trends <- derive_trends_per_parameter(data_trends, parameter = x, trend_vars = trend_vars_d1, reference_year_fun = trend_reference_year, yearmin_per_site = trend_yearmin_per_site)
  return(trends)
}
pars <- trend_parameters[!(trend_parameters %in% c("O3_peakseason_mean_d1_max_mean_h8gl", "O3_max_98p_m1", "NH3", "NHx", "Ndep"))] # no d1 trend analysis for these ones
trends <- purrr::map(pars, fun)
trends <- dplyr::bind_rows(trends)

# wrangle and aggregate relative trend results for plotting
trends_relative <- aggregate_trend_results(trends, reference_year_fun = trend_reference_year, nmin_sites_fun = trend_nmin_sites)
trends_relative$all <- 
  trends_relative$all |> 
  dplyr::mutate(pollutant = dplyr::recode(pollutant, Stickoxide = "Stickoxide | Stickstoffdioxid", NOx = "Stickoxide | Stickstoffdioxid", NO2 = "Stickoxide | Stickstoffdioxid", NH3 = "Ammoniak | reduzierter Stickstoff", NHx = "Ammoniak | reduzierter Stickstoff")) |> 
  dplyr::filter(year %in% !!trend_years)


# trend analysis and result aggregation (takes a while) for m1 data (NHx)
# ---
#TODO: work in progress!




# combine yearly trends with median measurement results and emission data; derive relative time series
# ---
# add pure measurement results
data_monitoring_median <- 
  data_monitoring_aq_y1 |> 
  dplyr::filter(parameter %in% unique(c("NO2", !!trend_parameters))) |>
  airquality.methods::aggregate_groups(y = "concentration", groups =  c("year", "parameter"), nmin = 1) |>
  dplyr::mutate(
    nmin = trend_nmin_sites(parameter),
    reference_year = trend_reference_year(parameter),
    middle = ifelse(n < nmin, NA, middle)
  ) |> 
  dplyr::select(year, parameter, n, middle, reference_year) |>
  dplyr::rename(median = middle) 

trends_relative$agg <- 
  data_monitoring_median |> 
  dplyr::filter(year == reference_year) |> 
  dplyr::rename(refval = median) |> 
  dplyr::select(-year, -reference_year, -n) |> 
  dplyr::right_join(data_monitoring_median, by = "parameter") |> 
  dplyr::mutate(
    pollutant = airquality.methods::shortpollutant(parameter), 
    pollutant = airquality.methods::longpollutant(pollutant),
    metric = airquality.methods::longparameter(parameter),
    "relative Immission" = median / refval,
    type = "Messwerte"
  ) |> 
  dplyr::select(year, pollutant, metric, parameter, type, n, `relative Immission`, reference_year) |> 
  dplyr::bind_rows(trends_relative$agg) |> 
  dplyr::mutate(
    pollutant = dplyr::recode(pollutant, Stickstoffdioxid = "Stickoxide | Stickstoffdioxid", Stickoxide = "Stickoxide | Stickstoffdioxid", Ammoniak = "Ammoniak | reduzierter Stickstoff"),
    site = "Kanton Zürich"
  ) |> 
  dplyr::filter(year %in% !!trend_years)


# derive relative emissions
emissions <-
  data_emikat |>
  airquality.methods::aggregate_groups(y = "emission", groups = c("year", "pollutant"), nmin = 1) |>
  dplyr::select(year, pollutant, sum) |>
  dplyr::rename(emission = sum) |>
  dplyr::mutate(
    pollutant = dplyr::recode(pollutant, NOx = "Stickoxide | Stickstoffdioxid", NH3 = "Ammoniak | reduzierter Stickstoff"),
    emission = ifelse(is.na(emission), 0, emission)
  )

emissions_relative <- 
  prepare_emission_trends(emissions, reference_year_fun = trend_reference_year) |> 
  tidyr::gather(class, value, -year, -pollutant, -type, -reference_year) |> 
  dplyr::mutate(site = "Kanton Zürich")


# combine relative monitoring & emission trends
trends_relative$agg <- 
  trends_relative$agg |> 
  tidyr::gather(class, value, -year, -pollutant, -metric, -parameter, -type, -n, -reference_year, -site) |> 
  dplyr::bind_rows(emissions_relative)

# FIXME
trends_relative$all <-
  trends_relative$all |> 
  dplyr::rename(Immission = value) |> 
  tidyr::gather(class, value, -year, -pollutant, -metric, -parameter, -site, -type, -reference_year) |> 
  dplyr::bind_rows(emissions_relative)

# trends_relative$all |>
#   ggplot(aes(x = year, y = value, color = type)) +
#   geom_line() +
#   geom_point() +
#   scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01)) +
#   facet_grid(parameter~site, scales = "free_y")


# write output datasets & clean up:
# ---
airquality.methods::write_local_csv(trends_relative$all, file = "inst/extdata/output/data_airquality_trends_relative_y1.csv")
airquality.methods::write_local_csv(trends_relative$agg, file = "inst/extdata/output/data_airquality_trends_relative_aggregated_y1.csv")
rm(list = c("pars", "emissions", "emissions_relative",
            "data_monitoring_median", "data_emikat", "data_monitoring_aq", "data_monitoring_met_d1", "data_trends", "fun", "trends", "trends_relative"))

