#TODO: work in progress!




prepare_data_trends <- function(data_aq, data_met) {
  
  # aq dataset
  data_trends <- 
    data_aq |> 
    dplyr::filter(canton %in% !!cantons) |> 
    dplyr::select(starttime, site, parameter, concentration) |> 
    tidyr::spread(parameter, concentration) |> 
    dplyr::mutate(
      site_met = dplyr::case_when( # TODO: besser...
        site %in% c("Bac_Turm", "Wld_Höhenklinik") ~ "Hörnli",
        stringr::str_detect(site, "Zch_") ~ "Zürich / Fluntern",
        stringr::str_detect(site, "Zürich-") ~ "Zürich / Fluntern",
        stringr::str_detect(site, "Sch_Güterstrasse") ~ "Zürich / Affoltern",
        TRUE ~ "Zürich / Kloten"
      )
    )
  
  # add meteo vars
  data_trends <- 
    data_met |> 
    dplyr::select(starttime, site, parameter, value) |>
    dplyr::rename(site_met = site) |> 
    dplyr::mutate(
      value = 
        dplyr::case_when(
          parameter == "T" ~ value + 273.15,
          parameter == "T_max_h1" ~ value + 273.15,
          parameter == "T_max_min10" ~ value + 273.15,
          TRUE ~ value
        )
    ) |> 
    tidyr::spread(parameter, value) |> 
    dplyr::right_join(data_trends, by = c("starttime", "site_met")) |> 
    tidyr::gather(parameter, value, -starttime, -site, -site_met)
  
  # merge some sites
  data_trends <- 
    data_trends |> 
    dplyr::filter(!(lubridate::year(starttime) > 2014 & site == "Win_Obertor")) |> 
    dplyr::filter(!(lubridate::year(starttime) > 2008 & site == "Bac_Turm")) |> 
    dplyr::mutate(
      site = dplyr::case_when(
        site %in% c("Bac_Turm", "Wld_Höhenklinik") & parameter != "PM2.5" ~ factor("Bachtel/Wald"),
        site %in% c("Win_Obertor", "Win_Veltheim") & parameter != "PM2.5" ~ factor("Win_Obertor/Veltheim"),
        TRUE ~ site
      )
    ) |> 
    dplyr::distinct(starttime, site, parameter, .keep_all = TRUE)
  
  return(data_trends)
}



rf_meteo_normalisation <- function(data, trend_vars, frac_train = 0.8, ntrees = 300, nsamples = 300, verbose = TRUE, minimal = TRUE, coverage = 0.8) {
  
  # see example at https://github.com/skgrange/rmweather
  # prepare, grow/train a random forest model and then create a meteorological normalised trend 
  print(paste0("processing ", unique(data$site)))
  
  list_normalised <- 
    data |> 
    rmweather::rmw_prepare_data(na.rm = TRUE, fraction = frac_train) |> 
    rmweather::rmw_do_all(
      variables = c("date_unix", "day_julian", "weekday", trend_vars),
      n_trees = ntrees,
      n_samples = nsamples,
      verbose = verbose
    )
  
  # Check model object's performance
  model_stats <- rmweather::rmw_model_statistics(list_normalised$model)
  
  # Plot variable importances
  imp <- rmweather::rmw_model_importance(list_normalised$model)
  
  # normalised trend and observations based in original interval
  data <- 
    data |>
    dplyr::select(date, site, parameter, value) |> 
    dplyr::left_join(list_normalised$normalised, by = "date") |> 
    dplyr::rename(
      gemessen = value,
      Trend = value_predict
    ) |> 
    tidyr::gather(type, value, -date, -site, -parameter) |> 
    dplyr::mutate(type = factor(type, levels = c("gemessen", "Trend"))) 
  
  print(paste0("R2 = ", airquality.methods::round_off(model_stats$r_squared,2)))
  print(imp)
  
  # normalised trend and observations based on yearly interval
  data_y1 <-
    data |>
    dplyr::group_by(year = lubridate::year(date), site, parameter, type) |>
    dplyr::summarise(
      n = sum(!is.na(value)),
      value = mean(value, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      value = ifelse(is.nan(value), NA, value),
      value = ifelse(n < 365 * !!coverage, NA, value)
    )
  
  if (!minimal) {
    
    # Check if model has suffered from overfitting
    pred_obs <-
      rmweather::rmw_predict_the_test_set(
        model = list_normalised$model,
        df = list_normalised$observations
      )
    
    # Investigate partial dependencies, if variable is NA, predict all
    partialdep <-
      rmweather::rmw_partial_dependencies(
        model = list_normalised$model,
        df = list_normalised$observations,
        variable = NA
      )
    
    results <- list(data = data, data_y1 = data_y1, model_stats = model_stats, imp = imp, partialdep = partialdep)
    
  } else {
    
    results <- list(data = data, data_y1 = data_y1, model_stats = model_stats, imp = imp)
    
  }
  
  return(results)
}


derive_trends_per_pollutant <- function(data_trends, pollutant, reference_year_fun, yearmin_per_site = 4, coverage = 0.8) {
  
  # how many data, reference year included?
  data_trends_agg <-
    data_trends |> 
    dplyr::filter(parameter %in% !!c(pollutant, trend_vars) & !is.na(value)) |> 
    dplyr::group_by(year = lubridate::year(starttime), site, parameter) |> 
    dplyr::summarise(n = dplyr::n()) |> 
    dplyr::ungroup() |> 
    dplyr::filter(n >= 365 * !!coverage)
  
  sites_trends_refyears <-
    data_trends_agg |>
    dplyr::mutate(reference_year = reference_year_fun(parameter)) |> 
    dplyr::filter(year == reference_year & parameter == !!pollutant) |>
    dplyr::distinct(site)  |>
    dplyr::pull(site) |>
    as.character()
  
  data_trends_agg <-
    data_trends_agg |> 
    dplyr::group_by(site, parameter) |> 
    dplyr::summarise(n = dplyr::n()) |> 
    dplyr::ungroup() |> 
    dplyr::arrange(dplyr::desc(n)) |> 
    dplyr::filter(n >= !!yearmin_per_site) |>
    dplyr::filter(site %in% !!sites_trends_refyears) |>
    tidyr::spread(parameter, n)
  
  sites_trends <- 
    data_trends_agg |> 
    dplyr::select(site, !!pollutant, !!trend_vars) |> 
    na.omit() |> 
    dplyr::pull(site) |> 
    as.character()
  
  print(sites_trends)
  
  data_analysis <- 
    data_trends |> 
    dplyr::filter(site %in% !!sites_trends) |> 
    dplyr::select(starttime, site, parameter, value)
  
  data_analysis <- 
    data_analysis |> 
    dplyr::filter(parameter %in% !!trend_vars) |> 
    tidyr::spread(parameter, value) |> 
    dplyr::right_join(dplyr::filter(data_analysis, !(parameter %in% !!trend_vars) & parameter == !!pollutant), by = c("starttime", "site")) |> 
    dplyr::rename(date = starttime) |> 
    dplyr::group_split(site)
  
  results <- purrr::map(data_analysis, function(x) rf_meteo_normalisation(x, trend_vars))
  results_y1 <- 
    results |> 
    purrr::map(function(x) x$data_y1) |> 
    dplyr::bind_rows()
  
  return(results_y1)
}



prepare_trend_results <- function(trends, reference_year_fun, nmin_sites_fun) {
  
  trends <-
    trends |>
    dplyr::mutate(reference_year = reference_year_fun(parameter)) |> 
    dplyr::filter(year == reference_year) |>
    dplyr::select(-year, -n) |>
    dplyr::rename(value_refyear = value) |>
    dplyr::right_join(trends, by = c("site", "parameter", "type")) |>
    dplyr::rename(pollutant = parameter) |> 
    dplyr::mutate(
      "relative Immission" = value / value_refyear,
      `relative Immission` = ifelse(site == "Zch_Schimmelstrasse" & year < 2009, NA, `relative Immission`) # due to strong traffic changes at this site
    ) |>
    dplyr::select(year, site, pollutant, type, value, `relative Immission`, reference_year)
  
  trends_agg <-
    trends |> 
    dplyr::group_by(year, pollutant, type) |>
    dplyr::summarise(
      n = sum(!is.na(`relative Immission`)),
      `relative Immission` = median(`relative Immission`, na.rm = TRUE)
    ) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(
      nmin = nmin_sites_fun(pollutant),
      `relative Immission` = ifelse(n < nmin, NA, `relative Immission`),
      reference_year = reference_year_fun(pollutant),
      pollutant = airquality.methods::longpollutant(pollutant)
    ) |> 
    dplyr::select(-nmin)
  
  trends <- dplyr::mutate(trends, pollutant = airquality.methods::longpollutant(pollutant))
  
  return(list(all = trends, agg = trends_agg))
}


prepare_emission_trends <- function(emissions, reference_year_fun) {
  
  emissions <-
    emissions |>
    dplyr::mutate(reference_year = reference_year_fun(pollutant)) |> 
    dplyr::filter(year == reference_year) |>
    dplyr::select(-year) |>
    dplyr::rename(emission_refyear = emission) |>
    dplyr::right_join(emissions, by = "pollutant") |>
    dplyr::mutate(
      "relative Emission" = emission / emission_refyear,
      pollutant = airquality.methods::longpollutant(pollutant),
      type = "emission"
      ) |>
    dplyr::select(year, pollutant, type, `relative Emission`, reference_year)
  
  return(emissions)
}






# parameters for trend analysis
# ---

# years to consider for analysis and later plotting
years <- 1990:(lubridate::year(Sys.Date()) - 1)  

# reference year for relative trends
reference_year <- function(pollutant, base_year = 2015){
  dplyr::case_when(
    pollutant == "PM2.5" ~ 2021,
    pollutant == "eBC" ~ 2020,
    pollutant == "NHx" ~ 2020,
    pollutant == "Ndep" ~ 2020,
    TRUE ~ base_year
  )
}

# minimum number of sites per year for which median trend is derived
nmin_sites <- function(pollutant){
  dplyr::case_when(
    pollutant == "PM10" ~ 3,
    pollutant == "PM2.5" ~ 3,
    pollutant == "eBC" ~ 3,
    pollutant == "NOx" ~ 5,
    pollutant == "NO2" ~ 5,
    pollutant == "O3" ~ 6,
    pollutant == "NHx" ~ 5,
    pollutant == "Ndep" ~ 5,
    TRUE ~ 4
  )
}

# minimum number of years available for trend analysis per site
yearmin_per_site <- 4 

# for analysis
cantons <- "ZH"
trend_vars <- c("T", "T_max_min10", "Hr", "StrGlo", "p", "WVs", "WD", "RainSum")
pollutants <- c("PM2.5", "PM10", "NOx", "eBC")
# TODO: also include NHx, Ndep and O3 on different time-interval than d1


# get monitoring datasets from airquality.data (see here: https://github.com/awelZH/airquality.data) 
# TODO: ... remove data_monitoring_ndep from airquality.data when ndep-analysis is online at github. Integrate future github dataset instead
# TODO: ... replace airquality.data as soon as useful api methods are available for this kind of data
# ---
# get pre-compiled airquality monitoring data as daily averages and yearly values
data_monitoring_aq <- airquality.data::data_monitoring_aq_d1 
data_monitoring_aq_y1 <- airquality.methods::read_local_csv("inst/extdata/output/data_airquality_monitoring_y1.csv", locale = readr::locale(encoding = "UTF-8"))

# get local pre-compiled emission data
data_emikat <- airquality.methods::read_local_csv("inst/extdata/output/data_emissions.csv", delim = ";", locale = readr::locale(encoding = "UTF-8"))

# get pre-compiled airquality-network meteorological data as daily averages
data_monitoring_met <- airquality.data::data_monitoring_met_d1


# prepare data
# ---
data_monitoring_aq <- dplyr::filter(data_monitoring_aq, lubridate::year(starttime) %in% !!years & canton %in% !!cantons)
data_monitoring_met <- dplyr::filter(data_monitoring_met, parameter %in% !!trend_vars)
data_trends <- prepare_data_trends(data_monitoring_aq, data_monitoring_met)


# trend analysis and result aggregation (takes a while)
# ---
fun <- function(x) {
  print(x)
  trends <- derive_trends_per_pollutant(data_trends, pollutant = x, reference_year_fun = reference_year, yearmin_per_site = yearmin_per_site)
  return(trends)
}
trends <- purrr::map(pollutants, fun)
trends <- dplyr::bind_rows(trends)


# prepare relative trend results for plotting
trends_relative <- prepare_trend_results(trends, reference_year_fun = reference_year, nmin_sites_fun = nmin_sites)
trends_relative$all <- 
  trends_relative$all |> 
  dplyr::mutate(pollutant = dplyr::recode(pollutant, Stickoxide = "Stickoxide | Stickstoffdioxid", NOx = "Stickoxide | Stickstoffdioxid", NO2 = "Stickoxide | Stickstoffdioxid", NH3 = "Ammoniak | reduzierter Stickstoff", NHx = "Ammoniak | reduzierter Stickstoff")) |> 
  dplyr::filter(year %in% !!years)

# add pure measurement results
data_monitoring_median <- 
  data_monitoring_aq_y1 |> 
  dplyr::filter(metric == "Jahresmittel" & pollutant %in% c("NO2", !!pollutants)) |>
  airquality.methods::aggregate_groups(y = "concentration", groups =  c("year", "pollutant"), nmin = 1) |>
  dplyr::mutate(
    nmin = nmin_sites(pollutant),
    reference_year = reference_year(pollutant),
    middle = ifelse(n < nmin, NA, middle)
  ) |> 
  dplyr::select(year, pollutant, n, middle, reference_year) |>
  dplyr::rename(median = middle) 

trends_relative$agg <- 
  data_monitoring_median |> 
  dplyr::filter(year == reference_year) |> 
  dplyr::rename(refval = median) |> 
  dplyr::select(-year, -reference_year, -n) |> 
  dplyr::right_join(data_monitoring_median, by = "pollutant") |> 
  dplyr::mutate(
    "relative Immission" = median / refval,
    pollutant = airquality.methods::longpollutant(pollutant),
    type = "Messwerte"
  ) |> 
  dplyr::select(year, pollutant, type, n, `relative Immission`, reference_year) |> 
  dplyr::bind_rows(trends_relative$agg) |> 
  dplyr::mutate(
    pollutant = dplyr::recode(pollutant, Stickstoffdioxid = "Stickoxide | Stickstoffdioxid", Stickoxide = "Stickoxide | Stickstoffdioxid", Ammoniak = "Ammoniak | reduzierter Stickstoff"),
    site = "Kanton Zürich"
    # aggregation = "Median"
  ) |> 
  dplyr::filter(year %in% !!years)


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
  prepare_emission_trends(emissions, reference_year_fun = reference_year) |> 
  tidyr::gather(parameter, value, -year, -pollutant, -type, -reference_year) |> 
  dplyr::mutate(site = "Kanton Zürich")


# combine relative monitoring & emission trends
trends_relative$agg <- 
  trends_relative$agg |> 
  tidyr::gather(parameter, value, -year, -pollutant, -type, -n, -reference_year, -site) |> 
  dplyr::bind_rows(emissions_relative)

trends_relative$all <-
  trends_relative$all |> 
  dplyr::rename(Immission = value) |> 
  tidyr::gather(parameter, value, -year, -pollutant, -site, -type, -reference_year) |> 
  dplyr::bind_rows(emissions_relative)

# trends_relative$all |> 
#   ggplot(aes(x = year, y = value, color = type)) +
#   geom_line() +
#   geom_point() +
#   scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01)) + 
#   facet_grid(pollutant~site, scales = "free_y")


# write output datasets & clean up:
# ---
airquality.methods::write_local_csv(trends_relative$all, file = "inst/extdata/output/data_airquality_trends_relative_y1.csv")
airquality.methods::write_local_csv(trends_relative$agg, file = "inst/extdata/output/data_airquality_trends_relative_aggregated_y1.csv")
rm(list = c("years", "reference_year", "nmin_sites", "yearmin_per_site", "pollutants", "cantons", "trend_vars", 
            "data_monitoring_median", "data_emikat", "data_monitoring_aq", "data_monitoring_met", "data_trends", "fun", "trends", "trends_relative"))







# plot_imp <- rmweather::rmw_plot_importance(imp)
# plot_pred_obs <- rmweather::rmw_plot_test_prediction(pred_obs)
# plot_pd <- rmweather::rmw_plot_partial_dependencies(partialdep)
# plot_trend_d1 <-
#   data |> 
#   ggplot2::ggplot(ggplot2::aes(x = date, y = value, group = type, color = type)) + 
#   ggplot2::geom_line() +
#   ggplot2::scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01)) +
#   ggplot2::scale_color_manual(values = c("gemessen" = "gray80", "Trend" = "steelblue")) +
#   theme_custom
# plot_trend_y1 <-
#   data_y1 |> 
#   ggplot2::ggplot(ggplot2::aes(x = year, y = value, group = type, color = type)) + 
#   ggplot2::geom_line() +
#   ggplot2::geom_point() +
#   ggplot2::scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01)) +
#   ggplot2::scale_color_manual(values = c("gemessen" = "gray80", "Trend" = "steelblue")) +
#   theme_custom

