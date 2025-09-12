#TODO: work in progress!

# years to consider for analysis and later plotting
years <- 1990:(lubridate::year(Sys.Date()) - 1)  

# reference year for relative trends
reference_year <- 2021 # base_scenario_year

# read pre-compiled emission data
data_emikat <- airquality.methods::read_local_csv("inst/extdata/output/data_emissions.csv", delim = ";", locale = readr::locale(encoding = "UTF-8"))

# read pre-compiled airquality monitoring data
data_monitoring_aq <- airquality.methods::read_local_csv("inst/extdata/output/data_airquality_monitoring_y1.csv", locale = readr::locale(encoding = "UTF-8"))

# read pre-compiled airquality-network meteorological data
data_monitoring_met <- airquality.data::data_monitoring_met_d1





# # ...
# nmin <- 1
# 
# # compile data
# emissions <-
#   data_emikat |>
#   airquality.methods::aggregate_groups(y = "emission", groups = c("year", "pollutant"), nmin = 1) |>
#   dplyr::select(year, pollutant, sum) |>
#   dplyr::rename(emission = sum) |>
#   dplyr::mutate(
#     emission = ifelse(is.na(emission), 0, emission),
#     pollutant = dplyr::recode(pollutant, NOx = "NO2 | NOx")
#   )
# 
# data_trends <-
#   data_monitoring_aq |>
#   dplyr::filter(metric == "Jahresmittel" & pollutant != "O3") |>
#   airquality.methods::aggregate_groups(y = "concentration", groups =  c("year", "pollutant"), nmin = nmin) |>
#   dplyr::select(year, pollutant, middle) |>
#   dplyr::rename(concentration_median = middle) |>
#   dplyr::mutate(pollutant = dplyr::recode(pollutant, NO2 = "NO2 | NOx")) |>
#   dplyr::left_join(emissions, by = c("year", "pollutant"))
# 
# data_trends <-
#   data_trends |>
#   dplyr::filter(year == !!reference_year) |>
#   dplyr::select(-year) |>
#   dplyr::rename(
#     concentration_refyear = concentration_median,
#     emission_refyear = emission
#   ) |>
#   dplyr::right_join(data_trends, by = c("pollutant")) |>
#   dplyr::mutate(
#     "relative Immission" = concentration_median / concentration_refyear - 1,
#     "relative Emission" = emission / emission_refyear - 1,
#     pollutant = airquality.methods::longpollutant(pollutant)
#   ) |>
#   dplyr::select(year, pollutant, `relative Immission`, `relative Emission`) |>
#   tidyr::gather(parameter, value, -year, -pollutant) |> 
#   dplyr::filter(year %in% !!years)
# 
# 
# # write output datasets & clean up:
# # ---
# airquality.methods::write_local_csv(data_trends, file = "inst/extdata/output/data_trends_emission_immission_relative.csv")
# rm(list = c("data_monitoring_aq", "data_emikat", "years", "reference_year", "nmin"))

















nmin <- 4 #minimum number of years available for trend analysis per site
cantons <- c("ZH", NA) # NA = NABEL sites
trend_vars <- c("T", "T_max_min10", "Hr", "StrGlo", "p", "WVs", "WD", "RainSum")


# aq dataset
data_trends <- 
  airquality.data::data_monitoring_aq_d1 |> 
  dplyr::filter(canton %in% !!cantons) |> 
  dplyr::select(starttime, site, parameter, concentration) |> 
  tidyr::spread(parameter, concentration) |> 
  dplyr::mutate(
    site_met = dplyr::case_when( # TODO: besser...
      site %in% c("Bac_Turm", "Wld_Höhenklinik") ~ "Hörnli",
      TRUE ~ "Zürich / Kloten"
    )
  )

# add meteo vars
data_trends <- 
  data_monitoring_met |> 
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
  dplyr::filter(parameter %in% !!trend_vars) |> 
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




# select subset of sites per pollutant used for trend analysis, needs to include the reference year
pollutant <- "PM10"


# how many data, reference year included?
data_trends_agg <-
  data_trends |> 
  dplyr::filter(parameter %in% !!c(pollutant, trend_vars) & !is.na(value) & lubridate::year(starttime) %in% !!years) |> 
  dplyr::group_by(year = lubridate::year(starttime), site, parameter) |> 
  dplyr::summarise(n = dplyr::n()) |> 
  dplyr::ungroup() |> 
  dplyr::filter(n >= 0.9*365)

sites_trends_refyears <-
  data_trends_agg |>
  dplyr::filter(year == !!reference_year & parameter == !!pollutant) |>
  dplyr::distinct(site)  |>
  dplyr::pull(site) |>
  as.character()

data_trends_agg <-
  data_trends_agg |> 
  dplyr::group_by(site, parameter) |> 
  dplyr::summarise(n = dplyr::n()) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(dplyr::desc(n)) |> 
  dplyr::filter(n >= !!nmin) |>
  dplyr::filter(site %in% !!sites_trends_refyears) |>
  tidyr::spread(parameter, n)

sites_trends <- 
  data_trends_agg |> 
  dplyr::select(site, !!pollutant, !!trend_vars) |> 
  na.omit() |> 
  dplyr::pull(site) |> 
  as.character()

sites_trends

data_analysis <- 
  data_trends |> 
  dplyr::filter(site %in% !!sites_trends & lubridate::year(starttime) %in% !!years) |> 
  dplyr::select(starttime, site, parameter, value)

data_analysis <- 
  data_analysis |> 
  dplyr::filter(parameter %in% !!trend_vars) |> 
  tidyr::spread(parameter, value) |> 
  dplyr::right_join(dplyr::filter(data_analysis, !(parameter %in% !!trend_vars) & parameter == !!pollutant), by = c("starttime", "site")) |> 
  dplyr::rename(date = starttime) |> 
  dplyr::group_split(site)

results <- purrr::map(data_analysis, function(x) rf_meteo_normalisation(x, trend_vars))







results |> 
  purrr::map(function(x) x$data) |> 
  dplyr::bind_rows() |> 
  ggplot(aes(x = date, y = value, color = type)) +
  geom_line() +
  facet_wrap(site~.)

results_y1 <- 
  results |> 
  purrr::map(function(x) x$data_y1) |> 
  dplyr::bind_rows()

results_y1 |> 
  ggplot(aes(x = year, y = value, color = type, size = n)) +
  geom_line() +
  geom_point() +
  facet_wrap(site~.)

results_y1 <-
  results_y1 |>
  dplyr::filter(year == !!reference_year) |>
  dplyr::select(-year, -n) |>
  dplyr::rename(
    value_refyear = value,
    # emission_refyear = emission
  ) |>
  dplyr::right_join(results_y1, by = c("site", "parameter", "type")) |>
  dplyr::rename(pollutant = parameter) |> 
  dplyr::mutate(
    "relative Immission" = value / value_refyear,
    # "relative Emission" = emission / emission_refyear,
    pollutant = airquality.methods::longpollutant(pollutant)
  ) |>
  dplyr::select(year, site, pollutant, type, value, `relative Immission`) #, `relative Emission`) |>
# tidyr::gather(parameter, value, -year, -pollutant)

results_y1 |> 
  dplyr::group_by(year, pollutant, type) |>
  dplyr::summarise(`relative Immission` = median(`relative Immission`, na.rm = TRUE)) |> 
  dplyr::ungroup() |> 
  # dplyr::filter(type == "normalised") |> 
  ggplot(aes(x = year, y = `relative Immission` - 1, color = type, group = type)) +
  geom_hline(yintercept = 0, color = "gray30", linetype = 2) +
  geom_line() +
  geom_point(shape = 21, fill = "white") +
  # geom_smooth(se = FALSE, span = 0.8) +
  scale_y_continuous(labels = scales::percent_format()) + 
  scale_color_manual(values = c("normalised" = "steelblue", "observed" = "gray80"))






rf_meteo_normalisation <- function(data, trend_vars, frac_train = 0.8, ntrees = 300, nsamples = 300, verbose = TRUE) {
  
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
  
  # normalised trend and observations based in original interval
  data <- 
    data |>
    dplyr::select(date, site, parameter, value) |> 
    dplyr::left_join(list_normalised$normalised, by = "date") |> 
    dplyr::rename(
      observed = value,
      normalised = value_predict
    ) |> 
    tidyr::gather(type, value, -date, -site, -parameter) |> 
    dplyr::mutate(type = factor(type, levels = c("observed", "normalised"))) 
  
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
      value = ifelse(n < 365*0.9, NA, value)
    )
  
  print(paste0("R2 = ", airquality.methods::round_off(model_stats$r_squared,2)))
  print(imp)
  
  results <- list(data = data, data_y1 = data_y1, model_stats = model_stats, imp = imp, partialdep = partialdep)
  
  return(results)
}






# theme_custom <- 
#   ggplot2::theme_minimal() +
#   ggplot2::theme(
#     axis.line.x = ggplot2::element_line(color = "gray30"),
#     axis.ticks = ggplot2::element_line(color = "gray30"),
#     panel.grid.major.x = ggplot2::element_blank(),
#     panel.grid.minor.x = ggplot2::element_blank(),
#     legend.title = ggplot2::element_blank()
#   )
# 
# 
# plot_imp <- rmweather::rmw_plot_importance(imp)
# plot_pred_obs <- rmweather::rmw_plot_test_prediction(pred_obs)
# plot_pd <- rmweather::rmw_plot_partial_dependencies(partialdep)
# plot_trend_d1 <-
#   data |> 
#   ggplot2::ggplot(ggplot2::aes(x = date, y = value, group = type, color = type)) + 
#   ggplot2::geom_line() +
#   ggplot2::scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01)) +
#   ggplot2::scale_color_manual(values = c("observed" = "gray80", "normalised" = "steelblue")) +
#   theme_custom
# plot_trend_y1 <-
#   data_y1 |> 
#   ggplot2::ggplot(ggplot2::aes(x = year, y = value, group = type, color = type)) + 
#   ggplot2::geom_line() +
#   ggplot2::geom_point() +
#   ggplot2::scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01)) +
#   ggplot2::scale_color_manual(values = c("observed" = "gray80", "normalised" = "steelblue")) +
#   theme_custom



