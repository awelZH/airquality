#TODO: work in progress!

# years to consider for analysis and later plotting
years <- 1990:(lubridate::year(Sys.Date()) - 1)  

# reference year for relative trends
# reference_year <- 2021 # base_scenario_year
reference_year <- function(pollutant, base_year = 2015){
  dplyr::case_when(
    pollutant == "PM2.5" ~ 2021,
    pollutant == "eBC" ~ 2020,
    pollutant == "NHx" ~ 2020,
    pollutant == "Ndep" ~ 2020,
    TRUE ~ base_year
  )
}

# read pre-compiled emission data
data_emikat <- airquality.methods::read_local_csv("inst/extdata/output/data_emissions.csv", delim = ";", locale = readr::locale(encoding = "UTF-8"))

# get pre-compiled airquality monitoring data as daily averages
data_monitoring_aq <- airquality.data::data_monitoring_aq_d1 

# get pre-compiled airquality-network meteorological data as daily averages
data_monitoring_met <- airquality.data::data_monitoring_met_d1

# minimum number of years available for trend analysis per site
yearmin_per_site <- 4 

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

# for analysis
cantons <- c("ZH", NA) # NA = NABEL sites
trend_vars <- c("T", "T_max_min10", "Hr", "StrGlo", "p", "WVs", "WD", "RainSum")
pollutants <- c("PM2.5", "PM10", "NOx")#, "eBC")

# prepare data
data_monitoring_aq <- dplyr::filter(data_monitoring_aq, lubridate::year(starttime) %in% !!years & canton %in% !!cantons)
data_monitoring_met <- dplyr::filter(data_monitoring_met, parameter %in% !!trend_vars)
data_trends <- prepare_data_trends(data_monitoring_aq, data_monitoring_met)

# trend analysis and result aggregation (takes a while)
fun <- function(x) {
  print(x)
  trends <- derive_trends_per_pollutant(data_trends, pollutant = x, reference_year_fun = reference_year, yearmin_per_site = yearmin_per_site)
  return(trends)
}
trends <- purrr::map(pollutants, fun)
trends <- dplyr::bind_rows(trends)

# prepare relative trend results for plotting
trends_relative <- prepare_trend_results(trends, reference_year_fun = reference_year, nmin_sites_fun = nmin_sites)




# plot results
theme_custom <-
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.line.x = ggplot2::element_line(color = "gray30"),
    axis.ticks = ggplot2::element_line(color = "gray30"),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank()
  )

trends_relative$all |> 
  ggplot(aes(x = year, y = value, color = type)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01)) + 
  facet_grid(pollutant~site, scales = "free_y")

trends_relative$agg |> 
  dplyr::filter(type == "Trend") |> 
  ggplot(aes(x = year, y = `relative Immission` - 1)) + 
  geom_hline(yintercept = 0, color = "gray60", linetype = 2) +
  geom_segment(
    data = tibble(pollutant = airquality.methods::longpollutant(pollutants), refyear = reference_year(pollutants)), 
    mapping = aes(x = refyear, y = -Inf, yend = 0), color = "gray60", linetype = 2, inherit_aes = FALSE
    ) +
  geom_line(data = dplyr::filter(trends_relative$all, type == "Trend"), mapping = aes(group = site), color = "gray80") +
  geom_point(data = dplyr::filter(trends_relative$all, type == "Trend"), shape = 21, fill = "white", color = "gray80", size = 1) +
  geom_line(linewidth = 1, color = "steelblue") +
  # geom_point(data = dplyr::filter(results_y1, type == "gemessen"), shape = 21, fill = "white", color = "gray80") +
  # geom_point(mapping = aes(size = n), shape = 21, fill = "white") +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.01,0.01)) + 
  # scale_color_manual(name = "Grundlage", values = c("Trend" = "steelblue", "gemessen" = "gray90")) +
  scale_size_binned(name = "Anzahl\nMessorte", breaks = c(-Inf,4,6,8,Inf), range = c(0.25,3)) +
  theme_custom + 
  facet_wrap(pollutant~., axes = "all", nrow = 2)











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
    dplyr::rename(
      value_refyear = value,
      # emission_refyear = emission
    ) |>
    dplyr::right_join(trends, by = c("site", "parameter", "type")) |>
    dplyr::rename(pollutant = parameter) |> 
    dplyr::mutate(
      "relative Immission" = value / value_refyear,
      # "relative Emission" = emission / emission_refyear,
      pollutant = airquality.methods::longpollutant(pollutant),
      `relative Immission` = ifelse(site == "Opf_Balsberg" & year < 2009, NA, `relative Immission`) # due to strong traffic changes at this site
    ) |>
    dplyr::select(year, site, pollutant, type, value, `relative Immission`) #, `relative Emission`) |>
  # tidyr::gather(parameter, value, -year, -pollutant)
  
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
      `relative Immission` = ifelse(n < nmin, NA, `relative Immission`)
      ) |> 
      dplyr::select(-nmin)
  
  return(list(all = trends, agg = trends_agg))
}




# results |> 
#   purrr::map(function(x) x$data) |> 
#   dplyr::bind_rows() |> 
#   ggplot(aes(x = date, y = value, color = type)) +
#   geom_line() +
#   facet_wrap(site~.)

# # ...
# nmin <- 1
# data_monitoring_aq <- airquality.methods::read_local_csv("inst/extdata/output/data_airquality_monitoring_y1.csv", locale = readr::locale(encoding = "UTF-8"))
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



