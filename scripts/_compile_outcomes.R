# Derive selected health outcomes per year from population-weighted mean data
# TODO: use upcoming R-package from SwissTPH et al. for calculations instead of own functions ...
# TODO: finalise ressources.csv


# read datasets ...
# ---
# =>  read input-metadata (crf, lower threshold concentration etc)
outcomes_meta <- 
  airquality.methods::filter_ressources(ressources, 24) |> 
  airquality.methods::read_local_csv(locale = readr::locale(encoding = "UTF-8")) |> 
  dplyr::select(-lower_conc_threshold_source, -min_conc_threshold, -crf_source, -comment, -threshold_unit, -crf_unit, -min_conc_threshold_source)

# => get Canton Zurich yearly mortality cases from opendata.swiss
# TODO: OGD dataset ...
# data_deathrates <- read_opendataswiss(filter_ressources(ressources, 25), source = "Statistisches Amt Kanton Zürich")
data_mortality <- airquality.methods::read_local_csv("inst/extdata/tod_nat_gatu.csv", delim = ",", locale = readr::locale(encoding = "UTF-8"))

# => read Swiss life-expectancy data (BFS Kohortensterbetafeln)
data_life_exp <- airquality.methods::read_bfs_life_expectancy_data()

# => read population weighted mean data
data_expo_weighmean <- airquality.methods::read_local_csv("inst/extdata/output/data_exposition_weighted_means_canton.csv", locale = readr::locale(encoding = "UTF-8")) 

# prepare datasets ...
# ---
# => number of deaths in Canton Zürich
data_mortality <- airquality.methods::prepare_mortality(data_mortality)  #FIXME once original dataset is adjusted

# => add life expectancy in Switzerland
data_life_exp <- airquality.methods::prepare_life_expectancy_data(data_life_exp)
data_mortality <- 
  data_life_exp |> 
  dplyr::select(-source) |> 
  dplyr::right_join(data_mortality, by = c("sex", "year_of_birth", "age")) 


# estimate health outcomes ...
# ---
# => derive preliminary deaths
data_deaths <- 
  data_mortality |> 
  dplyr::filter(age >= 30) |> # just to be sure
  dplyr::group_by(year = year_of_death) |> 
  dplyr::summarise(deaths = sum(frequency, na.rm = TRUE)) |> 
  dplyr::ungroup()

results_deaths <- estimate_all_prelim_deaths(data_expo_weighmean, data_deaths, outcomes_meta)

# # visual check
# results_deaths |>
#   ggplot(aes(x = year, y = outcome, fill = scenario, group = scenario)) +
#   geom_bar(color = NA, stat = "identity") +
#   geom_segment(data = . %>% dplyr::filter(scenario == "tatsächliche Belastung"), mapping = aes(y = outcome_lower, yend = outcome_upper)) +
#   facet_wrap(parameter~.)












estimate_prelim_deaths <- function(data_expo, data_deaths, data_pars, 
                                   parameter, year, 
                                   rrf_shape = "log_linear", nsim = 1000) {
  
  pars <- 
    list(
      outcome = data_pars$outcome_type,
      rrf = list(shape = rrf_shape, central = data_pars$crf, lower = data_pars$crf_lower, upper = data_pars$crf_upper, incr = data_pars$crf_conc_increment),
      cutoff = list(central = data_pars$lower_conc_threshold, lower = pmin(data_expo$concentration_min, data_pars$lower_conc_threshold), upper = data_pars$lower_conc_threshold) #lower & upper doesn't really work since the function expects 95% CI instead of alternative scenarios
    )
  
  results_rr <- 
    healthiar::attribute_health(
      geo_id_micro = c("actual","base"),
      approach_risk = "relative_risk",
      exp_central = c(data_expo$population_weighted_mean, data_expo$population_weighted_mean_base), # μg / m^3, population weighted mean
      erf_shape = pars$rrf$shape, # erf = exposure-response function
      rr_central = pars$rrf$central,
      rr_lower = pars$rrf$lower,
      rr_upper = pars$rrf$upper,
      rr_increment = pars$rrf$incr,  # μg / m^3
      cutoff_central = pars$cutoff$central, # μg / m^3, WHO guideline value concentration
      bhd_central = data_deaths$deaths # bhd = baseline health data (here: natural cause deaths for age >= 30)
    )
  
  results_montecarlo <- healthiar::summarize_uncertainty(results_rr, n_sim = nsim)
  
  results <-
    results_montecarlo$uncertainty_main |> 
    dplyr::rename(
      scenario = geo_id_micro,
      estimate = impact_ci,
      outcome = impact
    ) |> 
    dplyr::mutate(
      year = !!year,
      parameter = !!parameter, 
      outcome_type = !!pars$outcome
    ) |> 
    dplyr::select(year, parameter, outcome_type, scenario, estimate, outcome)
  
  return(results)
}


estimate_all_prelim_deaths <- function(data_expo_weighmean, data_deaths, outcomes_meta){
  
  data_expo <- 
    data_expo_weighmean |> 
    dplyr::filter(parameter %in% outcomes_meta$parameter & year %in% !!data_deaths$year) |> 
    dplyr::group_split(year, parameter)
  
  results_deaths <-
    data_expo |> 
    purrr::map(function(x) {
      x <- dplyr::mutate(x, population_weighted_mean_base = ifelse(is.na(population_weighted_mean_base), population_weighted_mean, population_weighted_mean_base))
      deaths <- dplyr::filter(data_deaths, year == !!x$year) 
      pars <- dplyr::filter(outcomes_meta, parameter == !!x$parameter)
      estimate_prelim_deaths(x, deaths, pars, x$parameter, x$year)
    }) |> 
    dplyr::bind_rows() |> 
    tidyr::spread(scenario, outcome) |>
    dplyr::mutate(avoided = ifelse(actual - base < 0, actual - base, NA)) |>
    dplyr::select(-base) |> 
    tidyr::gather(scenario, outcome, -year, -parameter, -outcome_type, -estimate)
  
  results_deaths <-
    data_expo_weighmean |> 
    dplyr::mutate(
      base_year = ifelse(is.na(base_year) & is.na(population_weighted_mean_base), year, base_year)
    ) |> 
    dplyr::select(year, pollutant, metric, parameter, base_year, population, source) |> 
    dplyr::right_join(results_deaths, by = c("year", "parameter")) |> 
    tidyr::spread(estimate, outcome) |> 
    dplyr::rename(
      outcome = central_estimate,
      outcome_lower = lower_estimate, 
      outcome_upper = upper_estimate
    ) |> 
    dplyr::mutate(
      outcome = ifelse(is.na(outcome) & scenario == "avoided", 0, outcome),
      scenario = dplyr::case_when(
        scenario == "actual" ~ "tatsächliche Belastung", 
        scenario == "avoided" ~ paste0("vermieden vs. ", base_year)
      ),
      outcome_type = dplyr::recode(outcome_type, "natural cause mortality for age >= 30" = "vorzeitige Todesfälle"),
      outcome_delta_min_conc = 0 #TODO for later: add alternative scenario with lowest observed concentration instead of WHO
    ) |> 
    dplyr::select(year, pollutant, metric, parameter, outcome_type, population, scenario, outcome, outcome_lower, outcome_upper, outcome_delta_min_conc)
  
  return(results_deaths)
}








# data_preliminary_deaths <- airquality.methods::prepare_preliminary_deaths(data_expo_weighmean, data_mortality, outcomes_meta)

# => derive lifeyears lost / decrease in life expectancy
# TODO ...

# => combine
# TODO ...
data_outcomes <- results_deaths


# write output datasets & clean up:
# ---
airquality.methods::write_local_csv(data_outcomes, file = "inst/extdata/output/data_health_outcomes.csv")
rm(list = c("outcomes_meta", "data_mortality", "data_life_exp", "data_expo_weighmean", "data_outcomes", "data_preliminary_deaths"))


