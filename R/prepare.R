#' Prepare data from ressources.csv for use in scripts
#'
#' @param ressources
#'
#' @export
prepare_ressources <- function(ressources) {

  ressources <-
    ressources |>
    dplyr::mutate(
      get = dplyr::case_when(
        stringr::str_detect(DOWNLOAD_URL, "inst/extdata") ~ paste(DOWNLOAD_URL, DATASET_NAME, sep = "/"),
        DOWNLOAD_SOURCE == "swisstopo" ~ DATASET_NAME,
        TRUE ~ DOWNLOAD_URL
      )
    )

  return(ressources)
}


#' Prepare mortality data
#'
#' @param data_mortality
#'
#' @export
prepare_mortality <- function(data_mortality) {

  data_mortality <-
    data_mortality |>
    dplyr::mutate(alterkat = ifelse(alterkat == 290, 291, alterkat)) |> #FIXME once original dataset is adjusted
    dplyr::filter(tukat == "krankheitsbedingt" & alterkat != 290) # 290 = younger than 30 years

  data_freq_na <-
    data_mortality |>
    dplyr::filter(alterkat == 291) |>
    dplyr::group_by(jahr, geschlecht) |>
    dplyr::summarise(freq_na = sum(anzahl)) |>
    dplyr::ungroup()

  data_freq_na <-
    data_mortality |>
    dplyr::filter(alterkat == 291) |>
    dplyr::group_by(jahr, geschlecht) |>
    dplyr::summarise(freq_na = sum(anzahl)) |>
    dplyr::ungroup()

  data_freq_na <-
    data_mortality |>
    dplyr::filter(alterkat != 291) |>
    dplyr::group_by(jahr, geschlecht) |>
    dplyr::summarise(na = sum(is.na(anzahl))) |>
    dplyr::ungroup() |>
    dplyr::full_join(data_freq_na, by = c("jahr", "geschlecht"))

  data_mortality <-
    data_mortality |>
    dplyr::filter(alterkat != 291) |>
    left_join(data_freq_na, by = c("jahr", "geschlecht")) |>
    dplyr::mutate(
      anzahl = ifelse(is.na(anzahl), freq_na / na, anzahl), # when NA, then actually < 4 due to privacy protection. So, for better average accuracy distribute alterkat == 291 over all NA cases
      year_of_birth = jahr - alterkat,
      geschlecht = factor(geschlecht),
      source = "Statistisches Amt Kanton Zürich & BFS"
    ) |>
    dplyr::rename(
      age = alterkat,
      frequency = anzahl,
      sex = geschlecht,
      year_of_death = jahr
    ) |>
    dplyr::select(-tukat, -na, -freq_na)

  return(data_mortality)
}


#' Prepare preliminary deaths from population-weighted mean, mortality cases and outcome metadata and derive health outcomes
#'
#' @param data_expo_weighmean
#' @param data_mortality
#' @param outcomes_meta
#' @param conc_threshold
#'
#' @export
prepare_preliminary_deaths <- function(data_expo_weighmean, data_mortality, outcomes_meta, conc_threshold = "lower_conc_threshold") {

  # aggregate mortality
  data_mortality <-
    data_mortality |>
    dplyr::rename(year = year_of_death) |>
    dplyr::group_by(year) |>
    dplyr::summarise(number_of_deaths = sum(frequency)) |>
    dplyr::ungroup()

  # combine and wrangle all input data
  data <-
    data_expo_weighmean |>
    dplyr::filter(parameter %in% unique(outcomes_meta$parameter)) |>
    dplyr::select(-pollutant, -metric, -source, -unit, -concentration_max, -concentration_mean, -concentration_median) |>
    tidyr::gather(scenario, population_weighted_mean, -year, -parameter, -base_year, -population, -concentration_min) |>
    dplyr::left_join(outcomes_meta, by = "parameter") |>
    dplyr::left_join(data_mortality, by = "year") |>
    dplyr::mutate(
      scenario = dplyr::recode(scenario, population_weighted_mean = "tatsächliche Belastung", population_weighted_mean_base = paste0("vermieden vs. ",na.omit(unique(.data$base_year)))),
      concentration_min = ifelse(stringr::str_detect(scenario, "vermieden"), NA, concentration_min)
    ) |>
    dplyr::select(-base_year) |>
    dplyr::filter(!is.na(scenario))

  # calculate outcomes
  data <-
    data |>
    dplyr::filter(scenario == "tatsächliche Belastung") |>
    dplyr::mutate(min_conc_threshold = pmin(concentration_min, lower_conc_threshold)) |>
    calculate_all_outcomes(conc_threshold = "min_conc_threshold") |>
    dplyr::select(year, pollutant, metric, scenario, outcome_type, outcome) |>
    dplyr::rename(outcome_min_conc = outcome) |>
    dplyr::right_join(data, by = c("year", "pollutant", "metric", "scenario", "outcome_type")) |>
    calculate_all_outcomes() |>
    dplyr::mutate(outcome_delta_min_conc = outcome_min_conc - outcome) |>
    dplyr::select(year, pollutant, metric, parameter, population, scenario, outcome_type, outcome, outcome_lower, outcome_upper, outcome_delta_min_conc)

  # restructure dataset
  scen <- unique(data$scenario)[stringr::str_detect(unique(data$scenario), "vermieden")]
  data <-
    data |>
    dplyr::select(year, pollutant, metric, parameter, scenario, outcome_type, outcome, population) |>
    tidyr::spread(scenario, outcome) |>
    dplyr::mutate(!!scen := pmin(0, `tatsächliche Belastung` - !!rlang::sym(scen))) |>
    dplyr::select(-`tatsächliche Belastung`) |>
    tidyr::gather(scenario, outcome, -year, -pollutant, -metric, -parameter, -outcome_type, -population) |>
    dplyr::bind_rows(dplyr::filter(data, scenario == "tatsächliche Belastung")) |>
    dplyr::arrange(pollutant, metric, scenario, year, outcome_type) |>
    dplyr::filter(!is.na(outcome)) |>
    dplyr::select(year, pollutant, metric, parameter, outcome_type,  population, scenario, outcome, outcome_lower, outcome_upper, outcome_delta_min_conc)

  return(data)
}


#' Wrangle life-expectancy data from *.px format into tibble()
#'
#' @param data
#'
#' @keywords internal
prepare_life_expectancy_data <- function(data) {

  # wrangle data
  data <-
    data |>
    tibble::as_tibble() |>
    dplyr::mutate(
      Alter = readr::parse_number(as.character(Alter)),
      Geburtsjahrgang = as.numeric(as.character(Geburtsjahrgang)),
      Geschlecht = dplyr::recode(Geschlecht, Frau = "weiblich", Mann = "männlich"),
      source = "BFS"
    ) |>
    dplyr::filter(stringr::str_detect(Beobachtungseinheit, "Lebensdauer")) |>
    tidyr::spread(Beobachtungseinheit, value) |>
    dplyr::rename(
      age = Alter,
      sex = Geschlecht,
      year_of_birth = Geburtsjahrgang,
      remaining_lifeyears = `Verbleibende Lebensdauer (ex)`
    ) |>
    dplyr::select(sex, year_of_birth, age, remaining_lifeyears, source)

  return(data)
}


#' Wrangle data from air quality monitoring and meteoswiss (d1) for statistical meteo-normalisation analysis
#'
#' @param data_aq
#' @param data_met
#' @param cantons Cantons whose monitoring sites are kept.
#'
#' @export
prepare_data_trends <- function(data_aq, data_met, cantons) {

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


#' Wrangle air pollutant emission data into relative emission trends for according comparison with monitoring data and trends
#'
#' @param emissions
#' @param reference_year_fun
#'
#' @export
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
      pollutant = longpollutant(pollutant),
      type = "emission"
    ) |>
    dplyr::select(year, pollutant, type, `relative Emission`, reference_year)

  return(emissions)
}
