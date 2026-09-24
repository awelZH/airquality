#' Wrangle data from air quality monitoring and meteoswiss (d1) for statistical meteo-normalisation analysis
#'
#' @param data_aq
#' @param data_met
#' @param cantons Cantons whose monitoring sites are kept.
#'
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
