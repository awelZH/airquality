# Analysis settings: all constants of the analysis in one place, grouped by topic (see CLAUDE.md, decision 7).
# Pure assignments, no packages attached: sourced by scripts/_setup.R, the plot scripts and the report.
# Settings that depend on the date (year_last, emis_year_max) change with the calendar year.

# => general
# last year analysed = current year - year_offset (data of a year usually appear in the following year)
year_offset <- 1
year_last <- lubridate::year(Sys.Date()) - year_offset

# reference year for all pollutants in exposition & outcomes calculation
base_scenario_year <- 2015

# map projection CRS = CH1903+ / LV95 throughout analysis
crs <- 2056

# => emissions
# last year kept in the emission data: EMIKAT projections beyond the current year are dropped, and it is the newest
# vehicle model year in the RSD data
emis_year_max <- lubridate::year(Sys.Date())

# subsectors whose mean yearly share of a pollutant's emissions is below this go into "verschiedene"
emis_subsector_min_share <- 0.05

# maximum number of subsectors per pollutant and sector, including "verschiedene"
emis_subsectors_max <- 4

# => monitoring
# cantons whose monitoring sites are compiled (the NABEL sites Zürich-Kaserne and Dübendorf-EMPA carry canton "ZH")
mon_cantons <- "ZH"

# => trends
# years to consider for analysis and later plotting
trend_years <- 1990:year_last

# cantons whose monitoring sites are analysed
trend_cantons <- "ZH"

# parameters for trend analysis
trend_parameters <- c("PM2.5", "PM10", "NOx", "eBC", "NHx", "Ndep", "O3_max_98p_m1")

# meteorological variables (daily) used for meteo-normalisation
trend_vars_d1 <- c("T", "T_max_min10", "Hr", "StrGlo", "p", "WVs", "WD", "RainSum")

# minimum number of years available for trend analysis per site
trend_yearmin_per_site <- 4

# reference year for relative trends with monitoring data
trend_reference_year <- function(parameter, base_year = 2015) {
  dplyr::case_when(
    parameter == "PM2.5" ~ 2021,
    parameter == "eBC" ~ 2020,
    parameter == "NHx" ~ 2020,
    parameter == "NH3" ~ 2020,
    parameter == "Ndep" ~ 2020,
    TRUE ~ base_year
  )
}

# minimum number of sites per year for which median trend is derived
trend_nmin_sites <- function(parameter) {
  dplyr::case_when(
    parameter == "PM10" ~ 3,
    parameter == "PM2.5" ~ 3,
    parameter == "eBC" ~ 2, #! simply not a lot high timeres sites available
    parameter == "NOx" ~ 5,
    parameter == "NO2" ~ 5,
    parameter == "O3" ~ 5,
    parameter == "O3_nb_h1>120" ~ 5,
    parameter == "O3_max_h1" ~ 5,
    parameter == "O3_nb_d1_max_h1>120" ~ 5,
    parameter == "O3_max_98p_m1" ~ 5,
    parameter == "O3_peakseason_mean_d1_max_mean_h8gl" ~ 5,
    parameter == "NHx" ~ 4,
    parameter == "Ndep" ~ 4,
    TRUE ~ 4
  )
}

# => exposition
# years to analyse: STATPOP is available from 2010 on
expo_years <- 2010:year_last

# subtract STATPOP collector pixels (inhabitants that cannot be located) and spread them over their municipality?
expo_correct_noloc <- TRUE

# years in which PM2.5 is derived from PM10 (no PM2.5 raster data before 2015)
expo_years_pm25_from_pm10 <- min(expo_years):2014

# minimum number of monitoring sites per year for fitting the O3 peak-season model
expo_o3_nmin_sites <- 7

# => health outcomes
# share of the median year a year's deaths need to reach to count as complete; the mortality data of the
# current year arrive piece by piece and would give far too few premature deaths
outcomes_min_year_share <- 0.8

# youngest age affected by the long-term exposure (the exposure-response functions refer to adults >= 30)
outcomes_min_age <- 30

# deaths assumed in a cell of the mortality data suppressed for privacy (1 to 3 deaths)
outcomes_suppressed_deaths <- 2

# shape of the exposure-response functions (relative risks per concentration increment)
outcomes_erf_shape <- "log_linear"


# => plots
# years to consider for plotting
plot_years <- 1995:year_last

# number of latest years for plotting relative threshold comparison
plot_n_years <- 3

# parameters to include for timeseries plotting
plot_parameters_timeseries <- c("NO2", "PM10", "PM2.5", "O3_max_98p_m1", "O3_peakseason_mean_d1_max_mean_h8gl", "eBC")

# parameters to include for exposition plotting
plot_parameters_exposition <- c("NO2", "O3_max_98p_m1", "PM10", "PM2.5", "O3_peakseason_mean_d1_max_mean_h8gl")

# parameters to include for health outcome plotting
plot_parameters_outcomes <- c("PM2.5", "NO2", "O3_peakseason_mean_d1_max_mean_h8gl")

# reference year for relative emission trends (independent of base_scenario_year)
plot_reference_year_emissions <- 2015

