# Air pollutant emissions: the emission inventory of the Canton of Zurich (EMIKAT) and the
# NOx emissions of vehicles measured with remote sensing (RSD).
#
# EMIKAT: raw inventory -> prepare_emissions() -> aggregate_emissions() -> add_emission_colours()
# RSD:    raw measurements -> prepare_rsd() (one row per vehicle with its NOx emission)
#         -> aggregate_rsd_nox() per vehicle group


# ---- emission inventory (EMIKAT) -------------------------------------------------

#' Select and rename the emission inventory data
#'
#' Keeps the latest inventory version (`stand`) of one canton up to `year_max` (the inventory also
#' contains projections), drops zero emissions and subsectors whose emissions are not distributed
#' onto the municipalities in a meaningful way, and renames black carbon `BC` to `eBC` as in the
#' monitoring data.
#'
#' @param data Raw inventory from opendata.swiss with German column names (`jahr`, `substanz`,
#'   `hauptgruppe`, `untergruppe`, `kanton`, `gemeinde`, `einheit`, `stand`, `emission`, ...).
#' @param canton Canton to keep.
#' @param exclude_subsectors Subsectors to drop.
#' @param year_max Last year kept; later years are projections.
#'
#' @return The inventory with English column names `year`, `pollutant`, `sector`, `subsector`,
#'   `canton`, `municipality`, `unit`, `emission` (other columns unchanged), without `stand`.
#'
#' @keywords internal
prepare_emissions <- function(data,
                              canton = "ZH",
                              exclude_subsectors = c("Weitere Punktquellen OL", "Rheinschifffahrt", "Flugverkehr Genf"),
                              year_max = Inf) {
  data |>
    dplyr::rename(
      year = jahr,
      pollutant = substanz,
      sector = hauptgruppe,
      subsector = untergruppe,
      canton = kanton,
      municipality = gemeinde,
      unit = einheit
    ) |>
    dplyr::mutate(
      stand = readr::parse_number(stand),
      pollutant = dplyr::if_else(pollutant == "BC", "eBC", pollutant)
    ) |>
    dplyr::filter(
      stand == max(stand),
      canton == .env$canton,
      year <= year_max,
      emission != 0,
      !(subsector %in% exclude_subsectors)
    ) |>
    dplyr::select(-stand)
}


#' Sum up emissions per year, pollutant, sector and (regrouped) subsector
#'
#' Subsectors are renamed or merged with the lookup table `subsector_new` (e.g. several small
#' subsectors into "verschiedene"); subsectors missing in the lookup keep their name.
#' Groups without emission are dropped.
#'
#' @param data Output of [prepare_emissions()].
#' @param subsector_new Lookup table with columns `subsector` and `subsector_new`
#'   (`inst/extdata/meta/emikat_subsector_new.csv`).
#'
#' @return One row per `year`, `pollutant`, `unit`, `sector`, `subsector_new` and `source`, with
#'   columns `year`, `pollutant`, `metric` ("Jahresmenge"), `unit`, `sector`, `subsector_new`,
#'   `emission`, `source`.
#'
#' @keywords internal
aggregate_emissions <- function(data, subsector_new) {
  lookup <- dplyr::select(subsector_new, subsector, subsector_new)

  data |>
    dplyr::left_join(lookup, by = dplyr::join_by(subsector)) |>
    dplyr::mutate(subsector_new = dplyr::coalesce(subsector_new, subsector)) |>
    dplyr::summarise(
      emission = sum(emission, na.rm = TRUE),
      .by = c(year, pollutant, unit, sector, subsector_new, source)
    ) |>
    dplyr::filter(emission > 0) |>
    dplyr::mutate(metric = "Jahresmenge") |>
    dplyr::select(year, pollutant, metric, unit, sector, subsector_new, emission, source)
}


#' Add plot order and colours per subsector
#'
#' Within each sector, subsectors are ordered by their total emission over all years and
#' pollutants in `data` (largest first), and get a shade of the sector's colour ramp
#' ([airquality.methods::pal_emissions()]). The columns are part of `data_emissions.csv`.
#'
#' @param data Output of [aggregate_emissions()].
#' @param sector_colours Named character vector: one [airquality.methods::pal_emissions()]
#'   palette name per sector.
#'
#' @return `data` with the columns `sector`, `subsector_new`, `rootcol` (palette name), `order`
#'   (plot order over all sectors), `col` (colour) in front, sorted by `year`, `pollutant`,
#'   `sector` and decreasing `emission`.
#'
#' @keywords internal
add_emission_colours <- function(data,
                                 sector_colours = c("Dienstleistungen" = "Gold", "Haushalte" = "Green",
                                                    "Industrie" = "Blue", "Land- und Forstw." = "Purple",
                                                    "Verkehr" = "Gray", "natürl. Emissionen" = "natural")) {
  unknown <- setdiff(unique(data$sector), names(sector_colours))
  if (length(unknown) > 0) {
    cli::cli_abort("No colour for sector{?s} {.val {unknown}} in {.arg sector_colours}.")
  }

  colours <-
    data |>
    dplyr::summarise(emission = sum(emission, na.rm = TRUE), .by = c(sector, subsector_new)) |>
    dplyr::arrange(sector, dplyr::desc(emission)) |>
    dplyr::mutate(
      rootcol = unname(sector_colours[sector]),
      order = dplyr::row_number()
    ) |>
    dplyr::mutate(col = airquality.methods::pal_emissions(dplyr::n(), unique(rootcol)), .by = sector) |>
    dplyr::select(sector, subsector_new, rootcol, order, col)

  colours |>
    dplyr::right_join(data, by = dplyr::join_by(sector, subsector_new)) |>
    dplyr::arrange(year, pollutant, sector, dplyr::desc(emission))
}


# ---- remote sensing (RSD) ----------------------------------------------------------

#' Prepare RSD measurements: one row per vehicle with its NOx emission
#'
#' Computes the vehicle specific power, applies the filter criteria, adds the vehicle metadata
#' (HBEFA NO2 fraction, NOx thresholds) and calculates the NOx emission.
#'
#' @param data Raw RSD data from opendata.swiss in long format: one row per vehicle measurement
#'   (`id`) and `parameter` (`velocity`, `acceleration`, `NO`, `CO2`, `CO`, `HC`, ...).
#' @param meta Vehicle metadata (`inst/extdata/meta/rsd_auxiliary.csv`): `vehicle_type`,
#'   `vehicle_fuel_type`, `vehicle_euronorm`, `parameter`, `value`, `source`, `remark`.
#' @param filters Filter criteria (`inst/extdata/meta/rsd_filters.csv`): `parameter`, `min`, `max`.
#' @param model_year_max Newest vehicle model year kept (upper bound of `vehicleyears`).
#'
#' @return One row per vehicle measurement with the measured concentrations as columns,
#'   `vehicle_specific_power` (kW/t), the metadata columns and `nox_emission` (g/kg fuel);
#'   `vehicle_type` and `vehicle_fuel_type` as factors.
#'
#' @keywords internal
prepare_rsd <- function(data, meta, filters, model_year_max) {
  filters$max[filters$parameter == "vehicleyears"] <- model_year_max

  data_vsp <- prep_vehicle_specific_power(data)

  data |>
    dplyr::select(-unit) |>
    dplyr::filter(!(parameter %in% c("acceleration", "velocity"))) |>
    dplyr::left_join(data_vsp, by = dplyr::join_by(id)) |>
    filter_rsd(filters) |>
    merge_restructure_rsd(meta) |>
    # concentrations as mixing ratios in percent (NO and HC are measured in ppm); the NO2 fraction
    # is taken from HBEFA rather than measured, since NO2 is only measured since RSD model 4500
    dplyr::mutate(nox_emission = calc_rsd_nox_emission(NO = NO / 10^4, p = fraction_no2_hbefa, CO2 = CO2,
                                                       CO = CO, HC = HC / 10^4))
}


#' Vehicle specific power per RSD measurement
#'
#' @param data Raw RSD data in long format (see [prepare_rsd()]).
#'
#' @return One row per `id` with `acceleration` (km/h/s), `velocity` (km/h) and
#'   `vehicle_specific_power` (kW/t).
#'
#' @keywords internal
prep_vehicle_specific_power <- function(data) {
  data |>
    dplyr::filter(parameter %in% c("acceleration", "velocity") & !is.na(value)) |>
    dplyr::select(id, site_roadgrade, parameter, value) |>
    tidyr::pivot_wider(names_from = parameter, values_from = value) |>
    # velocity from km/h and acceleration from km/h/s into m/s and m/s2
    dplyr::mutate(vehicle_specific_power = calc_vsp(velocity * 1000 / 60^2, acceleration * 1000 / 60^2, site_roadgrade)) |>
    dplyr::select(id, acceleration, velocity, vehicle_specific_power)
}


#' Vehicle specific power following Jiménez
#'
#' @param speed Speed in m/s.
#' @param accel Acceleration in m/s2.
#' @param slope Road grade as ratio.
#' @param vsp.a,vsp.b,vsp.c Coefficients of the light duty vehicle approximation.
#' @param vsp.g Gravitational acceleration in m/s2.
#'
#' @return Vehicle specific power in kW/t.
#'
#' @keywords internal
calc_vsp <- function(speed, accel, slope, vsp.a = 1.1, vsp.b = 0.132, vsp.c = 0.000302, vsp.g = 9.81) {
  speed * (vsp.a * accel + (vsp.g * slope) + vsp.b) + (vsp.c * speed^3)
}


#' Apply the RSD filter criteria and turn the measurements into one row per vehicle
#'
#' Keeps vehicles within the model years, velocity, acceleration, vehicle specific power and
#' weight limits (bounds inclusive) and with all concentrations needed for the NOx emission.
#'
#' @param data RSD data in long format with `acceleration`, `velocity` and
#'   `vehicle_specific_power` columns.
#' @param filters Filter criteria with columns `parameter`, `min`, `max`.
#'
#' @return One row per vehicle measurement, one column per measured parameter.
#'
#' @keywords internal
filter_rsd <- function(data, filters) {
  bound <- function(parameter, which) filters[[which]][filters$parameter == parameter]

  data |>
    dplyr::filter(
      vehicle_model_year %in% bound("vehicleyears", "min"):bound("vehicleyears", "max"),
      dplyr::between(acceleration, bound("accelerationrange", "min"), bound("accelerationrange", "max")),
      dplyr::between(velocity, bound("velocityrange", "min"), bound("velocityrange", "max")),
      dplyr::between(vehicle_specific_power, bound("vsprange", "min"), bound("vsprange", "max")),
      vehicle_unloaded_weight <= bound("weightmax", "max"),
      !is.na(value)
    ) |>
    tidyr::pivot_wider(names_from = parameter, values_from = value) |>
    dplyr::filter(!is.na(NO + CO2 + CO + HC))
}


#' Add the vehicle metadata (per Euronorm) to the RSD data
#'
#' Euro5a and Euro5b are merged into Euro5 (they are quite similar).
#'
#' @param data RSD data, one row per vehicle measurement.
#' @param meta Vehicle metadata in long format (see [prepare_rsd()]).
#'
#' @return `data` with one column per metadata parameter; `vehicle_type` and
#'   `vehicle_fuel_type` as factors.
#'
#' @keywords internal
merge_restructure_rsd <- function(data, meta) {
  meta <-
    meta |>
    dplyr::filter(!is_model_year(vehicle_euronorm)) |>
    rsd_meta_wide()

  data |>
    dplyr::mutate(vehicle_euronorm = dplyr::if_else(vehicle_euronorm %in% c("Euro5a", "Euro5b"), "Euro5", vehicle_euronorm)) |>
    dplyr::left_join(meta, by = dplyr::join_by(vehicle_type, vehicle_fuel_type, vehicle_euronorm)) |>
    dplyr::mutate(
      vehicle_type = factor(vehicle_type, levels = c("passenger car", "light duty vehicle")),
      vehicle_fuel_type = factor(vehicle_fuel_type, levels = c("gasoline", "diesel"))
    )
}


#' RSD NOx emission in g/kg fuel
#'
#' @param NO,CO2,CO,HC Concentrations as mixing ratios in percent.
#' @param p NO2 fraction of NOx.
#'
#' @return NOx emission (as NO2) in g/kg fuel.
#'
#' @keywords internal
calc_rsd_nox_emission <- function(NO, p, CO2, CO, HC) {
  Q <- CO / CO2
  Q1 <- HC / CO2
  Q2 <- NO / CO2
  NO_emission <- 30 * Q2 * 860 / ((1 + Q + 6 * Q1) * 12)
  NO_emission * 46 / (30 * (1 - p))
}


#' Mean RSD NOx emission per vehicle group
#'
#' Groups with fewer than `nmin` vehicles (from `filters`) get `NA` statistics; group
#' combinations that do not occur are added with `n = 0` ([airquality.methods::aggregate_groups()]).
#' Per Euronorm or per vehicle model year, the NOx threshold of the group is added. Per year of
#' measurement, all fuel types together are added as `vehicle_fuel_type = "all"`.
#'
#' @param data Output of [prepare_rsd()].
#' @param meta Vehicle metadata in long format (see [prepare_rsd()]); `vehicle_euronorm` holds
#'   either a Euronorm or a vehicle model year.
#' @param filters Filter criteria with columns `parameter`, `min`, `max` (uses `nmin`).
#' @param groups Grouping columns: `vehicle_type`, `vehicle_fuel_type`, `vehicle_euronorm`;
#'   `vehicle_model_year`, `vehicle_type`, `vehicle_fuel_type`; or `year`, `vehicle_fuel_type`.
#'
#' @return One row per group with `pollutant`, `metric`, the `groups`, `emission` (mean),
#'   `unit`, `n`, `standarderror`, `nox_emission_threshold_g_per_kg_fuel` (not per year) and `source`.
#'
#' @keywords internal
aggregate_rsd_nox <- function(data, meta, filters, groups) {
  nmin <- filters$min[filters$parameter == "nmin"]
  per_year <- "year" %in% groups

  if (per_year) {
    data <-
      data |>
      dplyr::mutate(vehicle_fuel_type = "all") |>
      dplyr::bind_rows(data) |>
      dplyr::mutate(year = lubridate::year(date_measured))
  }

  result <-
    data |>
    airquality.methods::aggregate_groups(y = "nox_emission", groups = c(groups, "source"), nmin = nmin) |>
    dplyr::mutate(
      pollutant = "NOx",
      metric = "Mittelwert",
      emission = mean,
      unit = "g/kg fuel"
    )

  if (per_year) {
    return(dplyr::select(result, pollutant, metric, dplyr::all_of(groups), emission, unit, n, standarderror, source))
  }

  if ("vehicle_model_year" %in% groups) {
    meta <-
      meta |>
      dplyr::filter(is_model_year(vehicle_euronorm)) |>
      dplyr::mutate(vehicle_euronorm = as.numeric(vehicle_euronorm)) |>
      dplyr::rename(vehicle_model_year = vehicle_euronorm)
  }

  result |>
    dplyr::left_join(rsd_meta_wide(meta), by = groups) |>
    dplyr::select(pollutant, metric, dplyr::all_of(groups), emission, unit, n, standarderror,
                  nox_emission_threshold_g_per_kg_fuel, source)
}


#' Vehicle metadata in wide format: one column per metadata parameter
#'
#' @param meta Vehicle metadata in long format (see [prepare_rsd()]).
#'
#' @keywords internal
rsd_meta_wide <- function(meta) {
  meta |>
    dplyr::select(-source, -remark) |>
    tidyr::pivot_wider(names_from = parameter, values_from = value)
}


#' Does a `vehicle_euronorm` entry of the RSD metadata hold a vehicle model year?
#'
#' @param x Character vector.
#'
#' @keywords internal
is_model_year <- function(x) stringr::str_detect(x, "^[0-9]{4}$")
