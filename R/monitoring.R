# Air quality monitoring in the Canton of Zurich (Ostluft and NABEL): yearly pollutant values and
# nitrogen deposition in sensitive ecosystems, from the pre-compiled datasets of airquality.data.
#
# pollutants: data_monitoring_aq_y1 -> prepare_monitoring_airquality()
# nitrogen deposition: site_meta_ndep -> prepare_ndep_site_meta()
#                      data_monitoring_ndep_y1 -> prepare_ndep_parameters() (one row per parameter)
#                      -> aggregate_ndep() (total deposition per site and year)
# Input checks (check_columns()) stop with a clear message if a dataset changed. The classes of the nitrogen
# deposition (ecosystems, site classes, estimated part, source category) come from airquality.methods.


# ---- pollutants ----------------------------------------------------------------------------

#' Select the yearly pollutant values of the monitoring sites in the given cantons
#'
#' @param data Yearly monitoring data (`airquality.data::data_monitoring_aq_y1`) with a column
#'   `canton`; sites without canton (e.g. NABEL sites outside the region) are dropped.
#' @param cantons Cantons whose sites are kept.
#'
#' @return `data` without the column `canton`.
#'
#' @keywords internal
prepare_monitoring_airquality <- function(data, cantons) {
  check_columns(data, c("site", "canton"), "monitoring data (airquality.data::data_monitoring_aq_y1)")

  data |>
    dplyr::filter(canton %in% cantons) |>
    dplyr::select(-canton)
}


# ---- nitrogen deposition: preparation and aggregation ------------------------------------------

#' Classify the nitrogen deposition sites
#'
#' @param site_meta Site metadata (`airquality.data::site_meta_ndep`).
#'
#' @return One row per site and ecosystem with `site`, `ecosys` (recoded), `x`, `y`, `masl`,
#'   `siteclass`, `emissionclass`, `gve_5km`, `nh3_emission_1km`, `cln` (critical load).
#'
#' @keywords internal
prepare_ndep_site_meta <- function(site_meta) {
  check_columns(site_meta, c("site", "ecosys", "x", "y", "masl", "gve_5km", "n_fertilization_5km",
                             "nh3_emission_1km", "cln"), "nitrogen deposition site metadata (airquality.data::site_meta_ndep)")

  site_meta |>
    dplyr::mutate(
      siteclass = airquality.methods::classify_ostluft_siteclass(gve_5km, n_fertilization_5km),
      emissionclass = airquality.methods::classify_nh3_emission(nh3_emission_1km),
      ecosys = airquality.methods::recode_ecosystems(ecosys)
    ) |>
    dplyr::select(site, ecosys, x, y, masl, siteclass, emissionclass, gve_5km, nh3_emission_1km, cln)
}


#' Nitrogen deposition per site, year and parameter, with the site metadata
#'
#' @param data Yearly nitrogen deposition per parameter (`airquality.data::data_monitoring_ndep_y1`).
#' @param site_meta Output of [prepare_ndep_site_meta()].
#' @param cantons Cantons whose sites are kept.
#'
#' @return One row per site, ecosystem, year and parameter (`data_ndep_pars_monitoring_y1.csv`).
#'
#' @keywords internal
prepare_ndep_parameters <- function(data, site_meta, cantons) {
  check_columns(data, c("year", "site", "canton", "ecosys", "parameter", "value", "unit", "part_est", "sampling",
                        "datasource", "source"), "nitrogen deposition data (airquality.data::data_monitoring_ndep_y1)")

  data |>
    dplyr::mutate(
      source_cat = airquality.methods::derive_source_category(parameter),
      ecosys = airquality.methods::recode_ecosystems(ecosys)
    ) |>
    dplyr::left_join(site_meta, by = dplyr::join_by(site, ecosys)) |>
    dplyr::mutate(
      metric = factor("Jahressumme"),
      pollutant = factor("Ndep")
    ) |>
    dplyr::rename(deposition = value) |>
    dplyr::select(year, site, canton, x, y, masl, ecosys, cln, siteclass, emissionclass, pollutant, metric, parameter,
                  deposition, unit, part_est, source_cat, sampling, datasource, source) |>
    dplyr::filter(canton %in% cantons)
}


#' List the data sources of a group once, sorted
#'
#' Entries may already list several sources ("FUB, Ostluft"), so they are split first. Without
#' this, the same set of sources appeared in several spellings and with duplicates, depending on
#' the row order of the input.
#'
#' @param datasource Character vector of data sources, comma-separated.
#'
#' @return One comma-separated string, or `NA` if there is no source.
#'
#' @keywords internal
combine_sources <- function(datasource) {
  sources <- unlist(stringr::str_split(datasource, ","))
  sources <- stringr::str_trim(sources)
  sources <- unique(sources[!is.na(sources) & sources != ""])
  if (length(sources) == 0) {
    return(NA_character_)
  }
  paste(sort(sources, method = "radix"), collapse = ",")
}


#' Total nitrogen deposition per site and year, with its estimated part
#'
#' Sums the deposition over all parameters; the estimated (modelled instead of measured) part is
#' the sum of `deposition * part_est`. The data sources of a site and year are listed once and
#' sorted ([combine_sources()]). Rows are sorted by the grouping columns, so the result does not
#' depend on the row order of the input.
#'
#' @param data Output of [prepare_ndep_parameters()].
#' @param additional_groups Further columns to keep (constant per site and year).
#'
#' @return One row per site and year (`data_ndep_monitoring_y1.csv`) with `estimated`,
#'   `deposition`, `estimated_class` and `frac_estimated_class`.
#'
#' @keywords internal
aggregate_ndep <- function(data, additional_groups = NULL) {
  groups <- c("site", "year", "ecosys", "siteclass", "emissionclass", "canton", "cln", "unit", additional_groups)
  groups_source <- c(groups, "datasource", "source")

  data |>
    dplyr::mutate(datasource = combine_sources(datasource), .by = dplyr::all_of(groups)) |>
    dplyr::summarise(
      estimated = sum(deposition * part_est),
      deposition = sum(deposition),
      .by = dplyr::all_of(groups_source)
    ) |>
    dplyr::arrange(dplyr::pick(dplyr::all_of(groups_source))) |>
    dplyr::mutate(
      estimated_class = airquality.methods::classify_estimated(estimated),
      frac_estimated_class = airquality.methods::classify_frac_estimated(estimated / deposition)
    )
}
