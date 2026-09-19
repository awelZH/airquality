# Air quality monitoring in the Canton of Zurich (Ostluft and NABEL): yearly pollutant values and
# nitrogen deposition in sensitive ecosystems, from the pre-compiled datasets of airquality.data.
#
# pollutants: data_monitoring_aq_y1 -> prepare_monitoring_airquality()
# nitrogen deposition: site_meta_ndep -> prepare_ndep_site_meta()
#                      data_monitoring_ndep_y1 -> prepare_ndep_parameters() (one row per parameter)
#                      -> aggregate_ndep() (total deposition per site and year)
# Input checks (check_columns()) stop with a clear message if a dataset changed.


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


# ---- nitrogen deposition: classes ------------------------------------------------------------

#' Recode ecosystem types into the categories of the nitrogen deposition analysis
#'
#' Forest types (Mischwald, Laubwald, Nadelwald) become "Wald", "Feuchtgebiet" becomes
#' "Flachmoor", settlements ("Siedlungen") count as "kein empf. Ökosys." (user decision
#' 2026-09-19). Other types become `NA`, with a warning.
#'
#' @param ecosys Character vector of ecosystem types.
#' @param levels Categories in plot order.
#'
#' @return Factor with `levels`.
#'
#' @keywords internal
recode_ecosystems <- function(ecosys, levels = c("Wald", "Hochmoor", "Flachmoor", "Trockenrasen", "kein empf. Ökosys.")) {
  recoded <- dplyr::case_when(
    stringr::str_detect(ecosys, "wald") ~ "Wald",
    ecosys == "Feuchtgebiet" ~ "Flachmoor",
    ecosys == "Siedlungen" ~ "kein empf. Ökosys.",
    .default = ecosys
  )
  unknown <- setdiff(stats::na.omit(recoded), levels)
  if (length(unknown) > 0) {
    cli::cli_warn("Unknown ecosystem type{?s} {.val {unknown}} set to NA.")
  }
  factor(recoded, levels = levels)
}


#' Ostluft site class by the agricultural pressure within 5 km
#'
#' "tief": fertilisation <= 50 and livestock <= 5000; "hoch": fertilisation > 100 or livestock >
#' 10000; "mittel" in between. Thresholds of the Ostluft site classification.
#'
#' @param gve Livestock units within 5 km (GVE).
#' @param n_fertilization Nitrogen fertilisation within 5 km (kg N/ha/a).
#' @param gve_breaks,n_fertilization_breaks Lower and upper threshold.
#'
#' @return Factor with levels "hoch", "mittel", "tief".
#'
#' @keywords internal
classify_ostluft_siteclass <- function(gve, n_fertilization, gve_breaks = c(5000, 10000), n_fertilization_breaks = c(50, 100)) {
  siteclass <- dplyr::case_when(
    n_fertilization <= n_fertilization_breaks[1] & gve <= gve_breaks[1] ~ "tief",
    n_fertilization > n_fertilization_breaks[2] | gve > gve_breaks[2] ~ "hoch",
    n_fertilization <= n_fertilization_breaks[2] & gve <= gve_breaks[2] ~ "mittel"
  )
  factor(siteclass, levels = c("hoch", "mittel", "tief"))
}


#' Classify values into labelled classes (lower bound of the first class included)
#'
#' @param x Numeric vector.
#' @param breaks Class boundaries.
#' @param labels Class labels.
#'
#' @return Factor with `labels` as levels.
#'
#' @keywords internal
classify <- function(x, breaks, labels) {
  cut(x, breaks = breaks, include.lowest = TRUE, labels = labels)
}


#' NH3 emission class of a site (emissions within 1 km)
#'
#' @param emission NH3 emission within 1 km.
#' @param breaks,labels Class boundaries and labels.
#'
#' @return Factor "tief", "mittel", "hoch".
#'
#' @keywords internal
classify_nh3_emission <- function(emission, breaks = c(0, 10, 30, Inf), labels = c("tief", "mittel", "hoch")) {
  classify(emission, breaks, labels)
}


#' Class of the estimated (modelled) part of the nitrogen deposition
#'
#' @param estimated Estimated deposition in kg N/ha/a.
#' @param breaks,labels Class boundaries and labels.
#'
#' @return Factor "<5 kg-N", "5-12 kg-N", ">12 kg-N".
#'
#' @keywords internal
classify_estimated <- function(estimated, breaks = c(0, 5, 12, Inf), labels = c("<5 kg-N", "5-12 kg-N", ">12 kg-N")) {
  classify(estimated, breaks, labels)
}


#' Class of the estimated fraction of the nitrogen deposition
#'
#' @param frac_estimated Estimated part divided by the total deposition (0–1).
#' @param breaks,labels Class boundaries and labels.
#'
#' @return Factor "<33%", "33-66%", ">66%".
#'
#' @keywords internal
classify_frac_estimated <- function(frac_estimated, breaks = c(0, 0.33, 0.66, 1), labels = c("<33%", "33-66%", ">66%")) {
  classify(frac_estimated, breaks, labels)
}


#' Source category of a nitrogen deposition parameter
#'
#' @param parameter Parameter names, e.g. "NO2-N_ddep" or "NH4-N_bdep".
#'
#' @return "aus NOx-Quellen" (names containing "NO"), "aus NH3-Quellen" (names containing "NH")
#'   or `NA`.
#'
#' @keywords internal
derive_source_category <- function(parameter) {
  dplyr::case_when(
    stringr::str_detect(parameter, "NO") ~ "aus NOx-Quellen",
    stringr::str_detect(parameter, "NH") ~ "aus NH3-Quellen"
  )
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
      siteclass = classify_ostluft_siteclass(gve_5km, n_fertilization_5km),
      emissionclass = classify_nh3_emission(nh3_emission_1km),
      ecosys = recode_ecosystems(ecosys)
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
      source_cat = derive_source_category(parameter),
      ecosys = recode_ecosystems(ecosys)
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


#' Total nitrogen deposition per site and year, with its estimated part
#'
#' Sums the deposition over all parameters; the estimated (modelled instead of measured) part is
#' the sum of `deposition * part_est`. The data sources of a site and year are listed
#' comma-separated. Rows are sorted by the grouping columns.
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
    dplyr::mutate(datasource = paste(unique(datasource), collapse = ","), .by = dplyr::all_of(groups)) |>
    dplyr::summarise(
      estimated = sum(deposition * part_est),
      deposition = sum(deposition),
      .by = dplyr::all_of(groups_source)
    ) |>
    dplyr::arrange(dplyr::pick(dplyr::all_of(groups_source))) |>
    dplyr::mutate(
      estimated_class = classify_estimated(estimated),
      frac_estimated_class = classify_frac_estimated(estimated / deposition)
    )
}
