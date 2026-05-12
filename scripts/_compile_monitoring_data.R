# compiling air quality monitoring data from several sites in the Canton of Zürich by Ostluft and NABEL monitoring networks


# load monitoring datasets from airquality.data (see here: https://github.com/awelZH/airquality.data)
# TODO: ... replace airquality.data as soon as useful api methods are available for this kind of data
# ---
# regular monitoring network data
data_monitoring_aq <- airquality.data::data_monitoring_aq_y1
data_monitoring_aq <- 
  data_monitoring_aq |> 
  dplyr::filter(canton == "ZH" | site %in% c("Zürich-Kaserne", "Dübendorf-EMPA")) |> # only sites in Canton Zürich & the only NABEL-sites in Canton Zürich
  dplyr::select(-canton)
# data_monitoring_ndep <- airquality.data::data_monitoring_ndep


# pre-compiled nitrogen deposition data based on monitoring, raster data and statistical models based on NABEL data
# TODO: integrate all functions appropriately

recode_ecosys <- function(data, levels = rev(c("kein empf. Ökosys.", "Trockenrasen", "Flachmoor", "Hochmoor", "Wald"))) {
  
  data |> 
    mutate(
      ecosys = ifelse(stringr::str_detect(ecosys, "wald"), "Wald", ecosys),
      ecosys = ifelse(ecosys == "Feuchtgebiet", "Flachmoor", ecosys),
      ecosys = factor(ecosys, levels = !!levels)
    )
}

ostluft_siteclass <- function(gve, nfert) {
  
  siteclass <-
    dplyr::case_when(
      when_all(nfert <= 50, gve <= 5000) ~ "tief",
      when_any(
        when_all(nfert > 50, nfert <= 100, gve <= 5000),
        when_all(gve > 5000, gve <= 10000, nfert <= 100),
      ) ~ "mittel",
      when_any(nfert > 100, gve > 10000) ~ "hoch"
    )
  
  return(factor(siteclass, levels = c("hoch", "mittel", "tiel")))
  
}

cut_emissions_1km <- function(emission, cuts = c(0,10,30,Inf), labels = c("tief", "mittel", "hoch")) {
  
  cut(emission, breaks = cuts, include.lowest = TRUE, labels = labels)
  
}

derive_source_cat <- function(data) {
  
  data |> 
    dplyr::mutate(
      source_cat = dplyr::case_when(
        stringr::str_detect(parameter, "NO") ~ "aus NOx-Quellen",
        stringr::str_detect(parameter, "NH") ~ "aus NH3-Quellen"
      )
    )
  
}

cut_estimated <- function(estimated, cuts = c(0,5,12,Inf), labels = c("<5 kg-N","5-12 kg-N",">12 kg-N")) {
  
  cut(estimated, breaks = cuts, include.lowest = TRUE, labels = labels)
  
}

cut_frac_estimated <- function(frac_estimated, cuts = c(0,0.33,0.66,1), labels = c("<33%","33-66%",">66%")) {
  
  cut(frac_estimated, breaks = cuts, include.lowest = TRUE, labels = labels)
  
}

aggregate_ndep <- function(data, additional_groups = NULL) {
  
  groups <- c("site", "year", "ecosys", "siteclass", "emissionclass", "canton", "cln", "unit", additional_groups)
  
  data |> 
    dplyr::group_by_at(groups) |> 
    dplyr::mutate(datasource = paste(unique(datasource), collapse = ",")) |>
    dplyr::group_by_at(c(groups, "datasource", "source")) |> 
    dplyr::summarise(
      estimated = sum(deposition * part_est),
      deposition = sum(deposition)
    ) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(
      estimated_class = cut_estimated(estimated),
      frac_estimated_class = cut_frac_estimated(estimated / deposition)
    )
  
}

site_meta_ndep <- 
  airquality.data::site_meta_ndep |> 
  dplyr::mutate(
    siteclass = ostluft_siteclass(gve_5km, n_fertilization_5km),
    emissionclass = cut_emissions_1km(nh3_emission_1km)
  ) |> 
  dplyr::select(site, ecosys, x, y, masl, siteclass, emissionclass, gve_5km, nh3_emission_1km, cln) |> 
  recode_ecosys()

data_monitoring_ndep_pars <- # parameter-specific
  airquality.data::data_monitoring_ndep_y1 |> 
  derive_source_cat() |> 
  recode_ecosys() |> 
  left_join(site_meta_ndep, by = c("site", "ecosys")) |> 
  dplyr::mutate(
    metric = factor("Jahressumme"),
    pollutant = factor("Ndep")
    ) |> 
  dplyr::rename(deposition = value) |> 
  dplyr::select(year, site, canton, x, y, masl, ecosys, cln, siteclass, emissionclass, pollutant, metric, parameter, deposition, unit, part_est, source_cat, sampling, datasource, source) |> 
  dplyr::filter(canton == "ZH") 

data_monitoring_ndep <- # total ndep
  data_monitoring_ndep_pars |> 
  aggregate_ndep(additional_groups = c("x", "y", "masl", "pollutant", "metric")) 


# write output datasets & clean up:
# ---
airquality.methods::write_local_csv(data_monitoring_aq, file = "inst/extdata/output/data_airquality_monitoring_y1.csv")
airquality.methods::write_local_csv(data_monitoring_ndep_pars, file = "inst/extdata/output/data_ndep_pars_monitoring_y1.csv")
airquality.methods::write_local_csv(data_monitoring_ndep, file = "inst/extdata/output/data_ndep_monitoring_y1.csv")
rm(list = c("data_monitoring_aq", "data_monitoring_ndep", "site_meta_ndep"))
