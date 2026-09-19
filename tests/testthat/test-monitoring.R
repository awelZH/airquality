# Unit tests for R/monitoring.R. All inputs are synthetic; no network.

# ---- pollutants ----------------------------------------------------------------------

test_that("prepare_monitoring_airquality() keeps the sites of the given cantons and drops `canton`", {
  data <- tibble::tibble(
    year = 2020, site = c("A", "B", "C", "D"), canton = c("ZH", "AG", NA, "ZH"),
    parameter = "NO2", concentration = 1:4
  )

  result <- prepare_monitoring_airquality(data, cantons = "ZH")
  expect_equal(result$site, c("A", "D"))
  expect_false("canton" %in% names(result))

  expect_equal(prepare_monitoring_airquality(data, cantons = c("ZH", "AG"))$site, c("A", "B", "D"))
})

test_that("prepare_monitoring_airquality() stops if a column is missing", {
  data <- tibble::tibble(year = 2020, site = "A")

  expect_error(prepare_monitoring_airquality(data, "ZH"), "canton", class = "airquality_input_error")
})

# ---- nitrogen deposition: classes ------------------------------------------------------

test_that("recode_ecosystems() merges forest types and wetlands and orders the levels", {
  result <- recode_ecosystems(c("Mischwald", "Laubwald", "Nadelwald", "Wald", "Feuchtgebiet", "Flachmoor",
                                "Hochmoor", "Trockenrasen", "kein empf. Ökosys."))

  expect_s3_class(result, "factor")
  expect_equal(levels(result), c("Wald", "Hochmoor", "Flachmoor", "Trockenrasen", "kein empf. Ökosys."))
  expect_equal(as.character(result), c("Wald", "Wald", "Wald", "Wald", "Flachmoor", "Flachmoor",
                                       "Hochmoor", "Trockenrasen", "kein empf. Ökosys."))
})

test_that("recode_ecosystems() counts settlements as not sensitive", {
  expect_equal(as.character(recode_ecosystems("Siedlungen")), "kein empf. Ökosys.")
})

test_that("recode_ecosystems() warns about unknown ecosystems, which become NA", {
  expect_warning(result <- recode_ecosystems(c("Wald", "Gletscher")), "Gletscher")
  expect_equal(as.character(result), c("Wald", NA))
  expect_no_warning(recode_ecosystems(c("Wald", NA)))
})

test_that("classify_ostluft_siteclass() follows the Ostluft thresholds (livestock units, fertilisation)", {
  gve <-   c(5000, 5000,  5001, 10000, 10001,  100, NA)
  nfert <- c(  50,   51,   100,   100,    10,  101, 10)

  result <- classify_ostluft_siteclass(gve, nfert)

  expect_equal(levels(result), c("hoch", "mittel", "tief"))
  expect_equal(as.character(result), c("tief", "mittel", "mittel", "mittel", "hoch", "hoch", NA))
})

test_that("classify_nh3_emission() cuts at 10 and 30 (lower bounds included)", {
  result <- classify_nh3_emission(c(0, 10, 10.1, 30, 31, NA))

  expect_equal(levels(result), c("tief", "mittel", "hoch"))
  expect_equal(as.character(result), c("tief", "tief", "mittel", "mittel", "hoch", NA))
})

test_that("classify_estimated() and classify_frac_estimated() cut the estimated part", {
  expect_equal(as.character(classify_estimated(c(0, 5, 6, 12, 13))),
               c("<5 kg-N", "<5 kg-N", "5-12 kg-N", "5-12 kg-N", ">12 kg-N"))
  expect_equal(as.character(classify_frac_estimated(c(0, 0.33, 0.5, 0.66, 1))),
               c("<33%", "<33%", "33-66%", "33-66%", ">66%"))
})

test_that("derive_source_category() assigns NOx and NH3 sources by the parameter name", {
  result <- derive_source_category(c("NO2-N_ddep", "NO3-N_bdep", "NH4-N_bdep", "NH3-N_ddep", "other"))

  expect_equal(result, c("aus NOx-Quellen", "aus NOx-Quellen", "aus NH3-Quellen", "aus NH3-Quellen", NA))
})

# ---- nitrogen deposition: preparation and aggregation -------------------------------------

make_site_meta <- function() {
  tibble::tibble(
    site = c("S1", "S2"), ecosys = c("Mischwald", "Siedlungen"), x = c(1, 2), y = c(3, 4), masl = c(400, 500),
    gve_5km = c(12000, 1000), n_fertilization_5km = c(10, 10), nh3_emission_1km = c(40, 5), cln = c(15, NA),
    other = "dropped"
  )
}

make_ndep <- function() {
  tibble::tibble(
    year = 2020, site = c("S1", "S1", "S1", "S2", "S3"), canton = c("ZH", "ZH", "ZH", "ZH", "AG"),
    ecosys = c("Laubwald", "Laubwald", "Laubwald", "Siedlungen", "Wald"),
    parameter = c("NO2-N_ddep", "NH4-N_bdep", "NH3-N_ddep", "NH4-N_bdep", "NH4-N_bdep"),
    value = c(2, 10, 4, 5, 1), unit = "kg/ha/a", part_est = c(1, 0, 0.5, 0, 0),
    sampling = "x", datasource = c("FUB", "Ostluft", "FUB", "Ostluft", "Ostluft"), source = "Ostluft"
  )
}

test_that("prepare_ndep_site_meta() classifies the sites and keeps the needed columns", {
  result <- prepare_ndep_site_meta(make_site_meta())

  expect_named(result, c("site", "ecosys", "x", "y", "masl", "siteclass", "emissionclass", "gve_5km",
                         "nh3_emission_1km", "cln"))
  expect_equal(as.character(result$siteclass), c("hoch", "tief"))
  expect_equal(as.character(result$emissionclass), c("hoch", "tief"))
  expect_equal(as.character(result$ecosys), c("Wald", "kein empf. Ökosys."))
})

test_that("prepare_ndep_parameters() adds the site metadata and keeps the given cantons", {
  site_meta <- prepare_ndep_site_meta(make_site_meta())

  result <- prepare_ndep_parameters(make_ndep(), site_meta, cantons = "ZH")

  expect_named(result, c("year", "site", "canton", "x", "y", "masl", "ecosys", "cln", "siteclass", "emissionclass",
                         "pollutant", "metric", "parameter", "deposition", "unit", "part_est", "source_cat",
                         "sampling", "datasource", "source"))
  expect_equal(result$site, c("S1", "S1", "S1", "S2"))
  expect_equal(result$cln, c(15, 15, 15, NA))
  expect_equal(as.character(result$siteclass), c("hoch", "hoch", "hoch", "tief"))
  expect_equal(result$source_cat, c("aus NOx-Quellen", "aus NH3-Quellen", "aus NH3-Quellen", "aus NH3-Quellen"))
  expect_equal(as.character(unique(result$pollutant)), "Ndep")
  expect_equal(as.character(unique(result$metric)), "Jahressumme")
  expect_equal(result$deposition, c(2, 10, 4, 5))
})

test_that("prepare_ndep_parameters() stops if the deposition data or the site metadata lack columns", {
  site_meta <- prepare_ndep_site_meta(make_site_meta())

  expect_error(prepare_ndep_parameters(dplyr::select(make_ndep(), -part_est), site_meta, "ZH"),
               "part_est", class = "airquality_input_error")
  expect_error(prepare_ndep_site_meta(dplyr::select(make_site_meta(), -cln)), "cln", class = "airquality_input_error")
})

test_that("aggregate_ndep() sums the deposition per site and year and the estimated part", {
  pars <- prepare_ndep_parameters(make_ndep(), prepare_ndep_site_meta(make_site_meta()), cantons = "ZH")

  result <- aggregate_ndep(pars, additional_groups = c("x", "y", "masl", "pollutant", "metric"))

  expect_named(result, c("site", "year", "ecosys", "siteclass", "emissionclass", "canton", "cln", "unit", "x", "y",
                         "masl", "pollutant", "metric", "datasource", "source", "estimated", "deposition",
                         "estimated_class", "frac_estimated_class"))
  expect_equal(result$site, c("S1", "S2"))
  s1 <- dplyr::filter(result, site == "S1")
  expect_equal(s1$deposition, 16)
  expect_equal(s1$estimated, 2 * 1 + 4 * 0.5)
  expect_equal(s1$datasource, "FUB,Ostluft")
  expect_equal(as.character(s1$estimated_class), "<5 kg-N")
  expect_equal(as.character(s1$frac_estimated_class), "<33%")
})

test_that("aggregate_ndep() sorts the rows by the grouping columns", {
  pars <- prepare_ndep_parameters(make_ndep(), prepare_ndep_site_meta(make_site_meta()), cantons = "ZH")
  shuffled <- pars[c(4, 3, 1, 2), ]

  result <- aggregate_ndep(shuffled, additional_groups = c("x", "y", "masl", "pollutant", "metric"))

  expect_equal(result$site, c("S1", "S2"))
})
