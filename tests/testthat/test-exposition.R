# Unit tests for R/exposition.R. All inputs are synthetic; no network.

# ---- assign_municipalities() ------------------------------------------------

make_square <- function(xmin, ymin, size) {
  sf::st_polygon(list(rbind(
    c(xmin, ymin), c(xmin + size, ymin), c(xmin + size, ymin + size),
    c(xmin, ymin + size), c(xmin, ymin)
  )))
}

make_map <- function() {
  # municipality 1 with an exclave (same bfs, two features), municipality 2,
  # a lake that belongs to no municipality (bfs 0, no name) and an enclave of
  # another canton (bfs 0, no name), as in the geolion map
  sf::st_sf(
    bfs = c(1L, 1L, 2L, 0L, 0L),
    gemeindename = c("A", "A", "B", NA, NA),
    art_text = c(
      "Gemeinde", "Exklave einer Gemeinde", "Gemeinde",
      "zu keiner Gemeinde gehörende Seefläche", "ausserkantonale Enklave (Kloster Fahr)"
    ),
    geometry = sf::st_sfc(
      make_square(0, 0, 200),
      make_square(1000, 1000, 100),
      make_square(200, 0, 200),
      make_square(0, 200, 400),
      make_square(2000, 2000, 100),
      crs = 2056
    )
  )
}

test_that("drop_foreign_enclaves() removes enclaves of other cantons only", {
  result <- drop_foreign_enclaves(make_map())

  expect_equal(nrow(result), 4)
  expect_false(any(grepl("ausserkantonal", result$art_text)))
})

test_that("assign_municipalities() assigns every cell exactly once", {
  cells <- tibble::tibble(
    x = c(50, 150, 1050, 250, 5000),
    y = c(50, 150, 1050, 50, 5000),
    year = 2020,
    population = c(1, 2, 4, 8, 32)
  )

  result <- assign_municipalities(cells, drop_foreign_enclaves(make_map()))

  expect_equal(nrow(result), nrow(cells))
  expect_equal(sum(result$population), sum(cells$population))
  expect_equal(result$bfsnr, c(1L, 1L, 1L, 2L, NA))
  expect_equal(result$gemeindename, c("A", "A", "A", "B", NA))
})

test_that("assign_municipalities() gives lake cells the nearest municipality", {
  # (50, 250) lies 50 m from A, (350, 250) 50 m from B
  cells <- tibble::tibble(x = c(50, 350), y = c(250, 250), population = c(16, 64))

  expect_message(
    result <- assign_municipalities(cells, drop_foreign_enclaves(make_map())),
    "nearest"
  )
  expect_equal(result$bfsnr, c(1L, 2L))
  expect_equal(result$gemeindename, c("A", "B"))
})

test_that("assign_municipalities() leaves cells in a foreign enclave outside the canton", {
  cells <- tibble::tibble(x = 2050, y = 2050, population = 23)

  result <- assign_municipalities(cells, drop_foreign_enclaves(make_map()))

  expect_true(is.na(result$bfsnr))
})

test_that("assign_municipalities() rejects ambiguous bfs numbers", {
  map <- make_map()
  map$gemeindename[2] <- "other name"

  expect_error(assign_municipalities(tibble::tibble(x = 50, y = 50), map), "bfs")
})


# ---- STATPOP collector pixels -------------------------------------------------

make_aligned_noloc <- function() {
  # audit tables as returned by airquality.methods::align_to_reference(); the
  # coordinates are the lower-left corners of the 100 m cells
  tibble::tibble(
    year = c(2020L, 2021L),
    res_x = 100,
    noloc = list(
      tibble::tibble(E_KOORD = c(0, 5000), N_KOORD = c(0, 5000), noloc = c(6, 9), subtracted = c(6, 9)),
      NULL
    )
  )
}

test_that("noloc_from_aligned() returns the subtracted inhabitants per cell centre", {
  result <- noloc_from_aligned(make_aligned_noloc())

  expect_named(result, c("year", "x", "y", "noloc"))
  expect_equal(result$year, c(2020, 2020))
  expect_equal(result$x, c(50, 5050))
  expect_equal(result$noloc, c(6, 9))
})

test_that("noloc_from_aligned() copes with years without correction", {
  aligned <- make_aligned_noloc()
  aligned$noloc <- list(NULL, NULL)

  result <- noloc_from_aligned(aligned)

  expect_equal(nrow(result), 0)
  expect_named(result, c("year", "x", "y", "noloc"))
})

make_cells_noloc <- function() {
  # municipality A: two inhabited cells and one empty cell; B: one cell;
  # one cell outside the canton
  tibble::tibble(
    x = c(50, 150, 150, 250, 5000),
    y = c(50, 50, 150, 50, 5000),
    year = 2020,
    population = c(10, 30, 0, 20, 7),
    bfsnr = c(1L, 1L, 1L, 2L, NA),
    gemeindename = c("A", "A", "A", "B", NA),
    no2 = c(10, 20, 99, 15, 50)
  )
}

test_that("redistribute_noloc() spreads collector inhabitants over the inhabited cells of their municipality", {
  cells <- make_cells_noloc()
  noloc <- tibble::tibble(year = 2020, x = 50, y = 50, noloc = 8)

  result <- redistribute_noloc(cells, noloc, drop_foreign_enclaves(make_map()))

  # A: 40 located + 8 collector inhabitants, proportional to the cells' inhabitants
  expect_equal(result$population, c(12, 36, 0, 20, 7))
})

test_that("redistribute_noloc() keeps the municipality's weighted mean", {
  cells <- make_cells_noloc()
  noloc <- tibble::tibble(year = 2020, x = 50, y = 50, noloc = 8)

  result <- redistribute_noloc(cells, noloc, drop_foreign_enclaves(make_map()))
  a_before <- dplyr::filter(cells, bfsnr == 1)
  a_after <- dplyr::filter(result, bfsnr == 1)

  expect_equal(
    stats::weighted.mean(a_after$no2, a_after$population),
    stats::weighted.mean(a_before$no2, a_before$population)
  )
})

test_that("redistribute_noloc() ignores collector pixels outside the canton and works per year", {
  cells <- dplyr::bind_rows(make_cells_noloc(), dplyr::mutate(make_cells_noloc(), year = 2021))
  noloc <- tibble::tibble(year = c(2020, 2021), x = c(5050, 250), y = c(5050, 50), noloc = c(100, 4))

  result <- redistribute_noloc(cells, noloc, drop_foreign_enclaves(make_map()))

  expect_equal(result$population[result$year == 2020], c(10, 30, 0, 20, 7))
  expect_equal(result$population[result$year == 2021], c(10, 30, 0, 24, 7))
})

test_that("redistribute_noloc() warns when a municipality has no inhabited cell", {
  cells <- dplyr::mutate(make_cells_noloc(), population = dplyr::if_else(bfsnr %in% 2L, 0, population))
  noloc <- tibble::tibble(year = 2020, x = 250, y = 50, noloc = 5)

  expect_warning(result <- redistribute_noloc(cells, noloc, drop_foreign_enclaves(make_map())), "B")
  expect_equal(result$population, cells$population)
})

test_that("redistribute_noloc() leaves cells unchanged without collector pixels", {
  cells <- make_cells_noloc()
  noloc <- tibble::tibble(year = numeric(), x = numeric(), y = numeric(), noloc = numeric())

  expect_equal(redistribute_noloc(cells, noloc, drop_foreign_enclaves(make_map())), cells)
})

test_that("round_population() rounds inhabitant counts to whole persons only", {
  data <- tibble::tibble(population = c(1.4, 2.6), population_cum = c(1.4, 4), population_cum_rel = c(0.35, 1), concentration = 1.26)

  result <- round_population(data)

  expect_equal(result$population, c(1, 3))
  expect_equal(result$population_cum, c(1, 4))
  expect_equal(result$population_cum_rel, c(0.35, 1))
  expect_equal(result$concentration, c(1.26, 1.26))
})


# ---- O3 peak-season derivation ----------------------------------------------

make_o3_monitoring <- function() {
  sites <- paste0("site", 1:10)
  no2 <- seq(10, 40, length.out = 10)
  offsets <- c(`2019` = 100, `2020` = 110)
  purrr::map(names(offsets), \(yr) {
    tibble::tibble(
      year = as.numeric(yr),
      site = rep(sites, times = 2),
      masl = 500,
      parameter = rep(c("NO2", "O3_peakseason_mean_d1_max_mean_h8gl"), each = 10),
      # small symmetric scatter: rlm() needs a non-zero residual scale
      concentration = c(no2, offsets[[yr]] - 0.8 * no2 + rep(c(0.1, -0.1), 5))
    )
  }) |>
    purrr::list_rbind()
}

test_that("fit_o3_peakseason_model() recovers slope and per-year offsets", {
  coefs <- fit_o3_peakseason_model(make_o3_monitoring())

  expect_named(coefs, c("year", "offset", "slope"))
  expect_equal(coefs$year, c(2019, 2020))
  expect_equal(coefs$offset, c(100, 110), tolerance = 1e-2)
  expect_equal(coefs$slope, c(-0.8, -0.8), tolerance = 1e-2)
})

test_that("fit_o3_peakseason_model() ignores years with too few sites", {
  monitoring <- make_o3_monitoring() |>
    dplyr::filter(!(year == 2020 & site %in% paste0("site", 1:4)))

  coefs <- fit_o3_peakseason_model(monitoring, nmin_sites = 7)

  expect_equal(coefs$year, 2019)
})

test_that("derive_o3_peakseason() applies the coefficients cell by cell", {
  cells <- tibble::tibble(year = c(2019, 2020, 2020), no2 = c(10, 20, NA))
  coefs <- tibble::tibble(year = c(2019, 2020), offset = c(100, 110), slope = -0.8)

  result <- derive_o3_peakseason(cells, coefs)

  expect_equal(result$o3_peakseason_mean_d1_max_mean_h8gl, c(92, 94, NA))
})

test_that("derive_o3_peakseason() warns and yields NA for years without coefficients", {
  cells <- tibble::tibble(year = c(2019, 2021), no2 = c(10, 10))
  coefs <- tibble::tibble(year = 2019, offset = 100, slope = -0.8)

  expect_warning(result <- derive_o3_peakseason(cells, coefs), "2021")
  expect_equal(result$o3_peakseason_mean_d1_max_mean_h8gl, c(92, NA))
})


# ---- PM2.5 from PM10 ----------------------------------------------------------

test_that("fit_pm_ratio() estimates one PM2.5:PM10 ratio per year from NABEL sites", {
  monitoring <- tibble::tibble(
    year = rep(c(2010, 2011), each = 6),
    site = rep(c("a", "b", "Bern-Bollwerk"), times = 4),
    source = "NABEL (BAFU & Empa)",
    parameter = rep(rep(c("PM10", "PM2.5"), each = 3), times = 2),
    # Bern-Bollwerk is excluded; a and b scatter symmetrically around the ratio
    concentration = c(20, 20, 20, 15.2, 14.8, 5, 20, 20, 20, 14.2, 13.8, 2)
  )

  ratios <- fit_pm_ratio(monitoring)

  expect_named(ratios, c("year", "ratio"))
  expect_equal(ratios$ratio, c(0.75, 0.7), tolerance = 1e-6)
})

test_that("derive_pm25_from_pm10() fills only missing PM2.5 in the target years", {
  cells <- tibble::tibble(
    year = c(2010, 2014, 2015),
    pm10 = c(20, 20, 20),
    pm2_5 = c(NA, 12, NA)
  )
  ratios <- tibble::tibble(year = c(2010, 2014, 2015), ratio = c(0.75, 0.7, 0.65))

  result <- derive_pm25_from_pm10(cells, ratios, years = 2010:2014)

  expect_equal(result$pm2_5, c(15, 12, NA))
})


# ---- coefficient log --------------------------------------------------------------

test_that("tidy_derivation_coefficients() lists every coefficient with its model", {
  coefs_o3 <- tibble::tibble(year = c(2019, 2020), offset = c(100, 110), slope = -0.8)
  ratios_pm <- tibble::tibble(year = 2010, ratio = 0.75)

  result <- tidy_derivation_coefficients(coefs_o3, ratios_pm, run = "2026-09-18 20:00:00")

  expect_named(result, c("run", "parameter", "year", "term", "value"))
  expect_equal(nrow(result), 5)
  expect_equal(unique(result$run), "2026-09-18 20:00:00")
  expect_equal(
    result$value[result$parameter == "O3_peakseason_mean_d1_max_mean_h8gl" & result$term == "offset"],
    c(100, 110)
  )
  expect_equal(result$term[result$parameter == "PM2.5"], "ratio_pm25_pm10")
})

test_that("append_log() writes a header once and appends later runs", {
  file <- withr::local_tempfile(fileext = ".csv")
  run1 <- tibble::tibble(run = "a", value = 1)
  run2 <- tibble::tibble(run = "b", value = 2)

  append_log(run1, file)
  append_log(run2, file)

  lines <- readLines(file)
  expect_equal(lines, c("run;value", "a;1", "b;2"))
})


# ---- long format and base scenario -----------------------------------------

test_that("cells_to_long() uses the established parameter names and metadata", {
  cells <- tibble::tibble(
    x = 50, y = 50, year = 2020, bfsnr = 1L, gemeindename = "A", population = 10,
    no2 = 20, pm10 = NA, pm2_5 = 10, o3_max_98p_m1 = 150,
    o3_peakseason_mean_d1_max_mean_h8gl = 90
  )

  result <- cells_to_long(cells)

  expect_setequal(
    result$parameter,
    c("NO2", "PM2.5", "O3_max_98p_m1", "O3_peakseason_mean_d1_max_mean_h8gl")
  )
  expect_equal(result$pollutant[result$parameter == "O3_max_98p_m1"], "O3")
  expect_equal(result$metric[result$parameter == "NO2"], "Jahresmittel")
  expect_false(anyNA(result$concentration))
})

test_that("add_base_scenario() attaches the base-year concentration of the same cell", {
  long <- tibble::tibble(
    x = c(50, 50, 150, 50, 150),
    y = 50,
    year = c(2015, 2020, 2020, 2015, 2020),
    parameter = c("NO2", "NO2", "NO2", "PM10", "PM10"),
    concentration = c(30, 20, 25, 18, 15)
  )

  result <- add_base_scenario(long, base_year = 2015)

  expect_equal(result$concentration_base, c(NA, 30, NA, NA, NA))
})


# ---- aggregation --------------------------------------------------------------

make_long <- function() {
  # four cells inside the canton (every one with a municipality) and one outside
  tibble::tibble(
    x = c(1, 2, 3, 4, 5),
    y = 1,
    year = 2020,
    bfsnr = c(1L, 1L, 2L, 2L, NA),
    gemeindename = c("A", "A", "B", "B", NA),
    population = c(10, 30, 60, 5, 1000),
    pollutant = "NO2",
    metric = "Jahresmittel",
    parameter = "NO2",
    concentration = c(10, 20, 30, 40, 99),
    concentration_base = c(12, 22, 32, 42, 99)
  )
}

test_that("canton weighted mean covers all cells inside the canton", {
  result <- aggregate_population_weighted_mean(make_long(), level = "canton")

  expect_equal(result$population, 105)
  expect_equal(result$population_weighted_mean, (100 + 600 + 1800 + 200) / 105)
  expect_equal(result$concentration_min, 10)
  expect_equal(result$concentration_max, 40)
  expect_equal(result$unit, "μg/m3")
  expect_equal(result$source, "BAFU & BFS")
})

test_that("municipality weighted means are computed per bfs number", {
  result <- aggregate_population_weighted_mean(make_long(), level = "municipality")

  expect_equal(result$bfsnr, c(1L, 2L))
  expect_equal(result$gemeindename, c("A", "B"))
  expect_equal(result$population, c(40, 65))
  expect_equal(result$population_weighted_mean, c(700 / 40, 2000 / 65))
})

test_that("municipalities add up to the canton", {
  long <- make_long()
  canton <- aggregate_population_weighted_mean(long, level = "canton")
  municipal <- aggregate_population_weighted_mean(long, level = "municipality")

  expect_equal(sum(municipal$population), canton$population)
  expect_equal(
    sum(municipal$population_weighted_mean * municipal$population) / sum(municipal$population),
    canton$population_weighted_mean
  )
})

test_that("weighted mean can be computed for the base-scenario concentration", {
  result <- aggregate_population_weighted_mean(
    make_long(), level = "canton", concentration = concentration_base
  )

  expect_equal(result$population_weighted_mean, (120 + 660 + 1920 + 210) / 105)
})

test_that("population exposition distribution bins per parameter and accumulates", {
  long <- make_long() |>
    dplyr::mutate(concentration = c(10.2, 10.7, 12.1, 13.9, 50))

  result <- aggregate_population_exposition_distrib(long)

  expect_named(result, c(
    "year", "pollutant", "metric", "parameter", "concentration",
    "population", "population_cum", "population_cum_rel", "source"
  ))
  expect_equal(result$concentration, c(10.5, 12.5, 13.5))
  expect_equal(result$population, c(40, 60, 5))
  expect_equal(result$population_cum_rel, c(40, 100, 105) / 105)
})

test_that("population exposition distribution drops cells without inhabitants", {
  long <- make_long() |>
    dplyr::mutate(population = c(0, 30, 60, 5, 1000))

  result <- aggregate_population_exposition_distrib(long)

  expect_equal(sum(result$population), 95)
  expect_false(10.5 %in% result$concentration)
})

test_that("ndep exposition distribution counts cells per 1 kg class", {
  ndep <- tibble::tibble(year = 2020, ndep_exmax = c(4.2, 4.9, 6.1, NA))

  result <- aggregate_ndep_exposition_distrib(ndep)

  expect_named(result, c("year", "ndep_exmax", "n_ecosys", "n_ecosys_cum", "n_ecosys_cum_rel", "source"))
  expect_equal(result$ndep_exmax, c(4.5, 6.5))
  expect_equal(result$n_ecosys, c(2L, 1L))
  expect_equal(result$n_ecosys_cum_rel, c(2 / 3, 1))
})


# ---- canton weighted means incl. base scenario (output shape) --------------

test_that("combine_canton_means() yields the established column order", {
  long <- make_long() |>
    dplyr::bind_rows(dplyr::mutate(make_long(), year = 2015, concentration_base = NA))

  result <- combine_canton_means(long, base_year = 2015)

  expect_named(result, c(
    "year", "pollutant", "metric", "parameter", "population_weighted_mean_base",
    "base_year", "population_weighted_mean", "population", "concentration_min",
    "concentration_max", "concentration_mean", "concentration_median", "unit", "source"
  ))
  expect_equal(result$year, c(2015, 2020))
  expect_equal(result$base_year, c(NA, 2015))
  expect_true(is.na(result$population_weighted_mean_base[1]))
})
