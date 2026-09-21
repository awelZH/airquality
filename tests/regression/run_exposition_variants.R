# Regression run for the exposition outputs: computes three variants with the new functions and writes
# them to tests/regression/results/<variant>/ under the established file names. compare_exposition.R
# then compares each variant with the frozen baseline (tests/regression/baseline/, outputs as of
# commit c0cb59c, computed with the old pipeline and airquality.methods 0.3.0).
#
# The variants separate the causes of deviations:
#   legacy     - old semantics: collector pixels kept, and cells of municipalities with several features
#                (exclaves: Glattfelden, Mönchaltorf; bfs 0: lakes, Kloster Fahr) counted once per feature
#                in the weighted means, as merge_statpop_with_subareas() did => isolates the refactoring
#   fixed      - collector pixels kept, every cell counted once, lake cells assigned to the nearest
#                municipality, Kloster Fahr excluded => effect of the corrected cell assignment
#   noloc_dropped - collector pixels subtracted and dropped (state before 2026-09-18 evening)
#   production - collector pixels subtracted and spread over their municipality => what the script writes
#
# The steps below mirror scripts/_compile_exposition_data.R.
#
# run from the project root: source("tests/regression/run_exposition_variants.R")

source("scripts/_setup.R", encoding = "UTF-8")

years_exposition <- 2010:(lubridate::year(Sys.Date()) - year_offset)
data_monitoring_aq <- airquality.data::data_monitoring_aq_y1
coefs_o3_peakseason <- fit_o3_peakseason_model(data_monitoring_aq)
ratios_pm <- fit_pm_ratio(data_monitoring_aq)

# the old pipeline used the full geolion map (incl. the Kloster Fahr enclave of the Canton of Aargau) and
# left cells with their centre in a lake without municipality (bfs 0)
map_municipalities_legacy <- airquality.methods::read_geolion_wfs(filter_ressources(ressources, 11), version = "2.0.0", crs = crs)

assign_municipalities_legacy <- function(cells, map) {
  centres <- dplyr::distinct(cells, x, y)
  hits <- sf::st_intersects(sf::st_as_sf(centres, coords = c("x", "y"), crs = sf::st_crs(map)), map)
  feature <- purrr::map_int(hits, \(i) if (length(i) > 0) i[[1]] else NA_integer_)
  centres <- dplyr::mutate(centres, bfsnr = map$bfs[feature], gemeindename = map$gemeindename[feature])
  dplyr::left_join(cells, centres, by = dplyr::join_by(x, y))
}

# number of map features per bfs number: the old join multiplied each cell by this factor
feature_count <- dplyr::count(sf::st_drop_geometry(map_municipalities_legacy), bfsnr = bfs, name = "n_features")

compute_variant <- function(correct_noloc, legacy_double_counting, redistribute = FALSE) {

  assign <- if (legacy_double_counting) {
    \(cells) assign_municipalities_legacy(cells, map_municipalities_legacy)
  } else {
    \(cells) airquality.methods::assign_municipalities(cells, map_municipalities)
  }

  rasters <- read_exposition_rasters(years_exposition, map_municipalities, correct_noloc = correct_noloc)
  noloc <- airquality.methods::noloc_from_aligned(if (redistribute) rasters else rasters[0, ])

  data_expo <-
    rasters |>
    rasters_to_cells() |>
    assign() |>
    airquality.methods::redistribute_noloc(noloc, map_municipalities) |>
    derive_o3_peakseason(coefs_o3_peakseason) |>
    derive_pm25_from_pm10(ratios_pm, years = min(years_exposition):2014) |>
    cells_to_long() |>
    add_base_scenario(base_scenario_year)

  data_expo_means <- data_expo
  if (legacy_double_counting) {
    data_expo_means <-
      data_expo |>
      dplyr::left_join(feature_count, by = dplyr::join_by(bfsnr)) |>
      tidyr::uncount(dplyr::coalesce(n_features, 1L))
  }

  list(
    data_exposition_weighted_means_canton = round_population(combine_canton_means(data_expo_means, base_scenario_year)),
    data_exposition_weighted_means_municipalities = round_population(aggregate_population_weighted_mean(data_expo_means, level = "municipality")),
    data_exposition_distribution_pollutants = round_population(aggregate_population_exposition_distrib(data_expo))
  )
}

variants <- list(
  legacy = list(correct_noloc = FALSE, legacy_double_counting = TRUE),
  fixed = list(correct_noloc = FALSE, legacy_double_counting = FALSE),
  noloc_dropped = list(correct_noloc = TRUE, legacy_double_counting = FALSE),
  production = list(correct_noloc = TRUE, legacy_double_counting = FALSE, redistribute = TRUE)
)

data_ndep_distrib <- aggregate_ndep_exposition_distrib(read_ndep_exceedance(map_municipalities))

for (variant in names(variants)) {
  cli::cli_h1("Variant {variant}")
  outputs <- rlang::exec(compute_variant, !!!variants[[variant]])
  outputs$data_exposition_distribution_ndep <- data_ndep_distrib

  dir <- file.path("tests/regression/results", variant)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  purrr::iwalk(outputs, \(data, name) airquality.methods::write_local_csv(data, file.path(dir, paste0(name, ".csv"))))
}
