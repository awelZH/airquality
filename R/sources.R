# Inventory of the data sources: the facts that change with every data release (years available, spatial
# resolution per year, version), derived on every run from targets that exist anyway. The register
# data/meta/ressources.csv holds the stable facts by hand and is never written by the pipeline (decision 13).


#' Write years as a compact list with ranges
#'
#' @param years Years (numeric); duplicates and `NA` are dropped.
#'
#' @return A string like "1990, 2000, 2010–2025", `NA` without years.
#'
#' @keywords internal
compact_years <- function(years) {
  years <- sort(unique(stats::na.omit(years)))
  if (length(years) == 0) {
    return(NA_character_)
  }
  run <- cumsum(c(1, diff(years) != 1))
  purrr::map_chr(split(years, run), \(y) if (length(y) == 1) as.character(y) else paste0(y[1], "–", y[length(y)])) |>
    paste(collapse = ", ")
}


#' Condense the cell size per year to ranges
#'
#' @param year Years.
#' @param res Cell size in metres of each year.
#'
#' @return A string like "200 m 1990–2019, 20 m 2020–2025" (sizes of whole kilometres in km), `NA` without years.
#'
#' @keywords internal
resolution_ranges <- function(year, res) {
  if (length(year) == 0) {
    return(NA_character_)
  }
  data <- dplyr::arrange(tibble::tibble(year = year, res = res), .data$year)
  run <- cumsum(c(1, diff(data$res) != 0))
  label <- \(res) if (res %% 1000 == 0) paste(res / 1000, "km") else paste(format(res, trim = TRUE), "m")

  purrr::map_chr(split(data, run), \(group) paste(label(group$res[1]), compact_years(group$year))) |>
    paste(collapse = ", ")
}


#' Cell size of a cell table per year
#'
#' @param cells Table with the cell centres `x` and `year`.
#' @param endpoint Endpoint of the source (collection id) the cells come from.
#'
#' @return Tibble with `endpoint`, `year`, `res` (smallest distance of the cell centres in x).
#'
#' @keywords internal
cell_sizes <- function(cells, endpoint) {
  cells |>
    dplyr::summarise(res = min(diff(sort(unique(.data$x)))), .by = "year") |>
    dplyr::arrange(.data$year) |>
    dplyr::mutate(endpoint = endpoint, .before = 1)
}


#' Installed version of the package of every `r_package` source
#'
#' @param ressources The register ([prepare_ressources()]).
#'
#' @return Named list, key to the version of the package named in its `ENDPOINT` (`package::object`).
#'
#' @keywords internal
package_versions <- function(ressources) {
  packages <- dplyr::filter(ressources, .data$SOURCE_TYPE == "r_package")
  purrr::map(rlang::set_names(sub("::.*$", "", packages$ENDPOINT), packages$KEY),
             \(package) as.character(utils::packageVersion(package)))
}


#' A version as a date, where it is a timestamp
#'
#' @param version Character: a timestamp like "2026-02-06T14:12:28+01:00" or a version like "0.1.3".
#'
#' @return `version`, timestamps cut to the date.
#'
#' @keywords internal
as_version <- function(version) {
  dplyr::if_else(grepl("^\\d{4}-\\d{2}-\\d{2}", version), substr(version, 1, 10), version)
}


#' Inventory of the data sources: years, spatial resolution and version per key
#'
#' @param ressources The register ([prepare_ressources()]); `KEY` and `ENDPOINT` are used.
#' @param assets Asset states of geo.admin collections ([geo_admin_asset_state()]): `collection`, `year`,
#'   `updated`; matched to the keys by `ENDPOINT`.
#' @param years Named list, key to the years of the data read.
#' @param grids Cell sizes: `endpoint`, `year`, `res` ([cell_sizes()], `grids` of [read_pollutant_maps()]).
#' @param versions Named list, key to its version (a timestamp is cut to the date).
#'
#' @return Tibble with one row per key of the register, in its order: `KEY`, `years`, `resolution`, `version`
#'   (`NA` where nothing is derived).
#'
#' @keywords internal
sources_inventory <- function(ressources, assets = NULL, years = list(), grids = NULL, versions = list()) {
  key_of <- rlang::set_names(ressources$KEY, ressources$ENDPOINT)

  asset_years <- if (!is.null(assets)) dplyr::transmute(assets, KEY = unname(key_of[.data$collection]), .data$year)
  data_years <- purrr::imap(years, \(y, key) tibble::tibble(KEY = key, year = as.numeric(y))) |> purrr::list_rbind()
  all_years <- dplyr::bind_rows(asset_years, data_years) |>
    dplyr::filter(!is.na(.data$KEY)) |>
    dplyr::summarise(years = compact_years(.data$year), .by = "KEY")

  resolution <- if (!is.null(grids)) {
    grids |>
      dplyr::mutate(KEY = unname(key_of[.data$endpoint])) |>
      dplyr::filter(!is.na(.data$KEY)) |>
      dplyr::summarise(resolution = resolution_ranges(.data$year, .data$res), .by = "KEY")
  }

  asset_versions <- if (!is.null(assets)) {
    assets |>
      dplyr::transmute(KEY = unname(key_of[.data$collection]), version = as.character(.data$updated)) |>
      dplyr::filter(!is.na(.data$KEY)) |>
      dplyr::summarise(version = max(.data$version), .by = "KEY")
  }
  all_versions <- dplyr::bind_rows(
    asset_versions,
    tibble::tibble(KEY = names(versions), version = as.character(unlist(versions)))
  )

  tibble::tibble(KEY = ressources$KEY) |>
    dplyr::left_join(all_years, by = "KEY") |>
    dplyr::left_join(resolution %||% tibble::tibble(KEY = character(), resolution = character()), by = "KEY") |>
    dplyr::left_join(all_versions, by = "KEY") |>
    dplyr::mutate(version = as_version(.data$version))
}


#' Append the inventory of the data sources to its log and name the sources that changed
#'
#' Called by a target that only reruns when the inventory changed, so every run in the log is a change.
#'
#' @param inventory Output of [sources_inventory()].
#' @inheritParams append_output
#' @param run Identifier of the run, by default the current time.
#'
#' @return The path of the log file.
#'
#' @keywords internal
log_sources_inventory <- function(inventory, file, dir, run = format(Sys.time(), "%Y-%m-%d %H:%M:%S")) {
  path <- file.path(dir, file)
  if (file.exists(path)) {
    log <- readr::read_delim(path, delim = ";", show_col_types = FALSE, col_types = readr::cols(.default = "c"))
    last <- dplyr::filter(log, .data$run == max(.data$run)) |> dplyr::select(!"run")
    changed <- dplyr::anti_join(inventory, last, by = names(inventory))$KEY
    if (length(changed) > 0) cli::cli_inform("Data sources changed since the last run: {.val {changed}}.")
  }

  append_output(dplyr::mutate(inventory, run = run, .before = 1), file, dir = dir)
}


#' Table of the data sources for the website: the register joined with the inventory
#'
#' @param ressources The register ([prepare_ressources()]).
#' @param inventory Output of [sources_inventory()].
#'
#' @return Tibble with German column names, one row per source read by the analysis (no `reference` rows):
#'   the title as markdown link to the landing page, provider, years, resolution, version and access.
#'
#' @keywords internal
sources_table <- function(ressources, inventory) {
  ressources |>
    dplyr::filter(.data$SOURCE_TYPE != "reference") |>
    dplyr::left_join(inventory, by = "KEY", relationship = "one-to-one") |>
    dplyr::transmute(
      Datengrundlage = dplyr::if_else(is.na(.data$LANDING_PAGE), .data$TITLE,
                                      paste0("[", .data$TITLE, "](", .data$LANDING_PAGE, ")")),
      Urheber = .data$PROVIDER,
      Jahre = .data$years,
      "Auflösung" = .data$resolution,
      Stand = .data$version,
      Zugang = paste(dplyr::if_else(.data$ACCESS == "public", "öffentlich", "nicht öffentlich"),
                     .data$LICENSE, sep = "; ") |> sub(pattern = "; NA$", replacement = "")
    ) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), \(x) dplyr::coalesce(x, "")))
}
