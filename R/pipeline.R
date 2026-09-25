# Building blocks of the targets pipeline (pipelines/*.R): writing the outputs as file targets, the state
# of online raster assets and opendata.swiss tables, restricted inputs, the work-in-progress outputs and the
# report.


#' Write an output CSV and return its path
#'
#' For a `format = "file"` target: the contract format of [airquality.methods::write_local_csv()]
#' (delimiter `;`, UTF-8, `NA`, full precision).
#'
#' @param data Data to write.
#' @param file File name, e.g. "data_emissions.csv".
#' @param dir Output directory (`path_output` of `settings.R`); created if missing.
#'
#' @return The path of the written file.
#'
#' @keywords internal
write_output <- function(data, file, dir) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(dir, file)
  airquality.methods::write_local_csv(data, file = path)
  path
}


#' Append rows to a log CSV and return its path
#'
#' @inheritParams write_output
#' @param dir Log directory (`path_log` of `settings.R`); created if missing.
#'
#' @return The path of the log file.
#'
#' @keywords internal
append_output <- function(data, file, dir) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(dir, file)
  airquality.methods::write_local_csv(data, file = path, append = TRUE)
  path
}


#' State of the assets of data.geo.admin.ch collections
#'
#' The cheap metadata (about 1 s per collection) behind the expensive raster reads: a target with
#' `tar_cue("always")` returns it on every run, and the raster targets that depend on it only rerun when
#' an asset was added, replaced or updated.
#'
#' @param collections Collection ids.
#' @param get_assets Function returning the assets of one collection
#'   ([airquality.methods::get_geo_admin_assets()]).
#'
#' @return Tibble with `collection`, `year`, `asset`, `href`, `updated` and `checksum`, sorted.
#'
#' @keywords internal
geo_admin_asset_state <- function(collections, get_assets = airquality.methods::get_geo_admin_assets) {
  purrr::map(unname(collections), get_assets) |>
    purrr::list_rbind() |>
    dplyr::select("collection", "year", "asset", "href", "updated", checksum = "file:checksum") |>
    dplyr::arrange(.data$collection, .data$year, .data$asset)
}


#' State of the resources of an opendata.swiss dataset
#'
#' The same for the tables of opendata.swiss: the metadata (under 1 s) behind a download of up to several
#' hundred MB. A target with `tar_cue("always")` returns it on every run, and the download target that
#' depends on it only reruns when a resource was added, removed, modified or changed its size. There is no
#' checksum on opendata.swiss, so a file replaced without new metadata goes unnoticed.
#'
#' @param url Package-show url of the CKAN API (`DOWNLOAD_URL` of `ressources.csv`).
#' @param file_filter Substring the download url must contain; the same as in
#'   [airquality.methods::read_opendataswiss()], so the state covers exactly the files that are read.
#' @param get_resources Function returning the resources of a dataset
#'   ([airquality.methods::get_opendataswiss_resources()]).
#'
#' @return Tibble with `download_url`, `modified` and `byte_size` of the matching resources, sorted. Stops
#'   with an error of class `airquality_input_error` if none matches.
#'
#' @keywords internal
opendataswiss_state <- function(url, file_filter = ".csv",
                                get_resources = airquality.methods::get_opendataswiss_resources) {
  resources <- get_resources(url)
  matching <- dplyr::filter(resources, stringr::str_detect(.data$download_url, stringr::fixed(file_filter)))

  if (nrow(matching) == 0) {
    cli::cli_abort(c(
      "No resource of {.url {url}} matches {.val {file_filter}}.",
      "i" = "Available: {.val {resources$download_url}}"
    ), class = "airquality_input_error")
  }

  matching |>
    dplyr::select("download_url", "modified", "byte_size") |>
    dplyr::arrange(.data$download_url)
}


#' Path of a restricted (non-public) input file
#'
#' @param path Path of the file in `data/restricted/`.
#'
#' @return `path`, if the file exists. Stops with an error of class `airquality_input_error` otherwise,
#'   pointing to `data/restricted/README.md`.
#'
#' @keywords internal
restricted_file <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort(c(
      "The restricted input {.file {path}} is missing.",
      "i" = "It is not public and not in the repository; see {.file data/restricted/README.md} where to get it."
    ), class = "airquality_input_error")
  }
  path
}


#' Check the outputs of the work-in-progress scripts against the pipeline outputs they are based on
#'
#' The WIP scripts (`wip/`) run outside the pipeline, so their outputs can be stale: a warning names the
#' files older than their newest input or missing.
#'
#' @param wip Paths of the work-in-progress outputs.
#' @param based_on Paths of the pipeline outputs they are computed from.
#'
#' @return Tibble with `file` and `status` ("up to date", "older than its inputs", "missing"), invisibly.
#'
#' @keywords internal
check_wip_outputs <- function(wip, based_on) {
  newest_input <- max(file.mtime(based_on))
  status <- dplyr::case_when(
    !file.exists(wip) ~ "missing",
    file.mtime(wip) < newest_input ~ "older than its inputs",
    .default = "up to date"
  )
  result <- tibble::tibble(file = wip, status = status)

  stale <- dplyr::filter(result, .data$status != "up to date")
  if (nrow(stale) > 0) {
    n <- nrow(stale)
    cli::cli_warn(c(
      "{n} work-in-progress output{?s} not up to date; rerun the script in {.file wip/}:",
      purrr::set_names(paste0(stale$file, ": ", stale$status), rep("!", nrow(stale)))
    ))
  }
  invisible(result)
}


#' Render the report and return the rendered pages
#'
#' @param input Quarto project directory (`report/`).
#' @param output_dir Directory the project renders into (its `output-dir`).
#'
#' @return Paths of the rendered HTML pages.
#'
#' @keywords internal
render_report <- function(input = "report", output_dir = "docs") {
  quarto::quarto_render(input, as_job = FALSE)
  list.files(output_dir, pattern = "[.]html$", full.names = TRUE)
}
