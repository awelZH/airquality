


#' Get the download url of a BFS asset from the BFS DAM api
#'
#' @param bfs_nr BFS order number, e.g. "px-x-0102020300_101".
#'
#' @return Url of the master file of the asset.
#'
#' @keywords internal
get_bfs_asset_url <- function(bfs_nr) {

  data <-
    httr2::request("https://dam-api.bfs.admin.ch/hub/api/dam/assets") |>
    httr2::req_url_query(orderNr = bfs_nr) |>
    httr2::req_headers(accept = "application/json") |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  hrefs <- purrr::map_chr(data[["data"]][[1]][["links"]], "href")
  master <- hrefs[stringr::str_detect(hrefs, "/master$")]
  if (length(master) != 1) {
    cli::cli_abort("Expected one master file for BFS asset {.val {bfs_nr}}, found {length(master)}.")
  }

  return(master)
}
