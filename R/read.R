
### read input files
### ------------------------------------------------------------


### function to read *.csv with air pollutant year-statistics exported from https://www.arias.ch/ibonline/ib_online.php and restructure the data similar to a standard long-format (see rOstluft::format_rolf())
read_arias <- function(file, encoding = "latin1", tz = "Etc/GMT-1"){ 
  
  # FIXME: Vereinfachen! (stat-feedback branch)
  locale <- readr::locale(encoding = encoding)
  site <- dplyr::pull(readr::read_delim(file, delim = ";", col_select = "Station", locale = locale))
  parameter <- dplyr::pull(readr::read_delim(file, delim = ";", col_select = "Schadstoff", locale = locale))
  metric <- dplyr::pull(readr::read_delim(file, delim = ";", col_select = "Messparameter", locale = locale))
  unit <- dplyr::pull(readr::read_delim(file, delim = ";", col_select = "Einheit", locale = locale))
  data <- readr::read_delim(file, col_select = c(-(1:10)), delim =";", locale = locale)
  starttime <- lubridate::fast_strptime(colnames(data), format = "%Y", tz = tz, lt = FALSE)
  data <- tibble::as_tibble(t(data))
  colnames(data) <- paste(site, parameter, unit, metric, sep = "_")
  data <- dplyr::bind_cols(tibble::tibble(starttime = starttime), data)
  data <- tidyr::gather(data, temp, value, -starttime)
  data <- tidyr::separate(data, temp, into = c("site", "parameter", "unit", "metric"), sep = "_")
  data <- dplyr::mutate(data,
                        parameter = dplyr::recode(parameter, !!!c("Partikelanzahl" = "PN", "EC / Russ" = "eBC")),
                        unit = dplyr::recode(unit, !!!c("ppm·h" = "ppm*h")),
                        metric = dplyr::recode(metric, !!!c("h\u00f6chster 98%-Wert eines Monats" = "O3_max_98%_m1", 
                                                            "Anzahl Stundenmittel > 120 µg/m3" = "O3_nb_h1>120",
                                                            "Dosis AOT40f" = "O3_AOT40")),
                        parameter = dplyr::case_when(metric == "Jahresmittel" ~ parameter, TRUE ~ metric),
                        interval = "y1",
                        value = as.numeric(stringr::str_remove(value, ";"))
  )
  data <- dplyr::mutate_if(data, is.character, factor)
  data <- dplyr::select(data, "starttime", "site", "parameter", "interval", "unit", "value")
  
  return(data)
}



### function to read *.txt ts (in this case hourly) data from NABEL database and restructure the data similar to a standard long-format (see rOstluft::format_rolf())
read_nabel_ts_h1 <- function(file, interval = "h1", encoding = "latin1", tz = "Etc/GMT-1"){ 
  
  # FIXME: Vereinfachen! (stat-feedback branch)
  locale <- readr::locale(encoding = encoding)
  header <- readr::read_delim(file, delim = "\t", n_max = 4, col_names = FALSE, locale = locale)
  site <- dplyr::pull(dplyr::filter(header, X1 == "Station"), X2)
  parameter <- dplyr::pull(dplyr::filter(header, X1 == "Messwert"), X2)
  unit <- dplyr::pull(dplyr::filter(header, X1 == "Einheit"), X2)
  data <- readr::read_delim(file, delim = "\t", col_names = FALSE, skip = 4, locale = locale)
  data <- dplyr::rename(data, endtime = X1, value = X2)
  data <- dplyr::mutate(data, 
                        site = dplyr::case_when(site == "DUE" ~ "D\u00fcbendorf-EMPA", site == "ZUE" ~ "Z\u00fcrich-Kaserne", TRUE ~ site),
                        starttime = lubridate::fast_strptime(endtime, format = "%d.%m.%y %H:%M", tz = tz, lt = FALSE) - lubridate::hours(as.numeric(stringr::str_remove(interval, "h"))),
                        parameter = parameter, 
                        unit = stringr::str_replace(unit, "ug", "µg"),
                        interval = interval
  )
  data <- dplyr::mutate_if(data, is.character, factor)
  data <- dplyr::select(data, starttime, site, parameter, interval, unit, value)
  
  return(data)
}




get_nabel_meta_arias <- function(file, encoding = "latin1") {
  
  locale <- readr::locale(encoding = encoding)
  meta <- readr::read_delim(file, delim =";", col_select = c("Station", "Ost Y", "Nord X", "H\u00f6he", "Zonentyp", "Stationstyp"), locale = locale)
  meta <- dplyr::distinct(meta, Station, `Ost Y`, `Nord X`, Höhe, Zonentyp, Stationstyp)
  meta <- dplyr::mutate(meta, Zonentyp = tolower(Zonentyp))
  meta <- dplyr::rename(meta, 
                        site = Station,
                        y = `Ost Y`,
                        x = `Nord X`,
                        masl = Höhe,
                        zone = Zonentyp,
                        type = Stationstyp
  )
  meta <- dplyr::select(meta, site, x, y, masl, zone, type)
  
  return(meta)
}




read_ostluft_meta <- function(file, encoding = "UTF-8") {
  
  locale <- readr::locale(encoding = encoding)
  meta <- readr::read_delim(file, delim =",", locale = locale)
  meta <- 
    meta %>% 
    dplyr::filter(msKT == "ZH" & !is.na(msNameAirMo) & !is.na(scSiedlungsgroesse) & !is.na(scVerkehrslage)) %>%
    dplyr::mutate(
      site_long = paste(msOrt, msOrtsteil, sep = " - "),
      scSiedlungsgroesse = stringr::str_trim(scSiedlungsgroesse),
      scVerkehrslage = stringr::str_trim(scVerkehrslage)
    ) %>%
    dplyr::select(msNameAirMo, site_long, spXCoord, spYCoord, spHoehe, scSiedlungsgroesse, scVerkehrslage) %>% 
    dplyr::rename(
      site = msNameAirMo,
      x = spXCoord,
      y = spYCoord,
      masl = spHoehe,
      zone = scSiedlungsgroesse, 
      type = scVerkehrslage
    )
  
  return(meta)
}









### function to read *.csv with air pollutant data from (internal) Airmo (OSTLUFT air quality database) export and restructure the data similar to a standard long-format (see rOstluft::format_rolf()); based on rOstluft::read_airmo_csv()
read_airmo_csv2 <- function(file, encoding = "UTF-8", tz = "Etc/GMT-1", na.rm = TRUE){
  
  locale <- readr::locale(encoding = encoding)
  header_cols <- readr::cols(X1 = readr::col_skip(), .default = readr::col_character())
  data_cols <- readr::cols(X1 = readr::col_character(), .default = readr::col_double())
  header <- readr::read_delim(file, ";", n_max = 10, col_types = header_cols, 
                              col_names = FALSE, locale = locale, trim_ws = TRUE, progress = FALSE)
  data <- readr::read_delim(file, ";", skip = 10, col_types = data_cols, 
                            col_names = FALSE, locale = locale, trim_ws = TRUE, progress = FALSE)
  header <- header[c(1, 4, 8, 7), ]
  data <- airmo_wide_to_long2(header, data, tz, na.rm)
  
  return(data)
}







read_bafu_zip_shp <- function(url, path_destination) {
  
  temp <- tempfile(tmpdir = path_destination, fileext = ".zip")
  download.file(url = url, dest = temp)
  files <- unzip(temp, list = TRUE)
  unzip(temp, exdir = path_destination)
  unlink(temp)
  shp <- sf::read_sf(fs::path(path_destination, files$Name[stringr::str_detect(files$Name, ".shp")]))
  # shp <- raster::shapefile(fs::path(path_destination, files$Name[stringr::str_detect(files$Name, ".shp")]))
  # shp <- terra::vect(fs::path(path_destination, files$Name[stringr::str_detect(files$Name, ".shp")]))
  unlink(fs::path(path_destination, files$Name))
  
  return(shp)
} 






get_geolion_wcs <- function(coverage, capability, name, na_value = c(0,-999), divisor = 10, crs = 2056) {
  
  chla <- capability$findCoverageSummaryById(coverage)
  # des <- chla$getDescription()
  # des$rangeType$field$nilValues
  data <- 
    chla$getCoverage() %>% 
    stars::st_as_stars() %>% 
    sf::st_set_crs(value = crs)
  data <- setNames(data, "value")
  data <-
    data %>% 
    dplyr::mutate(
      value = ifelse(value %in% na_value, NA, value),
      value = value / divisor
    )
  data <- setNames(data, name)
  
  return(data)
}


### wrapper ...
get_map <- function(coverage, capablilitylist, maplist, parameter, grid, boundary) {
  
  print(coverage)
  capability <- get_geolion_wcs_capabilities_from_list(capabilities, maplist, coverage, parameter)
  data <- get_geolion_wcs(coverage, capability, parameter) 
  data <- aggregate_to_grid(data, grid, parameter, boundary)
  
  return(data)
}







### courtesy of Statistikamt, modified
read_bfs_statpop_data <- function(year, path_destination) {
  
  # derive dataset url
  bfs_nr <- paste0("ag-b-00.03-vz", year, "statpop")
  meta_url <- gsub("bfs_nr", bfs_nr, "https://www.bfs.admin.ch/bfs/de/home/statistiken/kataloge-datenbanken.assetdetail.bfs_nr.html")
  
  command <- paste0("curl ", meta_url)
  asset_page <- system(command, intern = TRUE)
  
  asset_page_total <- paste0(asset_page, collapse = " ")
  
  #asset_page <- RCurl::getURLContent(meta_url, .encoding = "latin1")
  asset_number <- gsub(".*(https://.*assets/[0-9]+/).*", "\\1", asset_page_total)
  asset_number <- gsub(".*/([0-9]+)/", "\\1", asset_number)

  
  download_url <- paste0("https://www.bfs.admin.ch/bfsstatic/dam/assets/",asset_number,"/master")
  
  # download the ZIP file to a temporary location
  temp <- tempfile(tmpdir = path_destination, fileext = ".zip")
  command <- paste0("curl ", download_url, " --output ", temp)
  system(command, intern = TRUE)

  
  # list files within the ZIP archive
  files_in_zip <- 
    archive::archive(temp) %>% 
    dplyr::mutate(path_lower = tolower(path)) %>% 
    dplyr::filter(stringr::str_detect(path_lower, "^.*statpop\\d{4}\\.csv$")) %>% 
    dplyr::select(path) %>% 
    dplyr::pull(path)
  
  # Select the file that matches the pattern "STATPOP####.csv" (case-insensitive)
  # Assuming there's only one such file per archive
  target_file <- files_in_zip[stringr::str_detect(tolower(files_in_zip), "statpop\\d{4}\\.csv")]
  
  if (length(target_file) == 0) {
    stop("No file matching 'STATPOP[year].csv' pattern found in the ZIP archive.")
  }
  
  # Assuming the first match is the file we want (if there are multiple matches)
  largest_file <- target_file[1]
  
  # Open a connection to the matched file inside the ZIP
  con <- archive::archive_read(temp, file = largest_file)
  
  # Read the file into a stars raster
  data <- read_statpop_csv(con, year = extract_year(largest_file))
  
  # Remove the temporary file
  unlink(temp)
  
  return(data)
}





























