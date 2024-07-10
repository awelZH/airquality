
# read input files
# ------------------------------------------------------------


# function to read *.csv with air pollutant year-statistics exported from https://www.arias.ch/ibonline/ib_online.php and restructure the data similar to a standard long-format (see rOstluft::format_rolf())
read_monitoring_data_nabel_arias_csv <- function(file, keep_incomplete = FALSE, encoding = "latin1", tz = "Etc/GMT-1"){ 
  
  locale <- readr::locale(encoding = encoding)
  data <- readr::read_delim(file, delim =";", locale = locale)
  col_names <- range(as.numeric(names(data)), na.rm = TRUE)
  data <- dplyr::mutate_if(data, is.numeric, as.character)
  data_long <- tidyr::pivot_longer(
    data, 
    cols = as.character(min(col_names):max(col_names)),
    names_to = "starttime"
  )
  data_long_clean <- 
    data_long |> 
    dplyr::mutate(
      value = dplyr::case_when(
        keep_incomplete ~ as.numeric(gsub("\\*|\\;", "", value)),
        TRUE ~ as.numeric(value)
      ),
      interval = "y1",
      Schadstoff = dplyr::case_when(
        Messparameter == "höchster 98%-Wert eines Monats" ~ "O3_max_98p_m1",
        Messparameter == "Anzahl Stundenmittel > 120 µg/m3" ~ "O3_nb_h1>120",
        Messparameter == "Dosis AOT40f" ~ "O3_AOT40",
        Schadstoff == "Partikelanzahl" ~ "PN",
        Schadstoff == "EC / Russ" ~ "eBC",
        TRUE ~ Schadstoff
      ),
      Einheit = ifelse(Einheit == "ppm·h", "ppm*h", Einheit),
      starttime = as.POSIXct(paste0(starttime, "-01-01"), tz = "Etc/GMT-1"),
      source = factor("NABEL (BAFU & Empa)")
    ) |> 
    dplyr::select(
      starttime,
      site = Station,
      parameter = Schadstoff,
      interval,
      unit = Einheit,
      value,
      source
    ) |> 
    dplyr::mutate_if(is.character, as.factor)
  
  return(data_long_clean)
}





read_site_meta_nabel_arias_csv <- function(file, encoding = "latin1") {
  
  locale <- readr::locale(encoding = encoding)
  meta <- readr::read_delim(file, delim =";", col_select = c("Station", "Ost Y", "Nord X", "Höhe", "Zonentyp", "Stationstyp"), locale = locale)
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
  meta <- dplyr::mutate(meta,
                        ifelse(zone == "vorstädtisch", "klein-/vorstädtisch", zone),
                        site_long = site,
                        source = "NABEL (BAFU & Empa)"
  )
  meta <- dplyr::select(meta, site, site_long, x, y, masl, zone, type, source)
  
  return(meta)
}




read_site_meta_ostluft_metadb_csv <- function(file, encoding = "UTF-8") {
  
  locale <- readr::locale(encoding = encoding)
  meta <- readr::read_delim(file, delim =",", locale = locale)
  meta <- 
    meta |> 
    dplyr::filter(msKT == "ZH" & !is.na(msNameAirMo) & !is.na(scSiedlungsgroesse) & !is.na(scVerkehrslage)) |>
    dplyr::mutate(
      site_long = paste(msOrt, msOrtsteil, sep = " - "),
      scSiedlungsgroesse = stringr::str_trim(scSiedlungsgroesse),
      scVerkehrslage = stringr::str_trim(scVerkehrslage)
    ) |>
    dplyr::select(msNameAirMo, site_long, spXCoord, spYCoord, spHoehe, scSiedlungsgroesse, scVerkehrslage) |> 
    dplyr::rename(
      site = msNameAirMo,
      x = spXCoord,
      y = spYCoord,
      masl = spHoehe,
      zone = scSiedlungsgroesse, 
      type = scVerkehrslage
    ) |> 
    dplyr::mutate(
      zone = recode_ostluft_meta_zone(zone),
      type = recode_ostluft_meta_type(type),
      source = "OSTLUFT"
    ) 
  
  return(meta)
}




# function to read *.csv with air pollutant data from (internal) Airmo (OSTLUFT air quality database) export and restructure the data similar to a standard long-format (see rOstluft::format_rolf()); based on rOstluft::read_airmo_csv()
read_monitoring_data_ostluft_airmo_csv <- function(file, encoding = "UTF-8", tz = "Etc/GMT-1", na.rm = TRUE){
  
  locale <- readr::locale(encoding = encoding)
  header_cols <- readr::cols(X1 = readr::col_skip(), .default = readr::col_character())
  data_cols <- readr::cols(X1 = readr::col_character(), .default = readr::col_double())
  header <- readr::read_delim(file, ";", n_max = 10, col_types = header_cols, 
                              col_names = FALSE, locale = locale, trim_ws = TRUE, progress = FALSE)
  data <- readr::read_delim(file, ";", skip = 10, col_types = data_cols, 
                            col_names = FALSE, locale = locale, trim_ws = TRUE, progress = FALSE)
  header <- header[c(1, 4, 8, 7), ]
  data <- restructure_airmo_wide_to_long2(header, data, tz, na.rm)
  data <- dplyr::mutate(data, source = factor("OSTLUFT"))
  
  return(data)
}













