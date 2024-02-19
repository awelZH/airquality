
### function to read *.csv with air pollutant year-statistics exported from https://www.arias.ch/ibonline/ib_online.php and restructure the data similar to a standard long-format (see rOstluft::format_rolf())
read_arias <- function(file, encoding = "latin1", tz = "Etc/GMT-1"){ 
  
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
                        metric = dplyr::recode(metric, !!!c("höchster 98%-Wert eines Monats" = "O3_max_98%_m1", 
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
  
  locale <- readr::locale(encoding = encoding)
  header <- readr::read_delim(file, delim = "\t", n_max = 4, col_names = FALSE, locale = locale)
  site <- dplyr::pull(dplyr::filter(header, X1 == "Station"), X2)
  parameter <- dplyr::pull(dplyr::filter(header, X1 == "Messwert"), X2)
  unit <- dplyr::pull(dplyr::filter(header, X1 == "Einheit"), X2)
  data <- readr::read_delim(file, delim = "\t", col_names = FALSE, skip = 4, locale = locale)
  data <- dplyr::rename(data, endtime = X1, value = X2)
  data <- dplyr::mutate(data, 
                        site = dplyr::case_when(site == "DUE" ~ "Dübendorf-EMPA", site == "ZUE" ~ "Zürich-Kaserne", TRUE ~ site),
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
  meta <- readr::read_delim(file, delim =";", col_select = c("Station", "Ost Y", "Nord X", "Höhe", "Zonentyp", "Stationstyp"), locale = locale)
  meta <- dplyr::distinct(meta, Station, `Ost Y`, `Nord X`, `Höhe`, Zonentyp, Stationstyp)
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




### ... to be roughly in line with https://www.bafu.admin.ch/bafu/de/home/themen/luft/publikationen-studien/publikationen/immissionsmessung-von-luftfremdstoffen.html
### however, the OSTLUFT site classes are - as categories - not entirely consistent with the new Immissionsmessempfehlung. We will need to put future effort in a reclassifiacation
aggregate_ostluft_meta_zone <- function(zone) { 
  
  zone <- 
    dplyr::case_when(
      as.numeric(stringr::str_remove(zone, "H")) %in% c(21:23, 31:33) ~ "städtisch", # OSTLUFT: > 20'000 Gesamteinwohner; BAFU: > 1500 Einwohner/km2 und Gesamteinwohnerzahl > 50 000
      as.numeric(stringr::str_remove(zone, "H")) %in% 11:13 ~ "klein-/vorstädtisch", # OSTLUFT: > 1'000 Gesamteinwohner; BAFU: > 300 Einwohner/km2 im überbauten Gebiet und Gesamteinwohnerzahl > 5000
      as.numeric(stringr::str_remove(zone, "H")) == 0 ~ "ländlich", # OSTLUFT: < 1'000 Gesamteinwohner; BAFU: Gebiete mit geringer Siedlungsdichte (< 300 Einwohner/km2) oder kleinere Ortschaften (< 5000 Einwohner)
      TRUE ~ zone 
    )
  
  return(zone)
}



### ... to be roughly in line with https://www.bafu.admin.ch/bafu/de/home/themen/luft/publikationen-studien/publikationen/immissionsmessung-von-luftfremdstoffen.html
### however, the OSTLUFT site classes are - as categories - not entirely consistent with the new Immissionsmessempfehlung. We will need to put future effort in a reclassifiacation
aggregate_ostluft_meta_type <- function(type) { 
  
  type <- 
    dplyr::case_when(
      as.numeric(stringr::str_remove(type, "S")) %in% c(10:13, 20:23, 30:33) ~ "verkehrsbelastet", # OSTLUFT: DTV_S > 10'000; BAFU: has a finer scale that begins at DTV > 3'000 and cerctain max distance to street 
      as.numeric(stringr::str_remove(type, "S")) == 0 ~ "Hintergrund", # OSTLUFT: DTV_S < 10'000 & street more than 50m (in cities) or 300m (outside of cities) away; BAFU: see above
      TRUE ~ type 
    )
  
  return(type)
}



### internal function for read_airmo_csv2() to restructure data; based on similar function in rOstluft-package
airmo_wide_to_long2 <- function(header, data, tz = "Etc/GMT-1", na.rm = TRUE){
  
  colnames(data)[1] <- "starttime"
  col_ids <- rlang::names2(data)[-1]
  
  sites <- c(header[1, ], recursive = TRUE)
  sites <- rlang::set_names(sites, col_ids)
  parameters <- c(header[2, ], recursive = TRUE)
  parameters <- rlang::set_names(parameters, col_ids)
  intervals <- c(header[3, ], recursive = TRUE)
  intervals <- rlang::set_names(intervals, col_ids)
  units <- c(header[4, ], recursive = TRUE)
  units <- rlang::set_names(units, col_ids)
  
  data <- dplyr::mutate(data, starttime = lubridate::parse_date_time(.data$starttime, c("dmYHMS", "dmYHM", "dmY"), tz = tz))
  data <- tidyr::gather(data, "id", "value", -"starttime", na.rm = na.rm, factor_key = TRUE)
  data <- dplyr::mutate(data,
                        site = dplyr::recode(.data$id, !!!sites),
                        parameter = dplyr::recode(.data$id, !!!parameters),
                        interval = dplyr::recode(.data$id, !!!intervals),
                        unit = dplyr::recode(.data$id, !!!units)
  )
  data <- dplyr::select(data, "starttime", "site", "parameter", "interval", "unit", "value")
  
  return(data)
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




### function to make sure that there are no duplicate measurements per site / year / unit for data with interval = y1 in format rOstluft::format_rolf() 
### in case there have been NO2 monitor and passive sampler measurements (prefer monitor data = reference method); 
### same for PM10 monitor and high volume sampler measurements (prefer high-volume-sampler data = reference method);
### same for PM2.5 monitor and high volume sampler measurements (prefer high-volume-sampler data = reference method)
remove_duplicate_y1 <- function(data){
  
  replace_no2_ps <- function(parameter, value){
    if (sum(c("NO2", "NO2_PS") %in% parameter) == 2) {
      if (!is.na(value[which(parameter == "NO2")])){
        value[which(parameter == "NO2_PS")] <- NA
      }
    }
    return(value)
  }
  
  replace_pm10 <- function(parameter, value){
    if (sum(c("PM10", "PM10h") %in% parameter) == 2) {
      if (!is.na(value[which(parameter == "PM10h")])){
        value[which(parameter == "PM10")] <- NA
      }
    }
    return(value)
  }
  
  replace_pm25 <- function(parameter, value){
    if (sum(c("PM2.5", "PM2.5h") %in% parameter) == 2) {
      if (!is.na(value[which(parameter == "PM2.5h")])){
        value[which(parameter == "PM2.5")] <- NA
      }
    }
    return(value)
  }
  
  data <- 
    data %>% 
    dplyr::group_by(starttime, site, unit) %>% 
    dplyr::mutate(
      value = replace_no2_ps(parameter, value),
      value = replace_pm10(parameter, value),
      value = replace_pm25(parameter, value)
    ) %>% 
    dplyr::ungroup()
  
  return(data)
}





### function to plot standard timeseries of yearly values
ggplot_timeseries <- function(data, mapping = ggplot2::aes(x = starttime, y = value, color = siteclass), lims = c(0,NA), titlelab = NULL, captionlab = NULL, pointshape = 19, pointsize = 2,
                              threshold = list(value = NA, color = "gray30", label = NULL, labelsize = 4, linetype = 2, linesize = 1), 
                              theme = ggplot2::theme_minimal()) {
  
  plot <- 
    ggplot2::ggplot(data, mapping = mapping) + 
    ggplot2::geom_point(size = pointsize, shape = pointshape) +
    ggplot2::scale_x_datetime(expand = c(0.01,0.01)) +
    ggplot2::scale_y_continuous(limits = lims, expand = c(0.01,0.01)) +
    titlelab +
    captionlab +
    theme
  
  if (!is.na(sum(threshold$value))){
    text <- tibble::tibble(x = rep(min(data$starttime), length(threshold$value)), y = threshold$value, label = threshold$labels)
    plot <-
      plot + 
      ggplot2::geom_hline(yintercept = threshold$value, color = threshold$color, linetype = threshold$linetype, linewidth = threshold$linesize) +
      ggplot2::geom_text(data = text, mapping = ggplot2::aes(x = x, y = y, label = label), size = threshold$labelsize, 
                         hjust = 0, vjust = 0, nudge_y = pmax(0, 0.01 * max(lims), na.rm = TRUE), inherit.aes = FALSE)
  }
  
  return(plot)
}



aggregate_population_weighted_mean <- function(data, y, group = "geodb_oid") {
  
  data <- 
    data %>% 
    sf::st_drop_geometry() %>%
    dplyr::mutate(!!y := ifelse(art_code != 1, NA, !!rlang::sym(y))) %>%
    na.omit() %>%
    dplyr::group_by(!!rlang::sym(group)) %>% 
    dplyr::summarise(!!y := population_weighted_mean(!!rlang::sym(y), population)) %>% 
    dplyr::ungroup()
  
  return(data)
}




aggregate_exposition_distrib <- function(data, y, fun = function(x) {floor(x) + 0.5}) { # fun: abgerundet auf 1, Klassenmitte
  
  data <- 
    data %>% 
    dplyr::select(!!y, population) %>% 
    tibble::as_tibble() %>% 
    na.omit() %>% 
    dplyr::group_by(!!rlang::sym(y) := fun(!!rlang::sym(y))) %>% 
    dplyr::summarise(population = sum(population)) %>%
    dplyr::ungroup()
  
  return(data)
}



exposition_distrib_cumulative <- function(data, y) {
  
  data <- 
    data %>% 
    dplyr::filter(population > 0) %>% 
    dplyr::arrange(!!y) %>% 
    dplyr::mutate(population_relative = cumsum(population) / sum(population))
  
  return(data)
}



### function to plot exposition distribution histogram
ggplot_expo_hist <- function(data, x, y, barwidth = 1, xlims = c(0,NA), xbreaks = waiver(), titlelab = NULL, captionlab = NULL, xlabel = NULL,
                             threshold = list(value = NA, label = NULL, labelsize = 4, linetype = 2, linesize = 1),
                             fill_scale = NULL, theme = ggplot2::theme_minimal()) {
  
  if (is.null(fill_scale)) {
    mapping <- ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y))
  } else {
    mapping <- ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y), fill = !!rlang::sym(x))
  }
  
  plot <-
    ggplot2::ggplot(data, mapping = mapping) +
    ggplot2::geom_bar(stat = "identity", color = NA, width = barwidth) +
    ggplot2::scale_x_continuous(limits = xlims, breaks = xbreaks, expand = c(0.01,0.01)) +
    ggplot2::scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01), labels = function(x) format(x, big.mark = "'", scientific = FALSE)) +
    fill_scale +
    xlabel +
    titlelab +
    captionlab +
    theme +
    ggplot2::theme(axis.title.x = ggplot2::element_text()) 
  
  if (!is.na(sum(threshold$value))){
    text <- tibble::tibble(x = threshold$value, label = threshold$labels)
    plot <-
      plot +
      ggplot2::geom_vline(xintercept = threshold$value, color = threshold$color, linetype = threshold$linetype, linewidth = threshold$linesize) +
      ggplot2::geom_text(data = text, mapping = ggplot2::aes(x = x, y = 0, label = label), size = threshold$labelsize,
                         hjust = 0, vjust = 0, angle = 90, nudge_x = pmin(0, -0.01 * max(xlims), na.rm = TRUE), inherit.aes = FALSE)
  }
  
  return(plot)
}




### function to plot relative cumulative exposition distribution
ggplot_expo_cumulative <- function(data, x, y, linewidth = 1, xlims = c(0,NA), xbreaks = waiver(), titlelab = NULL, captionlab = NULL, xlabel = NULL,
                             threshold = list(value = NA, label = NULL, labelsize = 4, linetype = 2, linesize = 1),
                             theme = ggplot2::theme_minimal()) {
  
  plot <-
    ggplot2::ggplot(data, mapping = ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y))) +
    ggplot2::geom_line(linewidth = linewidth, color = "gray40") +
    ggplot2::scale_x_continuous(limits = xlims, breaks = xbreaks, expand = c(0.01,0.01)) +
    ggplot2::scale_y_continuous(limits = c(0,1), expand = c(0.01,0.01), labels = scales::percent_format()) +
    xlabel +
    titlelab +
    captionlab +
    theme +
    ggplot2::theme(axis.title.x = ggplot2::element_text()) 
  
  if (!is.na(sum(threshold$value))){
    text <- tibble::tibble(x = threshold$value, label = threshold$labels)
    plot <-
      plot +
      ggplot2::geom_vline(xintercept = threshold$value, color = threshold$color, linetype = threshold$linetype, linewidth = threshold$linesize) +
      ggplot2::geom_text(data = text, mapping = ggplot2::aes(x = x, y = 0, label = label), size = threshold$labelsize,
                         hjust = 0, vjust = 0, angle = 90, nudge_x = pmin(0, -0.01 * max(xlims), na.rm = TRUE), inherit.aes = FALSE)
  }
  
  return(plot)
}




### for O3 peak-season calculation: function identifying relevant months per year for metric calculation based on O3 monthly mean data
consecutive_months <- function(starttime, o3_m1) { 
  
  data <- tibble::tibble(starttime, O3 = o3_m1)
  data <- dplyr::arrange(data, starttime)
  data <- dplyr::mutate(data,
                        O3_runmean = zoo::rollapply(.data$O3, 6, mean, fill = NA, align = "left"),
                        month_start = lubridate::month(data$starttime),
                        month_end = pmin(month(data$starttime) + 5, 12),
                        n_months = month_end - month_start + 1
  )
  complete_months <- dplyr::filter(data, n_months == 6)
  peak_start <- dplyr::pull(dplyr::slice(complete_months, which.max(.data$O3_runmean)), starttime)
  if (length(peak_start) == 0) {
    peak_season <- rep(NA, nrow(data))
  } else {
    peak_season <- data$starttime %in% (peak_start + months(0:5))
    peak_season <- ifelse(data$n_months == 6 & is.na(data$O3_runmean), NA, peak_season)
  }
  
  return(peak_season)
}





### function to calculate daily maximum 8h running-mean O3 concentration based on O3 1h data in rOstluft::format_rolf()
max_mean_h8gl <- function(data) { # how to solve data coverage?
  
  data <-
    data %>% 
    dplyr::group_by(date = lubridate::as_date(starttime), site, parameter, unit) %>% 
    dplyr::mutate(value = zoo::rollapply(.data$value, 8, mean, fill = NA, align = "right")) %>%
    dplyr::slice(which.max(.data$value)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      starttime = starttime - lubridate::hours(7),
      parameter = factor(paste0(parameter, "_max_mean_h8gl")),
      interval = factor("d1")
    ) %>% 
    dplyr::arrange(site, parameter, starttime) %>% 
    dplyr::select(starttime, site, parameter, interval, unit, value)
  
  return(data)
}





### function to calculate O3 peak-season concentration per year and site, based on data as hourly means in rOstluft::format_rolf() 
calc_O3_peakseason <- function(data, min_coverage = 9/12) { # min_coverage: data coverage in months per year (9/12 because in early times, they used to not measure O3 during winter months)
  
  ### calculate O3 monthly means to derive peak season
  data_m1 <- rOstluft::resample(data, statistic = "mean", new_interval = "m1", data_thresh = 0.8)
  
  ### calculate O3 daily maximum 8 hour mean 
  # data_h8gl <- rOstluft::resample(data, statistic = "mean", new_interval = "h8gl")
  # data_max_mean_h8gl <- rOstluft::resample(data_h8gl, statistic = "max", new_interval = "d1")
  data_max_mean_h8gl <- max_mean_h8gl(data)
  
  ### make sure years are sufficiently data-covered
  coverage <-
    data_m1 %>%
    dplyr::group_by(year = year(lubridate::floor_date(starttime, unit = "1 year")), site) %>%
    dplyr::summarise(n = sum(!is.na(value))) %>%
    dplyr::ungroup()
  
  ### identify peak-season 6 consecutive months per year and site
  peak_season <-
    data_m1 %>%
    dplyr::mutate(year = lubridate::year(lubridate::floor_date(starttime, unit = "1 year"))) %>%
    dplyr::left_join(coverage, by = c("year", "site")) %>%
    dplyr::filter(n/12 >= min_coverage) %>%
    dplyr::group_by(year = lubridate::floor_date(starttime, unit = "1 year"), site) %>%
    dplyr::mutate(
      peak_season = consecutive_months(starttime, value),
      n = sum(peak_season, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(starttime, site, peak_season, n)
  
  ### join with "max_mean_h8gl" data and filter only days within relevant consecutive months
  ### calc mean per year (or more precisely peak-season) and site (= WHO metric peak-season)
  data_peakseason <-
    data_max_mean_h8gl %>%
    dplyr::mutate(starttime = lubridate::floor_date(starttime, unit = "1 month")) %>% 
    dplyr::left_join(peak_season, by = c("starttime", "site")) %>%
    dplyr::filter(peak_season) %>%
    dplyr::group_by(starttime = lubridate::floor_date(starttime, unit = "1 year"), site, unit) %>%
    dplyr::mutate(value = ifelse(n < 6, NA, value)) %>% 
    dplyr::summarise(value = mean(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      parameter = "O3_peakseason_mean_d1_max_mean_h8gl",
      interval = "y1",
    ) %>%
    dplyr::mutate_if(is.character, factor) %>%
    dplyr::select(starttime, site, parameter, interval, unit, value) %>% 
    dplyr::arrange(site, parameter, starttime)
  
  return(data_peakseason)
}




### function to extract target threshold values from overall threshold data for plotting with ggplot_timeseries()
get_threshold <- function(threshold_values, pollutant = NULL, aggregation = "y1", metric = "mean", unit = "µg/m3", 
                          source = c("LRV Grenzwert", "WHO Richtwert")) {
  
  thresholds <-
    threshold_values %>%
    dplyr::filter(
      source %in% !!source &
        pollutant == !!pollutant & aggregation == !!aggregation &
        metric == !!metric & unit == !!unit
    ) %>%
    dplyr::arrange(source)
  
  thresholds <-
    list(
      value = thresholds$threshold,
      color = thresholds$col,
      labels = thresholds$source,
      labelsize = thresholds$lbsz,
      linetype = thresholds$lty,
      linesize = thresholds$lsz
    )
  
  return(thresholds)
}




combine_thresholds <- function(data, threshold_values) {
  
  data <- 
    threshold_values %>% 
    dplyr::select(source, pollutant, metric, aggregation, threshold) %>% 
    dplyr::rename(
      parameter = pollutant,
      interval = aggregation
    ) %>% 
    dplyr::mutate(
      parameter = dplyr::case_when(
        metric == "number hourly mean values > 120 µg/m3" & parameter == "O3" ~ "O3_nb_h1>120",
        metric == "monthly 98%-percentile of ½ hour mean values ≤ 100 µg/m3" & parameter == "O3" ~ "O3_max_98%_m1",
        metric == "mean of daily maximum 8-hour mean concentration in the six consecutive months with the highest six-month running-mean concentration" & parameter == "O3" ~ "O3_peakseason_mean_d1_max_mean_h8gl",
        TRUE ~ parameter
      ),
      interval = dplyr::recode(interval, !!!c("m1" = "y1", "peak-season" = "y1"))
    ) %>% 
    dplyr::select(-metric) %>% 
    tidyr::spread(source, threshold) %>% 
    dplyr::right_join(data, by = c("parameter", "interval")) %>% 
    dplyr::select(starttime, site, parameter, interval, unit, value, siteclass, `LRV Grenzwert`, `WHO Richtwert`, source)
  
  return(data)
  
}




# lbi <- function(PM10_rel, NO2_rel, O3_rel) {
#   
#   cut_val_rel <- function(x) {
#     dplyr::case_when(
#       x <= 0.5 ~ 1,
#       x > 0.5 & x <= 0.75 ~ 2,
#       x > 0.75 & x <= 1 ~ 3,
#       x > 1 & x <= 1.25 ~ 4,
#       x > 1.25 & x <= 1.5 ~ 5,
#       x > 1.5 ~ 6
#     )
#   }
#   
#   LBI <- (4.5 * cut_val_rel(PM10_rel) + 4.5 * cut_val_rel(NO2_rel) + cut_val_rel(O3_rel)) / 10
#   LBI <- dplyr::case_when(
#     LBI <= 1.5 ~ 1,
#     LBI > 1.5 & LBI <= 2.5 ~ 2,
#     LBI > 2.5 & LBI <= 3.5 ~ 3,
#     LBI > 3.5 & LBI <= 4.5 ~ 4,
#     LBI > 4.5 & LBI <= 5.5 ~ 5,
#     LBI > 5.5 ~ 6
#   )
#   
#   return(LBI)
# }



# calc_lbi <- function(data, threshold_values) {
#   
#   data <- combine_thresholds(data, threshold_values)
#   data <- 
#     data %>% 
#     dplyr::mutate(value_relative = value / `LRV Grenzwert`) %>% 
#     dplyr::filter(parameter %in% c("NO2", "PM10", "O3_max_98%_m1")) %>% 
#     dplyr::group_by(starttime, site, interval, unit, source) %>% 
#     dplyr::select(parameter, value_relative) %>% 
#     tidyr::spread(parameter, value_relative) %>% 
#     dplyr::mutate(LBI = lbi(PM10, NO2, `O3_max_98%_m1`)) %>% 
#     dplyr::ungroup() %>% 
#     dplyr::select(starttime, site, interval, LBI, source) %>% 
#     tidyr::gather(parameter, value, -starttime, -site, -interval, -source) %>% 
#     dplyr::mutate(
#       parameter = factor(parameter),
#       unit = factor(NA)
#     )
#   
#   return(data)
# }




# recode_lbi <- function(lbi) {
#   
#   lbi <- dplyr::recode(as.character(lbi), !!!c("1" = "gering", "2" = "mässig", "3" = "deutlich", "4" = "erheblich", "5" = "hoch", "6" = "sehr hoch"))
#   lbi <- factor(lbi, levels = c("gering", "mässig", "deutlich", "erheblich", "hoch", "sehr hoch"))
#   
#   return(lbi)
# }




### ... only a valid approximation for vehicles weighting less than 3.5 t
calc_vsp <- function(speed, accel, slope, # speed in m/s, accel in m/s/s, slope as ratio, mass = 3.5 in t
                     vsp.a = 1.1, vsp.b = 0.132, vsp.c = 0.000302, vsp.g = 9.81) {
  
  vsp <- speed * (vsp.a * accel + (vsp.g * slope) + vsp.b) + (vsp.c * speed^3)
  
  return(vsp)
}




calc_rsd_nox_emission <- function(NO, p, CO2, CO, HC) { # all concentrations in mixing ratios as percent
  
  Q <- CO / CO2
  Q1 <- HC / CO2
  Q2 <- NO / CO2
  NO_emission <- 30 * Q2 * 860 / ((1 + Q + 6 * Q1) * 12)
  NOx_emission <- NO_emission * 46 / (30 * (1 - p))
  
  return(NOx_emission)
}




### aggregate RSD data calculating: n, percentiles, median, mean, standard deviation, standard error
aggregate_groups_rsd <- function(data, y, groups = c("vehicle_type", "vehicle_fuel_type", "vehicle_euronorm"),
                                 nmin = 100, perc = list(ymin = 0.05, lower = 0.25, middle = 0.5, upper = 0.75, ymax = 0.95)) {
  
  data <-
    data %>%
    dplyr::group_by_at(dplyr::vars(groups)) %>%
    dplyr::summarise(
      n = length(na.omit(!!rlang::sym(y))),
      min = quantile(!!rlang::sym(y), perc$ymin, na.rm = TRUE),
      lower = quantile(!!rlang::sym(y), perc$lower, na.rm = TRUE),
      middle = quantile(!!rlang::sym(y), perc$middle, na.rm = TRUE),
      upper = quantile(!!rlang::sym(y), perc$upper, na.rm = TRUE),
      max = quantile(!!rlang::sym(y), perc$ymax, na.rm = TRUE),
      mean = mean(!!rlang::sym(y), na.rm = TRUE),
      standarddeviation = sd(!!rlang::sym(y), na.rm = TRUE),
      standarderror = standarddeviation / sqrt(n)
    ) %>%
    ungroup()
  
  data_all <-
    data %>% 
    dplyr::select(tidyr::all_of(groups)) %>% 
    dplyr::distinct_all() %>% 
    tidyr::expand(tidyr::crossing(!!!rlang::syms(groups)))
  
  data <- dplyr::left_join(data_all, data, by = groups) 
  data <- dplyr::mutate_at(data, c("min", "lower", "middle", "upper", "max", "mean", "standarddeviation", "standarderror"), list(~ifelse(n < nmin, NA, .)))
  data <- dplyr::mutate_at(data, c("min", "lower", "middle", "upper", "max", "mean", "standarddeviation", "standarderror"), list(~ifelse(is.nan(.), NA, .)))
  data <- dplyr::mutate_at(data, c("min", "lower", "middle", "upper", "max", "mean", "standarddeviation", "standarderror"), list(~ifelse(is.infinite(.), NA, .)))
  data$n <- ifelse(is.na(data$n), 0, data$n)
  
  return(data)
}



### function to ggplot emissions employing structured coloring... this is not ideal, but the best I can do
ggplot_emissions <- function(data, cols, pos = "stack", width = 0.8, theme_emissions = ggplot2::theme_minimal()) {
  
  groups <-
    data %>% 
    dplyr::group_by(pollutant, unit, sector, subsector) %>% 
    dplyr::summarise(emission = mean(emission)) %>% 
    dplyr::group_by(pollutant, unit, sector) %>% 
    dplyr::mutate(
      subsector = dplyr::case_when(
        emission < min(sort(emission, decreasing = TRUE)[1:3]) ~ "sonstige",
        TRUE ~ subsector
      )
    ) %>%
    dplyr::group_by(pollutant, unit, sector, subsector) %>% 
    dplyr::summarise(emission = sum(emission)) %>% 
    dplyr::group_by(pollutant, unit) %>% 
    dplyr::mutate(
      subsector = dplyr::case_when(
        subsector != "weitere" & emission < 0.02 * sum(emission)  ~ "sonstige",
        TRUE ~ subsector
      )
    ) %>%
    dplyr::group_by(pollutant, unit, sector, subsector) %>% 
    dplyr::summarise(emission = sum(emission)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(others = stringr::str_detect(subsector, "sonstige")) %>% 
    dplyr::arrange(sector, desc(others), emission) %>% 
    dplyr::mutate(subsector = paste0(sector, " / ", subsector)) %>% 
    dplyr::mutate(rootcol = dplyr::recode(sector, !!!cols)) %>% 
    dplyr::group_by(sector) %>% 
    dplyr::mutate(col = colorRampPalette(c(unique(rootcol), shades::brightness(unique(rootcol), 0.6)))(length(subsector))) %>% 
    dplyr::ungroup()
  
  plot <- 
    data %>% 
    dplyr::mutate(
      subsector = paste0(sector, " / ", subsector),
      subsector = dplyr::case_when(
        !(subsector %in% groups$subsector) ~ paste0(sector," / sonstige"),
        TRUE ~ subsector
      ),
      subsector = factor(subsector, levels = groups$subsector)
    ) %>% 
    dplyr::group_by(year, pollutant, unit, sector, subsector) %>% 
    dplyr::summarise(emission = sum(emission)) %>% 
    dplyr::ungroup() %>% 
    ggplot2::ggplot(aes(x = factor(year), y = emission, fill = subsector)) +
    ggplot2::geom_bar(stat = "identity", position = pos, width = width) + 
    ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = "'"), expand = c(0.01,0.01)) +
    scale_fill_manual(values = setNames(groups$col, groups$subsector)) +
    theme_emissions +
    ggplot2::theme(legend.title = ggplot2::element_blank())
  
  return(plot)
}



longtitle <- function(x) {
  
  long <- dplyr::case_when(
    x == "PM10" ~ "Feinstaub PM10",
    x == "PM2.5" ~ "Feinstaub PM2.5",
    x == "NMVOC" ~ "nicht-Methan Kohlenwasserstoffe",
    x == "NH3" ~ "Ammoniak",
    x == "CO" ~ "Kohlenstoffmonoxid",
    x == "SO2" ~ "Schwefeldioxîd",
    x == "NOx" ~ "Stickoxide",
    x == "eBC" ~ "Russ",
    TRUE ~ x
  )
  
  return(long)
}



extract_year <- function(string) {as.numeric(stringr::str_extract(string, "(1|2)[0-9]{3}"))}



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



get_geolion_wcs <- function(coverage, capabilities, name, na_value = c(0,-999), divisor = 10, crs = 2056) {
  
  chla <- capabilities$findCoverageSummaryById(coverage)
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



read_statpop_csv <- function(file, year, crs = 2056) {
  
  var <- paste0("B", stringr::str_sub(year, 3, 4), "BTOT")
  delim <- ifelse(as.numeric(year) > 2015, ";", ",")
  data <- 
    file %>% 
    readr::read_delim(delim = delim, locale = readr::locale(encoding = "UTF-8")) %>% 
    dplyr::select(RELI, E_KOORD, N_KOORD, !!var) %>% 
    dplyr::rename(
      x = E_KOORD,
      y = N_KOORD,
      population = !!var
    ) %>% 
    # dplyr::mutate(year = as.numeric(year)) %>% 
    # sf::st_as_sf(coords = c("x", "y", "year"), dim = "XYZ") %>% 
    sf::st_as_sf(coords = c("x", "y"), dim = "XY") %>%
    sf::st_set_crs(value = crs) %>% 
    stars::st_rasterize() 
  
  return(data)
}



population_weighted_mean <- function(concentration, population) {sum(concentration * population, na.rm = TRUE) / sum(population, na.rm = TRUE)}



immission_colorscale <- function(...) {
  cols <- c("#004DA8", "#005ce6", "#0070ff", "#00c5ff", "#47d9fa", "#56f9fb", "#2e9c6b", "#38bd00", "#56d900", 
            "#51f551", "#ffff00", "#ffd400", "#ffa300", "#ff5200", "#ff0000", "#ff0094", "#de00a1", "#c500ba")
  return(rOstluft.plot:::scale_fill_gradientn_squished(..., colors = cols, na.value = NA))
}



### see here: https://gist.github.com/sotoattanito/8e6fad4b7322ceae9f14f342985f1681
round.off <- function (x, digits = 0) {
  
  posneg = sign(x)
  z = trunc(abs(x) * 10 ^ (digits + 1)) / 10
  z = floor(z * posneg + 0.5) / 10 ^ digits
  
  return(z)
}

