# read in arial data

locale <- readr::locale(encoding = encoding)

data <- readr::read_delim(file, delim =";", locale = locale)

col_names <- tail(names(data),1)

data <- dplyr::mutate_if(data, is.numeric, as.character)

data_long <- tidyr::pivot_longer(
  data, 
  cols = `1984`:col_names,
  names_to = "starttime"
)

data_long_clean <- data_long |> 
  dplyr::mutate(value = as.numeric(gsub("\\*|\\;", "", value)),
                interval = "y1") |> 
  dplyr::mutate(Schadstoff = dplyr::case_when(
    Messparameter == "höchster 98%-Wert eines Monats" ~ "O3_max_98p_m1",
    Messparameter == "Anzahl Stundenmittel > 120 µg/m3" ~ "O3_nb_h1>120",
    Messparameter == "Dosis AOT40f" ~ "O3_AOT40",
    Schadstoff == "Partikelanzahl" ~ "PN",
    Schadstoff == "EC / Russ" ~ "eBC",
    TRUE ~ Schadstoff
  )) |> 
  dplyr::mutate(Einheit = ifelse(Einheit == "ppm·h", "ppm*h", Einheit)) |> 
  dplyr::mutate(starttime = as.Date(paste0(starttime, "-01-01"))) |> 
  dplyr::select(
    starttime,
    site = Station,
    parameter = Schadstoff,
    interval,
    unit = Einheit,
    value
  ) |> 
  dplyr::mutate_if(is.character, as.factor)

