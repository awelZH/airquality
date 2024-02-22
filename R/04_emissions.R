### -----------------------------------------------
### -----------------------------------------------
### pollutant emission data 
### -----------------------------------------------
### -----------------------------------------------


### -----------------------------------------------
### read, restructure and combine data
### prepare data for plotting and write datasets
### -----------------------------------------------


# req <- httr2::request("https://opendata.swiss/api/3/action/package_list")
# req <- httr2::req_perform(req)
# opendatasets <- unlist(httr2::resp_body_json(req)$result) # all available datasets
# opendatasets[stringr::str_detect(opendatasets, "luftschadstoffemissionen-im-kanton-zurich")] # => exists
# opendatasets[stringr::str_detect(opendatasets, "messdaten-langjahriger-abgasmessungen-im-realen-fahrbetrieb-mittels-remote-sensing-rsd")] # => exists


### emission budgets of air pollutants in the Canton of Zürich, stratified for emission sector groups and subgroups 
req <- httr2::request(files$emissions$budget$opendata)
req <- httr2::req_perform(req)
emikat <- httr2::resp_body_json(req)$result        
emikat <- unlist(purrr::map(emikat$resources, function(x) x$url))
emikat <- emikat[stringr::str_detect(emikat, ".csv")]

data_emikat <-
  emikat %>%
  readr::read_delim(delim = ",") %>%
  dplyr::filter(kanton == "ZH" & !(untergruppe %in% c("Weitere Punktquellen OL", "Rheinschifffahrt", "Flugverkehr Genf"))) %>% # exclude some groups that might be redundant due to area distribution methodology
  dplyr::select(-kanton, -einheit_lang) %>% 
  dplyr::rename( # ... just for the sake of script language consistency
    year = jahr,
    pollutant = substanz,
    sector = hauptgruppe,
    subsector = untergruppe,
    municipality = gemeinde, 
    unit = einheit
  ) %>% 
  dplyr::mutate(
    pollutant = dplyr::case_when(pollutant == "BC" ~"eBC", TRUE ~ pollutant),
    source = "OSTLUFT"
  )


### evaluating NOx emissions by vehicle remote sensing (RSD) in the Canton of Zürich
### see: https://www.zh.ch/de/umwelt-tiere/luft-strahlung/luftschadstoffquellen/emissionen-verkehr/abgasmessungen-rsd.html
### read and restructure RSD data
req <- httr2::request(files$emissions$rsd$opendata)
req <- httr2::req_perform(req)
rsd <- httr2::resp_body_json(req)$result        
rsd <- unlist(purrr::map(rsd$resources, function(x) x$url))
rsd <- rsd[stringr::str_detect(rsd, ".csv")]

data_rsd <- 
  rsd %>% 
  lapply(function(x) readr::read_delim(x, delim = ",")) %>% 
  dplyr::bind_rows()

rsd_meta <-
  fs::path("data/input", files$emissions$rsd$meta) %>% 
  readr::read_delim(delim = ";") %>%
  dplyr::select(-source, -remark) %>% 
  tidyr::spread(parameter, value) %>% 
  dplyr::mutate(
    vehicle_type = factor(vehicle_type, levels = c("passenger car", "light duty vehicle")),
    vehicle_fuel_type = factor(vehicle_fuel_type, levels = c("gasoline", "diesel"))
  ) %>% 
  dplyr::rename(NOx_emission_threshold_g_per_kg_fuel = `nox_emission_threshold_g_per_kg Treibstoff`)

### calculate vehicle specific power and apply data filters for a meaningful analysis
### calculate mean values and corresponding NOx emissions
### ! for a profound analysis, one should also check units, explore site roadgrade, vehicle specific power and air temperature distribution, and general data plausibility and consistency etc in detail
data_temp <- 
  data_rsd %>% 
  dplyr::filter(parameter %in% c("acceleration", "velocity") & !is.na(value)) %>%
  dplyr::select(id, site_roadgrade, parameter, value) %>%
  tidyr::spread(parameter, value) %>% 
  dplyr::mutate(vehicle_specific_power = calc_vsp(velocity * 1000 / 60^2, acceleration * 1000 / 60^2, site_roadgrade)) %>%  # also convert velocity from km/h into m/s and acceleration from km/h/s into m/s2
  dplyr::select(id, acceleration, velocity, vehicle_specific_power) # vehicle_specific_power in kW/t

data_rsd <-
  data_rsd %>%
  dplyr::select(-unit) %>% 
  dplyr::filter(!(parameter %in% c("acceleration", "velocity"))) %>%
  dplyr::left_join(data_temp, by = "id") %>% 
  dplyr::filter(
    vehicle_model_year %in% rsd_filters$vehicleyears &
      (acceleration >= min(rsd_filters$accelerationrange) & acceleration <= max(rsd_filters$accelerationrange)) &
      (velocity >= min(rsd_filters$velocityrange) & velocity <= max(rsd_filters$velocityrange)) &
      (vehicle_specific_power >= min(rsd_filters$vsprange) & vehicle_specific_power <= max(rsd_filters$vsprange)) &
      vehicle_unloaded_weight <= rsd_filters$weightmax &
      !is.na(value)
  ) %>%
  dplyr::mutate(vehicle_euronorm = dplyr::recode(vehicle_euronorm, !!!c("Euro5a" = "Euro5", "Euro5b" = "Euro5"))) %>% # merge both sub-Euro5 norms since they are quite similar
  tidyr::spread(parameter, value) %>%
  dplyr::filter(!is.na(NO + CO2 + CO + HC)) %>%  # all concentrations nessecary for NOx emission calculation
  dplyr::left_join(dplyr::filter(rsd_meta, is.na(as.numeric(vehicle_euronorm))), by = c("vehicle_type", "vehicle_fuel_type", "vehicle_euronorm")) %>% 
  dplyr::mutate(
    vehicle_type = factor(vehicle_type, levels = c("passenger car", "light duty vehicle")),
    vehicle_fuel_type = factor(vehicle_fuel_type, levels = c("gasoline", "diesel")),
    NOx_emission = calc_rsd_nox_emission(NO = NO / 10^4, p = fraction_no2_hbefa, CO2 = CO2, CO = CO, HC = HC / 10^4) # input: concentrations all in percent; originally: NO in ppm, CO2 in %, CO in %, HC in ppm; output: NOx emissions in g/kg fuel;  add HBEFA-derived NO2 and use that for NOx emission calculation rather than measured NO2 since that has only been available since RSD-model 4500
  ) %>% 
  dplyr::filter(!is.na(NOx_emission))


### aggregate RSD NOx emissions per norm, vehicle type and fuel type
data_rsd_per_norm <-
  data_rsd %>% 
  aggregate_groups_rsd(y = "NOx_emission", groups = c("vehicle_type", "vehicle_fuel_type", "vehicle_euronorm"), nmin = rsd_filters$nmin) %>% 
  dplyr::rename(NOx_emission = mean) %>% 
  dplyr::mutate(
    unit = "g/kg fuel",
    source = "Kanton Zürich/AWEL"
  ) %>% 
  dplyr::left_join(dplyr::filter(rsd_meta, is.na(as.numeric(vehicle_euronorm))), by = c("vehicle_euronorm", "vehicle_type", "vehicle_fuel_type")) %>% 
  dplyr::select(vehicle_euronorm, vehicle_type, vehicle_fuel_type, NOx_emission, unit, n, standarderror, NOx_emission_threshold_g_per_kg_fuel, source)


### aggregate RSD NOx emissions per year of vehicle model, vehicle type and fuel type
data_temp <- 
  rsd_meta %>% 
  dplyr::filter(!is.na(as.numeric(vehicle_euronorm))) %>% 
  dplyr::mutate(vehicle_euronorm = as.numeric(vehicle_euronorm)) %>% 
  dplyr::rename(vehicle_model_year = vehicle_euronorm)
  
data_rsd_per_yearmodel <-
  data_rsd %>% 
  aggregate_groups_rsd(y = "NOx_emission", groups = c("vehicle_model_year", "vehicle_type", "vehicle_fuel_type"), nmin = rsd_filters$nmin) %>% 
  dplyr::rename(NOx_emission = mean) %>% 
  dplyr::mutate(
    unit = "g/kg fuel",
    source = "Kanton Zürich/AWEL"
  ) %>% 
  dplyr::left_join(data_temp, by = c("vehicle_model_year", "vehicle_type", "vehicle_fuel_type")) %>% 
  dplyr::select(vehicle_model_year, vehicle_type, vehicle_fuel_type, NOx_emission, unit, n, standarderror, NOx_emission_threshold_g_per_kg_fuel, source)


### aggregate RSD NOx emissions per year of measurement and fuel type
data_rsd_per_yearmeas <-
  data_rsd %>% 
  dplyr::mutate(vehicle_fuel_type = "all") %>% 
  dplyr::bind_rows(data_rsd) %>% 
  dplyr::mutate(year = lubridate::year(date_measured)) %>% 
  aggregate_groups_rsd(y = "NOx_emission", groups = c("year", "vehicle_fuel_type"), nmin = rsd_filters$nmin) %>% 
  dplyr::rename(NOx_emission = mean) %>% 
  dplyr::mutate(
    unit = "g/kg fuel",
    source = "Kanton Zürich/AWEL"
  ) %>% 
  dplyr::select(year, vehicle_fuel_type, NOx_emission, unit, n, standarderror, source)




### write datasets
readr::write_delim(data_rsd_per_norm, file = "data/output/data_nox_emissions_rsd_per_norm.csv", delim = ";", na = "NA")
readr::write_delim(data_rsd_per_yearmodel, file = "data/output/data_nox_emissions_rsd_per_yearmodel.csv", delim = ";", na = "NA")
readr::write_delim(data_rsd_per_yearmeas, file = "data/output/data_nox_emissions_rsd_per_yearmeas.csv", delim = ";", na = "NA")




### -----------------------------------------------
### plot data
### -----------------------------------------------


### plot details of Canton Zürich air pollutant emissions per pollutant, subsector and year (absolute values)
### and plot maps of air pollutant emissions per pollutant, municipality and year
data_temp <- 
  data_emikat %>% 
  dplyr::group_by(year, pollutant, unit, sector, subsector) %>% 
  dplyr::summarise(emission = sum(emission)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(emission > 0)

data_temp2 <- 
  data_emikat %>% 
  dplyr::group_by(municipality, year, pollutant, unit) %>% 
  dplyr::summarise(emission = sum(emission)) %>% 
  dplyr::ungroup()

data_temp2 <-
  boundaries %>% 
  dplyr::filter(art_code == 1) %>% 
  dplyr::rename(municipality = gemeindename) %>% 
  dplyr::right_join(data_temp2, by = "municipality") %>% 
  dplyr::select(year, pollutant, emission)

cols_emissions <- setNames(as.character(cols_emissions), unique(data_temp$sector))

plots$emissions <- 
  setNames(unique(data_temp$pollutant), unique(data_temp$pollutant)) %>% 
  lapply(function(x) {
    
    list(
      
      absolute =
        data_temp %>% 
        dplyr::filter(pollutant == x) %>% 
        ggplot_emissions(cols_emissions, theme_emissions = theme_ts) + 
        ggplot2::ggtitle(
          label = openair::quickText(paste0("Luftschadstoff-Emissionen ", longtitle(x)," (",x,")")),
          subtitle = as.expression(substitute(a * ", Jahresmenge nach Quellgruppen (t " * Jahr^-1 * ")", env = list(a = x)))
          ) +
        ggplot2::labs(caption = "Quelle: OSTLUFT, Grundlage: EMIS Schweiz"), 
      
      relative = 
        data_temp %>% 
        dplyr::filter(pollutant == x) %>% 
        ggplot_emissions(cols_emissions, pos = "fill", width = 0.75, theme_emissions = theme_ts) + 
        ggplot2::scale_y_continuous(labels = scales::percent_format(), expand = c(0.01,0.01)) +
        ggplot2::ggtitle(
          label = openair::quickText(paste0("Luftschadstoff-Emissionen ", longtitle(x)," (",x,")")),
          subtitle = as.expression(substitute(a * ", Jahresmengen-Anteile nach Quellgruppen (relativ)", env = list(a = x)))
        ) +
        ggplot2::labs(caption = "Quelle: OSTLUFT, Grundlage: EMIS Schweiz"), 
      
      map = 
        data_temp2 %>% 
        dplyr::filter(pollutant == x) %>% 
        ggplot2::ggplot(aes(fill = emission)) +
        ggplot2::geom_sf() + 
        ggplot2::coord_sf(datum = sf::st_crs(crs)) +
        ggplot2::facet_wrap(year~.) +
        ggplot2::scale_fill_viridis_c(name = paste0(x,"\nEmission"), option = "A", direction = -1, na.value = NA) +
        theme_map +
        ggplot2::ggtitle(
          label = openair::quickText(paste0("Luftschadstoff-Emissionen ", longtitle(x)," (",x,")")),
          subtitle = as.expression(substitute(a * ", Jahresmenge pro Gemeinde (t " * Jahr^-1 * ")", env = list(a = x)))
        ) +
        ggplot2::labs(caption = "Quelle: OSTLUFT, Grundlage: EMIS Schweiz")
      
      # ... possibly later: maps with emission per inhabitant per municipality? 
    )
    
  })



### plot RSD NOx emissions by vehicle type, fuel type and euronorm
plots$emissions$NOx$rsd_norm <-
  data_rsd_per_norm %>% 
  tidyr::expand(vehicle_type, vehicle_fuel_type, vehicle_euronorm) %>%
  dplyr::left_join(data_rsd_per_norm, by = c("vehicle_type", "vehicle_fuel_type", "vehicle_euronorm")) %>%
  dplyr::mutate(
    vehicle_type = dplyr::recode_factor(vehicle_type, !!!c("passenger car" = "Personenwagen", "light duty vehicle" = "leichte Nutzfahrzeuge")),
    vehicle_fuel_type = dplyr::recode_factor(vehicle_fuel_type, !!!c("gasoline" = "Benzin", "diesel" = "Diesel")),
    vehicle_euronorm = factor(vehicle_euronorm)
  ) %>% 
  ggplot2::ggplot(aes(x = vehicle_euronorm, y = NOx_emission, group = vehicle_type, fill = vehicle_type)) +
  ggplot2::geom_bar(stat = "identity", width = 0.75, position = ggplot2::position_dodge()) +
  ggplot2::geom_linerange(mapping = aes(ymin = NOx_emission - standarderror, ymax = NOx_emission + standarderror), color = "gray60", position = ggplot2::position_dodge(width = 0.75)) +
  ggplot2::geom_segment(mapping = aes(x = as.numeric(vehicle_euronorm) - 0.45, xend = as.numeric(vehicle_euronorm) + 0.45, y = NOx_emission_threshold_g_per_kg_fuel, yend = NOx_emission_threshold_g_per_kg_fuel), color = "red3", linewidth = 1) +
  ggplot2::facet_wrap(vehicle_fuel_type~., strip.position = "bottom") +
  ggplot2::scale_y_continuous(limits = c(0,25), breaks = seq(0,25,5), expand = c(0.01,0.01), labels = function(x) format(x, big.mark = "'")) +
  ggplot2::scale_fill_manual(name = "Fahrzeugkategorie:", values = c("Personenwagen" = "cadetblue3", "leichte Nutzfahrzeuge" = "darkslategray")) +
  # ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1)) +
  ggplot2::ggtitle(
    label = openair::quickText("Abgasmessungen von Stickoxiden (NOx) im realen Fahrbetrieb"),
    subtitle = openair::quickText("NOx Emissionen, Mittelwert pro Abgasnorm (g/kg Treibstoff)")
  ) +
  ggplot2::labs(caption = "Quelle: Kanton Zürich/AWEL") +
  theme_ts +
  ggplot2::theme(
    strip.background = ggplot2::element_blank(),
    # strip.background = ggplot2::element_rect(color = "gray40"),
    strip.placement = "outside",
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom"
  )

### plot RSD NOx emissions by vehicle model year, vehicle type and fuel type
plots$emissions$NOx$rsd_yearmodel <-
  data_rsd_per_yearmodel %>% 
  tidyr::expand(vehicle_type, vehicle_fuel_type, vehicle_model_year) %>% 
  dplyr::left_join(data_rsd_per_yearmodel, by = c("vehicle_type", "vehicle_fuel_type", "vehicle_model_year")) %>% 
  dplyr::mutate(
    vehicle_type = dplyr::recode_factor(vehicle_type, !!!c("passenger car" = "Personenwagen", "light duty vehicle" = "leichte Nutzfahrzeuge")),
    vehicle_fuel_type = dplyr::recode_factor(vehicle_fuel_type, !!!c("gasoline" = "Benzin", "diesel" = "Diesel"))
  ) %>% 
  ggplot2::ggplot(aes(x = vehicle_model_year, y = NOx_emission, group = vehicle_type, fill = vehicle_type)) +
  ggplot2::geom_bar(stat = "identity", width = 0.75, position = ggplot2::position_dodge()) +
  ggplot2::geom_linerange(mapping = aes(ymin = NOx_emission - standarderror, ymax = NOx_emission + standarderror), color = "gray60", position = ggplot2::position_dodge(width = 0.75)) +
  ggplot2::geom_step(mapping = aes(x = vehicle_model_year + 0.475, y = NOx_emission_threshold_g_per_kg_fuel), color = "red3", linewidth = 1) +
  ggplot2::facet_wrap(vehicle_fuel_type~., strip.position = "bottom") +
  ggplot2::scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01), labels = function(x) format(x, big.mark = "'")) +
  ggplot2::scale_fill_manual(name = "Fahrzeugkategorie:", values = c("Personenwagen" = "cadetblue3", "leichte Nutzfahrzeuge" = "darkslategray")) +
  ggplot2::ggtitle(
    label = openair::quickText("Abgasmessungen von Stickoxiden (NOx) im realen Fahrbetrieb"),
    subtitle = openair::quickText("NOx Emissionen, Mittelwert pro Fahrzeug-Modelljahr (g/kg Treibstoff)")
  ) +
  ggplot2::labs(caption = "Quelle: Kanton Zürich/AWEL") +
  theme_ts +
  ggplot2::theme(
    strip.background = ggplot2::element_blank(),
    # strip.background = ggplot2::element_rect(color = "gray40"),
    strip.placement = "outside",
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom"
  )

### plot RSD NOx emission time series (year of measurement) by fuel type
plots$emissions$NOx$rsd_timeseries <-
  data_rsd_per_yearmeas %>% 
  dplyr::mutate(vehicle_fuel_type = dplyr::recode_factor(vehicle_fuel_type, !!!c("all" = "Benzin & Diesel", "gasoline" = "Benzin", "diesel" = "Diesel"))) %>% 
  ggplot2::ggplot(aes(x = year, y = NOx_emission)) +
  ggplot2::geom_smooth(mapping = aes(color = vehicle_fuel_type), span = 0.6, level = 0.95) +
  # ggplot2::geom_point(color = "gray60") +
  ggplot2::scale_x_continuous(expand = c(0.01,0.01)) +
  ggplot2::scale_y_continuous(limits = c(0,NA), expand = c(0.01,0.01), labels = function(x) format(x, big.mark = "'")) +
  ggplot2::scale_color_manual(name = "Treibstoff:", values = c("Benzin & Diesel" = "gray40", "Benzin" = "gold3", "Diesel" = "red3")) +
  ggplot2::ggtitle(
    label = openair::quickText("Abgasmessungen von Stickoxiden (NOx) im realen Fahrbetrieb"),
    subtitle = openair::quickText("NOx Emissionen, Trend der Mittelwerte (g/kg Treibstoff)") # ... "Trend der Mittelwerte Personenwagen und leichte Nutzfahrzeuge", but too long for subtitle
  ) +
  ggplot2::labs(caption = "Quelle: Kanton Zürich/AWEL") +
  theme_ts +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom"
  )


### clean up
rm(list = c("req", "rsd", "rsd_filters", "data_temp", "data_temp2", "data_rsd", "data_rsd_per_norm", "data_rsd_per_yearmodel", "data_rsd_per_yearmeas", "rsd_meta", "emikat", "data_emikat", "cols_emissions"))
