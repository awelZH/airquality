
# ... to be roughly in line with https://www.bafu.admin.ch/bafu/de/home/themen/luft/publikationen-studien/publikationen/immissionsmessung-von-luftfremdstoffen.html
# however, the OSTLUFT site classes are - as categories - not entirely consistent with the new Immissionsmessempfehlung. We will need to put future effort in a reclassifiacation
recode_ostluft_meta_zone <- function(zone) { 
  
  zone <- 
    dplyr::case_when(
      as.numeric(stringr::str_remove(zone, "H")) %in% c(21:23, 31:33) ~ "städtisch", # OSTLUFT: > 20'000 Gesamteinwohner; BAFU: > 1500 Einwohner/km2 und Gesamteinwohnerzahl > 50 000
      as.numeric(stringr::str_remove(zone, "H")) %in% 11:13 ~ "klein-/vorstädtisch", # OSTLUFT: > 1'000 Gesamteinwohner; BAFU: > 300 Einwohner/km2 im \u00fcberbauten Gebiet und Gesamteinwohnerzahl > 5000
      as.numeric(stringr::str_remove(zone, "H")) == 0 ~ "ländlich", # OSTLUFT: < 1'000 Gesamteinwohner; BAFU: Gebiete mit geringer Siedlungsdichte (< 300 Einwohner/km2) oder kleinere Ortschaften (< 5000 Einwohner)
      TRUE ~ zone 
    )
  
  return(zone)
}



# ... to be roughly in line with https://www.bafu.admin.ch/bafu/de/home/themen/luft/publikationen-studien/publikationen/immissionsmessung-von-luftfremdstoffen.html
# however, the OSTLUFT site classes are - as categories - not entirely consistent with the new Immissionsmessempfehlung. We will need to put future effort in a reclassifiacation
recode_ostluft_meta_type <- function(type) { 
  
  type <- 
    dplyr::case_when(
      as.numeric(stringr::str_remove(type, "S")) %in% c(10:13, 20:23, 30:33) ~ "verkehrsbelastet", # OSTLUFT: DTV_S > 10'000; BAFU: has a finer scale that begins at DTV > 3'000 and cerctain max distance to street 
      as.numeric(stringr::str_remove(type, "S")) == 0 ~ "Hintergrund", # OSTLUFT: DTV_S < 10'000 & street more than 50m (in cities) or 300m (outside of cities) away; BAFU: see above
      TRUE ~ type 
    )
  
  return(type)
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