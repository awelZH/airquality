


content <- aqmet$get_content()
meta <- 
  aqmet$get_meta() %>% 
  bind_rows()
# content %>% dplyr::filter(interval == "y1") %>% pull(site) %>% as.character %>% unique %>% sort()
# content %>% dplyr::filter(interval == "y1") %>% pull(parameter) %>% as.character %>% unique %>% sort()
# meta %>% pull(site) %>% as.character %>% unique %>% sort()
# meta %>% pull(parameter) %>% as.character %>% unique %>% sort()
# ps <- content %>% dplyr::filter(parameter == "NO2" & !is.na(as.numeric(stringr::str_remove(site, "_"))) & year > 2016) %>% pull(site) %>% as.character() %>% unique() %>% sort()
# saveRDS(ps, "/home/rstudio/rscripts/temp/NO2_PS.rds")





# harmonize (crappy) metadata / siteclass
# ----------------------------------------------------------------------
# meta$nabel %>% pull(site) %>% as.character() %>% unique() %>% sort()
meta <-
  meta %>% 
  dplyr::rename(year = mspbJahr) %>% 
  dplyr::filter(parameter %in% par & !is.na(year) & mspbPublic & !(is.na(scVerkehrslage) & is.na(msNABEL)) & (msKT == "ZH" | site %in% unique(extrasites_pm, specific_sites$site))) %>%
  mutate(site2 = case_when(!is.na(msOrt) ~ paste0(msOrt, "_", msOrtsteil), TRUE ~ site)) %>% 
  dplyr::distinct(site, parameter, year, scVerkehrslage, .keep_all = TRUE)

meta <-
  expand_grid(site = sort(unique(c(meta$site, extrasites_pm, specific_sites$site))), parameter = unique(meta$parameter), year = sort(unique(meta$year))) %>% 
  left_join(meta, by = c("site", "parameter", "year")) %>%
  mutate(Standortklasse = NA) %>%
  assign_nabel_siteclass() %>%
  simplyify_ostluft_siteclass() %>%
  melt_siteclass() %>%
  dplyr::filter(!is.na(Standortklasse)) %>% 
  dplyr::select(site, site2, parameter, year, Standortklasse) 

sites <- 
  meta %>%
  pull(site) %>% 
  as.character() %>% 
  unique() %>% 
  sort()

targets <- 
  content %>% 
  mutate(
    filtercombi = (parameter %in% par & interval == "y1" & site %in% sites),
    filtercombi2 = ((site %in% extrasites_pm) & parameter == "PM2.5" & interval == "y1") | ((site %in% extrasites_background & parameter %in% par) & interval == "y1")
  ) %>% 
  dplyr::filter(filtercombi | filtercombi2) %>%
  mutate_if(is.factor, as.character) %>% 
  dplyr::distinct(year, site, parameter, .keep_all = TRUE) %>% 
  group_split(year, parameter)

# sapply(targets, function(x) x$parameter)
# targets[[248]]






### get target data, pad series and merge with siteclass and aggregate per year / parameter / siteclass and do stuff...
### ----------------------------------------------------------------------
### get data
# aqmet$get(year = 2021, site = "8400_0040", interval = "y1", filter = parameter == "NO2")
data <- lapply(targets, function(x) {aqmet$get(year = x$year, site = x$site, interval = "y1", filter = parameter == unique(as.character(x$parameter)))})
data <- bind_rows(data)
data <- mutate(data, parameter = dplyr::recode_factor(parameter, !!!c("ECx" = "EC", "EC10" = "EC", "EC2.5" = "EC", "eBC2.5" = "EC"))) # !

# data %>%  dplyr::filter(Standortklasse == "ländlich / Hintergrund" & parameter == "NO2") %>% View
# data %>%  dplyr::filter(Standortklasse == "städtisch / verkehrsbelastet" & parameter == "NO2") %>% View
# data %>%  dplyr::filter(parameter == "O3_98%_min30_max_m1")


### exclude ambiguous sites & wrong units
data <- dplyr::filter(data, !(site %in% exclude_sites_NO2 & parameter %in% c("NO2", "NO2_nb_d1>80", "NO2_max_d1")) & !(site == "8000_0090" & parameter == "NO2" & year(starttime) == 2009))
data <- dplyr::filter(data, !(site %in% exclude_sites_EC & parameter == "EC"))
data <- dplyr::filter(data, !(site %in% exclude_sites_PM10 & parameter %in% c("PM10", "PM10_nb_d1>50", "PM10_max_d1")))
data <- dplyr::filter(data, !(site %in% exclude_sites_O3 & parameter %in% c("O3_98%_min30_max_m1", "O3_nb_h1>120", "O3_max_h1")))
data <- dplyr::filter(data, !(parameter == "NO2" & unit == "ppb"))


### exclude parameters for not plotting them
data <- dplyr::filter(data, !(parameter %in% exclude_parameter))


### ! manually add JMW 2020 for Zch_Schimmelstrasse (incomplete timeseries!) => annotate that when using the plot!)
data <- 
  rOstluft::read_airmo_csv("data/temp.csv") %>% 
  bind_rows(data)

### ! manually add JMW 2022 for eBC Zürich-Kaserne (incomplete timeseries!) => eBC sowieso nur Stichproben)
data <-
  aqmet$get(site = "Zürich-Kaserne", year = 2022, interval = "min10", filter = parameter == "eBC2.5") %>% 
  resample(new_interval = "y1") %>% 
  bind_rows(data)
  

### merge data with metadata & expand passive sampler sitenames
data <- 
  data %>% 
  pad() %>% 
  mutate(year = lubridate::year(starttime))

data <-
  meta %>% 
  dplyr::select(site, site2, Standortklasse, year) %>%
  right_join(data, by = c("site", "year")) %>% 
  dplyr::distinct(site, Standortklasse, parameter, year, .keep_all = TRUE) %>%
  dplyr::select(-year) %>% 
  dplyr::filter(!is.na(Standortklasse)) %>% 
  mutate_if(is.character, factor)
# str(data)

### rename Winterthur Veltheim / Obertor and Bachtel - Turm / Wald - Höhenklinik in order to merge both timeseries
data <-
  data %>% 
  mutate(
    site = dplyr::recode_factor(site, !!!c("Win_Obertor" = "Win_Veltheim", "Bac_Turm" = "Wld_Höhenklinik")),
    site2 = dplyr::recode_factor(site2, !!!c("Winterthur_Obertor" = "Winterthur_Veltheim/Obertor", "Winterthur_Veltheim" = "Winterthur_Veltheim/Obertor", 
                                             "Hinwil_Bachtel Turm" = "Bachtel/Wald", "Wald_Höhenklinik" = "Bachtel/Wald"))
  )

### check Winterthur Technikumstr.
# data %>% 
#   dplyr::filter(site == "8400_0040") %>% tail

### aggregate stats per Standortklasse / year / parameter
data_agg <- 
  data %>% 
  group_by(starttime, Standortklasse, parameter) %>% 
  dplyr::summarise(
    mean = round(mean(value, na.rm = TRUE), 2),
    sd = round(sd(value, na.rm = TRUE), 2),
    median = round(median(value, na.rm = TRUE), 2),
    q02 = round(quantile(value, 0.02, na.rm = TRUE), 2),
    q98 = round(quantile(value, 0.98, na.rm = TRUE), 2),
    min = round(min(value, na.rm = TRUE), 2),
    max = round(max(value, na.rm = TRUE), 2),
    n = length(!is.na(value))
  ) %>% 
  ungroup() %>% 
  mutate(
    min = ifelse(is.infinite(min), NA, min),
    max = ifelse(is.infinite(max), NA, max),
    Jahr = year(starttime)
  )




### save analysis-ready data as *.rds
saveRDS(data, "data/data.rds")
saveRDS(data_agg, "data/data_agg.rds")



