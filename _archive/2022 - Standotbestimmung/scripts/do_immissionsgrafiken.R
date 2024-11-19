### Immissions-Grafiken Kanton Zürich
### beachte: PM10 JMW & TMW > 50 Zch_Schimmelstrasse 2020: unvollständiges JMW, daher Wert aus ostluft.ch *.csv manuell eingefügt, bei Verwendung der Grafik erläutern!)

screen_res_factor <- 2.6
read_data_from_rds <- TRUE
# screen_res_factor <- 1
# read_data_from_rds <- FALSE

year_min <- 2000
year_period <- 4



### ----------------------------------------------------------------------------
### load packages, functions & constants for data analysis and display
### ----------------------------------------------------------------------------
source("scripts/initialise.R", encoding = "UTF-8")



### ----------------------------------------------------------------------------
### get data from rOstluft and do data-wrangling or read pre-compiled dataset
### ----------------------------------------------------------------------------
if (!read_data_from_rds) {
  
  ### get data from rOL
  source("scripts/get_data.R", encoding = "UTF-8")
  source("scripts/get_WHO.R", encoding = "UTF-8")
  ### get data for exposition
  source("scripts/get_data_expo.R", encoding = "UTF-8")
  
} else {
  
  ### read pre-compiled analysis-ready datasets from *.rds
  data <- readRDS("data/data.rds")
  data_agg <- readRDS("data/data_agg.rds")
  data_WHO <- readRDS("data/data_WHO.rds")
  data_expo <- readRDS("data/data_expo.rds")
  data_clo <- readRDS("data/data_clo.rds")
  
}

# data[data$site2 == "Winterthur_Technikumstrasse 79" & data$parameter == "NO2" & year(data$starttime) == 2019,]




### ----------------------------------------------------------------------------
### create various plots
### ----------------------------------------------------------------------------
source("scripts/plotting.R", encoding = "UTF-8")


results_expo <-
  d %>% 
  left_join(refs, by = "parameter") %>%
  dplyr::filter(value >= threshold) %>% 
  group_by(parameter, stat, Referenz) %>% 
  dplyr::summarise(R21BTOT = round((max(R21BTOT) - min(R21BTOT)) / 100) * 100) %>% 
  ungroup() %>% 
  dplyr::arrange(Referenz, parameter, stat) %>% 
  mutate(fraction = round(R21BTOT / max(d$R21BTOT) * 100, 1))
# results_expo






### ----------------------------------------------------------------------------
### save plots
### ----------------------------------------------------------------------------

### Belastung relativ LRV
ragg::agg_png("plots/Belastung_relativ_LRV.png", width = 12, height = 6, units = "in", res = 300, scaling = 1 / screen_res_factor)
plots$belastung_relativ_LRV
invisible(dev.off())

### Belastung relativ WHO
ragg::agg_png("plots/Belastung_relativ_WHO.png", width = 12, height = 6, units = "in", res = 300, scaling = 1 / screen_res_factor)
plots$belastung_relativ_WHO
invisible(dev.off())

# ### Langzeitbelastung relativ LRV
# ragg::agg_png("plots/Langzeitbelastung_relativ_LRV.png", width = 8, height = 5, units = "in", res = 300, scaling = 1 / screen_res_factor)
# plots$langzeitbelastung_relativ_LRV
# invisible(dev.off())
# 
# ### Langzeitbelastung relativ WHO
# ragg::agg_png("plots/Langzeitbelastung_relativ_WHO.png", width = 7, height = 5, units = "in", res = 300, scaling = 1 / screen_res_factor)
# plots$langzeitbelastung_relativ_WHO
# invisible(dev.off())
# 
### Kurzzeitbelastung relativ LRV
# ragg::agg_png("plots/Kurzzeitbelastung_relativ1.png", width = 4, height = 5, units = "in", res = 300, scaling = 1 / screen_res_factor)
# plots$kurzzeitbelastung_relativ1 + theme(legend.position = "none")
# invisible(dev.off())
# 
# ragg::agg_png("plots/Kurzzeitbelastung_relativ2.png", width = 2.5, height = 5, units = "in", res = 300, scaling = 1 / screen_res_factor)
# plots$kurzzeitbelastung_relativ2 + theme(legend.position = "none")
# invisible(dev.off())
# 
# ragg::agg_png("plots/Kurzzeitbelastung_alternativ_LRV.png", width = 6.25, height = 5.1, units = "in", res = 300, scaling = 1 / screen_res_factor)
# plots$kurzzeitbelastung_relativ_alternativ_LRV 
# invisible(dev.off())

### Kurzzeitbelastung relativ WHO
# ...

### PM10
ragg::agg_png("plots/PM10.png", width = 12, height = 4, units = "in", res = 300, scaling = 1 / screen_res_factor)
plots$PM10
invisible(dev.off())

### PM2.5
ragg::agg_png("plots/PM25.png", width = 7, height = 4, units = "in", res = 300, scaling = 1 / screen_res_factor)
plots$PM2.5
invisible(dev.off())

### eBC
ragg::agg_png("plots/eBC.png", width = 7, height = 4, units = "in", res = 300, scaling = 1 / screen_res_factor)
plots$eBC
invisible(dev.off())

### NO2
ragg::agg_png("plots/NO2.png", width = 12, height = 4, units = "in", res = 300, scaling = 1 / screen_res_factor)
plots$NO2
invisible(dev.off())

### O3
ragg::agg_png("plots/O3.png", width = 12, height = 4, units = "in", res = 300, scaling = 1 / screen_res_factor)
plots$O3
invisible(dev.off())

### N-Einträge Langzeitmessreihe Bachtel / Mischwald
ragg::agg_png("plots/N-Eintrag_Bachtel_Mischwald.png", width = 8, height = 4, units = "in", res = 300, scaling = 1 / screen_res_factor)
plots$bachtel
invisible(dev.off())

### ... N-Einträge pro Standort MK OL: siehe verlinkte Grafik

### Exposition PM10, PM2.5, NO2
ragg::agg_png("plots/exposition.png", width = 7, height = 4, units = "in", res = 300, scaling = 1 / screen_res_factor)
plots$expo_clo
invisible(dev.off())

### Exposition N-Einträge > CLO
ragg::agg_png("plots/exposition_clo.png", width = 7, height = 3.5, units = "in", res = 300, scaling = 1 / screen_res_factor)
plots$expo
invisible(dev.off())









### Datenauszug für UB22: 
plots$NO2$data %>%
  dplyr::filter(Messgrösse == "Stickstoffdioxid Jahresmittel\n(NO2, µg/m3)") %>%
  bind_rows(plots$PM2.5$data) %>%
  bind_rows(plots$eBC$data) %>%
  dplyr::select(-site, -Messzeitraum) %>%
  mutate(Messgrösse = stringr::str_replace(Messgrösse, "\n", " ")) %>% 
  dplyr::filter(!is.na(Messwert)) %>%
  write.table("data_UB22_Luft.csv", sep = ";", quote = FALSE, row.names = FALSE)


plots$expo_clo$data %>% 
  dplyr::rename(
    "empfindl. Ökosystem" = ecosystem,
    "Überschreitung Critical Load" = ndep,
    "Anteil an Ökosystem-Gesamtfläche" = fraction
  ) %>% 
  write.table("data_UB22_CLO_KtZH.csv", sep = ";", quote = FALSE, row.names = FALSE)



