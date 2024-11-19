




### packages
### ----------------------------------------------------------------------
require(rOstluft)
require(dplyr)
require(tidyr)
require(stringr)
require(readr)
require(lubridate)
require(ggplot2)
require(ggiraph)
require(ggh4x)
require(ggbreak)
require(parallel)
require(multidplyr)
require(ragg)
require(cowplot)
require(zoo)






### constants & functions
### ----------------------------------------------------------------------
path_pollumap <- "G:/LKS/03_Monitoring/02_Aufgaben/98_Kleinaufgaben/2022_Standortbestimmung_MaPla_Luft/BevExpo/data/roh/i_pollumap_zh_2021.csv"
path_population <- "G:/LKS/03_Monitoring/02_Aufgaben/98_Kleinaufgaben/2022_Standortbestimmung_MaPla_Luft/BevExpo/data/roh/reli_bfs_ew21.csv"
path_clo <- "G:/LKS/03_Monitoring/02_Aufgaben/98_Kleinaufgaben/2022_Standortbestimmung_MaPla_Luft/BevExpo/data/roh/comb_critical_loads.csv"

# path_to_functions <- "G:/LKS/03_Monitoring/02_Aufgaben/99_Sonstiges/R/helpers"
# source(fs::path(path_to_functions, "functions_parallel.R"), encoding = "utf8")


basesize <- 14 * screen_res_factor
fontsize <- basesize * 0.36
pointsize <- 2 * screen_res_factor
linesize <- screen_res_factor * 0.5
plots <- list()


assign_IGW <- function(parameter) {
  case_when(
    parameter == "PM2.5" ~ 10,
    parameter == "PM10" ~ 20,
    parameter == "NO2" ~ 30,
    parameter == "NO2_nb_d1>80" ~ 1,
    parameter == "PM10_nb_d1>50" ~ 3,
    parameter == "SO2" ~ 30,
    # parameter == "CO" ~ 8,
    parameter == "O3_98%_min30_max_m1" ~ 100,
    parameter == "O3_nb_h1>120" ~ 1
  )
}


assign_WHO <- function(parameter) {
  case_when(
    parameter == "PM2.5" ~ 5,
    parameter == "PM10" ~ 15,
    parameter == "NO2" ~ 10,
    parameter == "NO2_nb_d1>25" ~ 3,
    # parameter == "O3peakseason" ~ 60,
    # parameter == "O38hour>100" ~ 3,
    parameter == "PM2.5_nb_d1>15" ~ 3,
    parameter == "PM10_nb_d1>45" ~ 3
  )
}


assign_EKL <- function(parameter) {
  case_when(
    parameter == "EC" ~ 0.1
  )
}


simplyify_ostluft_siteclass <- function(data) {
  data %>% 
    mutate(
      Siedlung = case_when(
        as.numeric(str_remove(scSiedlungsgroesse, "H")) %in% c(22:23, 31:33) ~ "städtisch",
        # as.numeric(str_remove(scSiedlungsgroesse, "H")) %in% c(21, 31, 11, 12, 13)  & as.numeric(str_sub(scSiedlungsgroesse, 2, 3)) > 10 ~ "kleinstädtisch",
        as.numeric(str_remove(scSiedlungsgroesse, "H")) == 0 ~ "ländlich",
        TRUE ~ "n.b."
      ),
      Verkehr  = case_when(
        as.numeric(str_remove(scVerkehrslage, "S")) %in% c(22:23, 32:33) ~ "verkehrsbelastet",
        # as.numeric(str_remove(scVerkehrslage, "S")) < 30 & as.numeric(str_sub(scVerkehrslage, 2, 3)) > 20 ~ "verkehrsbelastet",
        as.numeric(str_remove(scVerkehrslage, "S")) == 0 ~ "Hintergrund",
        TRUE ~ "n.b."
      ),
      Standortklasse = ifelse(is.na(Standortklasse) & Siedlung != "n.b." & Verkehr != "n.b.", paste(Siedlung, Verkehr, sep = " / "), Standortklasse)
    ) %>% 
    dplyr::select(-Siedlung, -Verkehr) 
}


assign_nabel_siteclass <- function(data) {
  nabel_siteclass <- c("Dübendorf" = "vorstädtisch / Hintergrund", "Lägeren" = "ländlich / Hintergrund", "Tänikon" = "ländlich / Hintergrund", "Payerne" = "ländlich / Hintergrund", 
                       "Rigi-Seebodenalp" = "ländlich / Hintergrund", "Zürich-Kaserne" = "städtisch / Hintergrund", "Härkingen-A1" = "ländlich / verkehrsbelastet",
                       "Basel-Binningen" = "vorstädtisch / Hintergrund", "Bern-Bollwerk" = "städtisch / verkehrsbelastet", "Beromünster" = "ländlich / Hintergrund")
  data %>% 
    mutate(
      Standortklasse = dplyr::recode(site, !!!nabel_siteclass, .default = ""),
      Standortklasse = ifelse(Standortklasse == "", NA, Standortklasse)
    )
}


melt_siteclass <- function(data) {
  data %>% 
    mutate(
      Standortklasse = ifelse(Standortklasse %in% c("vorstädtisch / Hintergrund", "kleinstädtisch / Hintergrund", "ländlich / verkehrsbelastet", "vorstädtisch / verkehrsbelastet", "kleinstädtisch / verkehrsbelastet"), NA, Standortklasse),
      Standortklasse = factor(Standortklasse, levels = c("ländlich / Hintergrund", "städtisch / Hintergrund", "städtisch / verkehrsbelastet"))	
    )
}


theme_ts <- 
  theme_classic(base_size = basesize) + 
  theme(
    # legend.position = "top",
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "gray60"),
    axis.line = element_line(color = "gray60"),
    strip.background = element_blank(),
    axis.text = element_text(color = "gray30"),
    strip.text = element_text(hjust = 0, size = basesize, color = "gray30"), 
    axis.title = element_blank()
  )


theme_ts2 <- 
  theme_minimal(base_size = basesize) + 
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks = element_line(color = "gray60"),
    strip.background = element_blank(),
    axis.text = element_text(color = "gray30"),
    strip.text = element_text(hjust = 0, size = basesize, color = "gray30"), 
    axis.title = element_blank(),
  )





### initialize rOstluft datastore(s)
### ----------------------------------------------------------------------
aqmet <- store_aqmet() # make sure rOstluft aqmet store is registered and credentials are loaded in Rprofile 



### initialize evaluation parameters
### ----------------------------------------------------------------------
par <- c("NO2", "PM10", "PM2.5", "EC", "O3_98%_min30_max_m1", "O3_nb_h1>120", "O3_max_h1", "NO2_nb_d1>80", "PM10_nb_d1>50", "PM10_max_d1", "NO2_max_d1") # , "PM2.5_max_d1")

par_plot <- c("PM10" = "Feinstaub PM10 Jahresmittel\n(PM10, µg/m3)", "PM10_nb_d1>50" = "Feinstaub PM10 Tagesmittel > 50 µg/m3\n(PM10, Anzahl)", "PM2.5" = "Feinstaub PM2.5 Jahresmittel\n(PM2.5, µg/m3)", 
              "EC" = "Russ Jahresmittel*\n(eBC, µg/m3)", "NO2" = "Stickstoffdioxid Jahresmittel\n(NO2, µg/m3)", "NO2_nb_d1>80" = "Stickstoffdioxid Tagesmittel > 80 µg/m3\n(NO2, Anzahl)",
              "O3_98%_min30_max_m1" = "Ozon max. Monats-98%-Perzentil\n(O3, µg/m3)", "O3_nb_h1>120" = "Ozon Stundenmittel > 120 µg/m3\n(O3, Anzahl)", "O3_max_h1" = "Ozon max. Stundenmittel\n(O3, µg/m3)",
              "PM10_max_d1" = "PM10 max. Tagesmittel\n(PM10, µg/m3)", "NO2_max_d1" = "Stickstoffdioxid max. Tagesmittel\n(NO2, µg/m3)")# , "PM2.5_max_d1" = "PM2.5 max. Tagesmittel\n(PM2.5, µg/m3)")

extrasites_background <- c("Tänikon", "Payerne", "StG_Stuelegg") # , "Rigi-Seebodenalp"
extrasites_pm <- c("Tänikon", "Payerne", "Bern-Bollwerk", "StG_Stuelegg") # , "Rigi-Seebodenalp"
# data %>% dplyr::filter(parameter == "NO2") %>% dplyr::arrange(Standortklasse, site, starttime) %>% View

cols <- setNames(viridis::viridis(3, end = 0.88, direction = -1), c("ländlich / Hintergrund", "städtisch / Hintergrund", "städtisch / verkehrsbelastet"))
# cols <- setNames(c("steelblue", "goldenrod3", "red3"), c("ländlich / Hintergrund", "städtisch / Hintergrund", "städtisch / verkehrsbelastet"))

specific_sites <- expand_grid(parameter = par, Standortklasse = names(cols), site = NA)
specific_sites[specific_sites$parameter == "PM10",]$site <- c("Tänikon", "Zürich-Kaserne", "Zch_Schimmelstrasse")
specific_sites[specific_sites$parameter == "PM10_nb_d1>50",]$site <- c("Tänikon", "Zürich-Kaserne", "Zch_Schimmelstrasse")
specific_sites[specific_sites$parameter == "PM10_max_d1",]$site <- c("Tänikon", "Zürich-Kaserne", "Zch_Schimmelstrasse")
specific_sites[specific_sites$parameter == "PM2.5",]$site <- c("Payerne", "Zürich-Kaserne", "Bern-Bollwerk")
# specific_sites[specific_sites$parameter == "PM2.5_max_d1",]$site <- c("Tänikon", "Zürich-Kaserne", "Zch_Schimmelstrasse")
specific_sites[specific_sites$parameter == "EC",]$site <- c("Payerne", "Win_Veltheim", "Zch_Schimmelstrasse")
specific_sites[specific_sites$parameter == "NO2",]$site <- c("Tänikon", "Zch_Stampfenbachstrasse", "8400_0040") # 8400_0040 = "Winterthur_Technikumstrasse 79"
specific_sites[specific_sites$parameter == "NO2_nb_d1>80",]$site <- c("Tänikon", "Zch_Stampfenbachstrasse", "Zch_Schimmelstrasse")
specific_sites[specific_sites$parameter == "NO2_max_d1",]$site <- c("Tänikon", "Zch_Stampfenbachstrasse", "Zch_Schimmelstrasse")
specific_sites[specific_sites$parameter == "O3_98%_min30_max_m1",]$site <- c("Tänikon", "Zch_Heubeeribüel", "Zch_Schimmelstrasse")
specific_sites[specific_sites$parameter == "O3_nb_h1>120",]$site <- c("Tänikon", "Zch_Heubeeribüel", "Zch_Schimmelstrasse")
specific_sites[specific_sites$parameter == "O3_max_h1",]$site <- c("Tänikon", "Zch_Heubeeribüel", "Zch_Schimmelstrasse")
# specific_sites[specific_sites$parameter == "O3_98%_min30_max_m1",]$site <- c("Bachtel/Wald", "Zch_Heubeeribüel", "Zch_Schimmelstrasse")
# specific_sites[specific_sites$parameter == "O3_nb_h1>120",]$site <- c("Bachtel/Wald", "Zch_Heubeeribüel", "Zch_Schimmelstrasse")
# specific_sites[specific_sites$parameter == "O3_max_h1",]$site <- c("Bachtel/Wald", "Zch_Heubeeribüel", "Zch_Schimmelstrasse")


exclude_parameter <- NA
# exclude_parameter <- c("PM10", "NO2_nb_d1>80", "PM10_nb_d1>50", "O3_nb_h1>120", "O3_98%_min30_max_m1", "NO2_nb_d1>25", "PM2.5_nb_d1>15", "PM10_nb_d1>45")
exclude_sites_NO2 <- c(
                       "Bern-Bollwerk", "Payerne", "StG_Stuelegg", "Wet_Filderen", "8913_0040", "8910_0010", "8164_0010", "8910_0030", "8910_0030", "8907_0060", "8907_0050", "8903_0010", "Win_St.Gallerstrasse", "8302_0010", "Zch_Heubeeribüel", "8400_0360", "8400_0350", "8000_0790", 
                       "8000_1490", "8000_1770", # Wasserwerkstrasse & Manessestrasse => nicht ausreichend Metadaten für eine Langzeitmessreihendarstellung; evtl. nachführen lassen !
                       "8000_0010" # Kaserne PS = redundant
                       )
exclude_sites_EC <- c("Bern-Bollwerk")
exclude_sites_PM10 <- c("Bern-Bollwerk", "Payerne", "StG_Stuelegg")
exclude_sites_O3 <- c("Bern-Bollwerk", "Payerne")


grenzwerte <- 
  tibble(
    parameter = c(par, par, par), Referenzwert = c(assign_IGW(par), assign_WHO(par), assign_EKL(par)), 
    Referenz = c(rep("LRV-Grenzwert", length(par)), rep("WHO-Empfehlung 2021", length(par)), rep("EKL-Zielwert", length(par)))
  ) %>% 
  dplyr::filter(!is.na(Referenzwert) & !(parameter %in% exclude_parameter)) 

clo_bachtel <- list(min = 5, mid = 10, max = 20) # CLO range für Mischwald gemäss FUB = 5 - 20 kgN/ha/a, Einzelwert gemäss Naturschutzfachstelle = 10 

par <- if ("EC" %in% par) {par <- unique(c(par, "EC2.5", "EC10", "eBC2.5"))}

years <- (year(Sys.Date()) - year_period):(year(Sys.Date()) - 1)
# years <- 2018:2022







