library(rOstluft)
library(dplyr)
library(ggplot2)
library(lubridate)

### get selected OSTLUFT & NABEL data from aqmet & harmonise eBC naming & assign coordinates
#pars <- c("PM2.5", "PM10", "EC1.0", "EC2.5", "EC10", "EC", "eBC", "eBC1.0", "eBC2.5", "eBC10")
# pars <- c("NO2", "PM2.5", "PM10", "EC1.0", "EC2.5", "EC10", "EC", "eBC", "eBC1.0", "eBC2.5", "eBC10")
pars <- c("NO2")
yr <- 2022
#yr <- c("2020", "2021")

aq <- rOstluft::store_aqmet()

meta <- aq$get_meta()
meta <- bind_rows(meta$nabel, meta$ostluft)
meta <- 
  meta %>% 
  dplyr::filter(mspbJahr == yr & parameter %in% pars & !is.na(x)) %>% 
  mutate(
    parameter = dplyr::recode_factor(parameter, `EC` = "eBC"),
    site = dplyr::recode(site, `Dübendorf` = "Dübendorf-Empa")
    ) %>%
  dplyr::distinct(site, .keep_all = TRUE) %>% 
  dplyr::select(site, x, y)

content <- 
  aq$get_content() %>% 
  dplyr::filter(year == yr & interval == "y1" & parameter %in% pars)

data <- aq$get(year = yr, site = unique(as.character(content$site)), interval = "y1", filter = parameter %in% pars)
data <- 
  data %>% 
  mutate(parameter = dplyr::recode_factor(parameter, `EC1.0` = "eBC", `EC2.5` = "eBC", `EC10` = "eBC", `eBC2.5` = "eBC")) %>% 
  left_join(meta, by = "site") %>% 
  dplyr::filter(!(site %in% c("Bern-Bollwerk", "Payerne", "Rigi-Seebodenalp", "Beromünster", "Win_Neuhegi"))) %>% 
  mutate_if(is.character, factor)

dplyr::filter(data, parameter == "NO2")
dplyr::filter(data, parameter == "PM10")
dplyr::filter(data, parameter == "PM2.5")
dplyr::filter(data, parameter == "eBC")

# Export Data as *.csv
# write.table(data, file = "aqmet_20_all.csv", sep = ";", dec = ".", row.names = FALSE, col.names = TRUE)

data %>% 
  filter(parameter != "NO2") %>% 
  ggplot(aes(x = site, y = value)) +
  geom_bar(stat = "identity") +
  facet_wrap(parameter~., scales = "free_x") +
  coord_flip()

