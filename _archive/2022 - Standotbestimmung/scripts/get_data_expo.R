
### read datasets
data_p <- readr::read_delim(path_pollumap, delim = ";", locale = locale(encoding = "latin1"))
data_pop <- readr::read_delim(path_population, delim = ";", locale = locale(encoding = "latin1"))
data_clo <- readr::read_delim(path_clo, delim = ";", locale = locale(encoding = "latin1"))

### aggregate to 1 ha, bzw.Raster Bevölkerung per parallel computing, see https://multidplyr.tidyverse.org/articles/multidplyr.html
cluster <- new_cluster(parallel::detectCores() - 1)
cluster_library(cluster, c("dplyr", "tidyr"))

data_p <-
  data_p %>% 
  dplyr::select(RELI, no2_21_mod, pm25_21_mod, pm10_21_mod, bc_21_mod) %>% # select variables to summarise; "*._mod" = an Messwerten Kalibrierte Karten
  gather(parameter, value, -RELI) %>% 
  dplyr::filter(value != 0) %>%
  group_by(parameter) %>%
  partition(cluster)

t <- Sys.time()
data_p <-
  data_p %>% 
  group_by(RELI, parameter) %>% 
  dplyr::summarise(
    min = min(value, na.rm = TRUE), 
    q25 = quantile(value, 0.25),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    q75 = quantile(value, 0.75),
    max = max(value, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  collect()
Sys.time() - t
# str(data_p)

data_pop <- 
  data_pop %>%  
  filter(KT == 1 & !is.na(R21BTOT)) %>% # nur Rasterzellen innerhalb Kanton ZH mit vorhandener Bevölkerung
  select(RELI, GEM, R21BTOT) 
# str(data_pop)

### merge and filter
data_expo <- full_join(data_p, data_pop, by = "RELI") 
str(data_expo)

### clean up workspace
rm(list = c("data_p", "data_pop"))

### auch noch für Ökosysteme
data_clo <- 
  data_clo %>% 
  dplyr::select(fm, hm, tw, lfi) %>% 
  dplyr::rename(
    Flachmoore = fm, 
    Hochmoore = hm, 
    `Trockenwiesen/-weiden` = tw,
    Wald = lfi
  ) %>% 
  gather(ecosystem, ndep) %>% 
  dplyr::filter(!is.na(ndep) & ndep > -3*10^38)

### save analysis-ready data as *.rds
saveRDS(data_expo, "data/data_expo.rds")
saveRDS(data_clo, "data/data_clo.rds")





