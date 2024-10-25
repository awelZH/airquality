# ==> not used?
# FIXME: do wee need read_helpers?


# read_statpop_csv <- function(file, year, crs = 2056) {
#   
#   var <- paste0("B", stringr::str_sub(year, 3, 4), "BTOT")
#   delim <- ifelse(as.numeric(year) > 2015, ";", ",")
#   data <- 
#     file %>% 
#     readr::read_delim(delim = delim, locale = readr::locale(encoding = "UTF-8")) %>% 
#     dplyr::select(RELI, E_KOORD, N_KOORD, !!var) %>% 
#     dplyr::rename(
#       x = E_KOORD,
#       y = N_KOORD,
#       population = !!var
#     ) %>% 
#     # dplyr::mutate(year = as.numeric(year)) %>% 
#     # sf::st_as_sf(coords = c("x", "y", "year"), dim = "XYZ") %>% 
#     sf::st_as_sf(coords = c("x", "y"), dim = "XY") %>%
#     sf::st_set_crs(value = crs) %>% 
#     stars::st_rasterize() 
#   
#   return(data)
# }

