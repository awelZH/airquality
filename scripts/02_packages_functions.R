### nessecary for function functionality:

# FIXME:
# for (package in c('<package1>', '<package2>')) {
#   if (!require(package, character.only=T, quietly=T)) {
#     install.packages(package)
#     library(package, character.only=T)
#   }
# } 

library(tibble)
library(tidyr)
library(dplyr)
library(purrr)
library(stringr)
library(rlang)
library(rjson)
library(lubridate)
library(RCurl) # will later be used in read_bfs_statpop_data()
library(readr)
library(archive)
library(sf)
library(stars)
library(ows4R)
library(ggplot2)

### on top: required for script functionality

library(devtools)
library(httr2)
library(fs)
library(ggh4x)
library(scales)
library(lemon)
library(openair)
library(shades)
library(wesanderson)
library(rOstluft.plot) # https://github.com/Ostluft/rOstluft.plot

### load functions 

devtools::load_all()

