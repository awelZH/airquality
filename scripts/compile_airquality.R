
# setup analysis: load libraries & functions & read ressource table + map boundaries data
source("scripts/_setup.R", encoding = "UTF-8")

# read, restructure, analyse & save air pollutant emission data 
source("scripts/_gather_emission_data.R", encoding = "UTF-8")
  
# read, restructure & save air quality monitoring data
source("scripts/_gather_monitoring_data.R", encoding = "UTF-8")

# read, restructure & analyse air quality and inhabitant raster data, derive population and ecosystem exposition & save data 
source("scripts/_gather_exposition_data.R", encoding = "UTF-8")
