# eBC faktor 10?


# get links to data sources
source("scripts/__ressources.R", encoding = "UTF-8") #FIXME: see issue 11

# setup analysis: load libraries & functions & read map boundaries data
source("scripts/_setup.R", encoding = "UTF-8")

# read, restructure, analyse & save air pollutant emission data 
source("scripts/_emission_data.R", encoding = "UTF-8")^
  
# read, restructure & save air quality monitoring data
source("scripts/_monitoring_data.R", encoding = "UTF-8")

# read, restructure & analyse air quality and inhabitant raster data, derive population and ecosystem exposition & save data 
source("scripts/_exposition_data.R", encoding = "UTF-8")
