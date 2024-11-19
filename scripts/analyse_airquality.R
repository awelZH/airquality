
# setup analysis: load libraries & functions & read ressource table + map boundaries data
source("scripts/_setup.R", encoding = "UTF-8")

# compile O3 peak-season data => do this offline, since some input files are strictly-speaking not open data 
# source("scripts/_compile_o3_peakseason.R", encoding = "UTF-8")

# read, restructure, analyse & save air pollutant emission data 
source("scripts/_compile_emission_data.R", encoding = "UTF-8")
  
# read, restructure & save air quality monitoring data
source("scripts/_compile_monitoring_data.R", encoding = "UTF-8")

# read, restructure & analyse air quality and inhabitant raster data, derive population and ecosystem exposition & save data 
source("scripts/_compile_exposition_data.R", encoding = "UTF-8")

# plot results
# source("scripts/_plot_airquality.R", encoding = "UTF-8")
