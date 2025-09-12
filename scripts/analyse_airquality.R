# 1) setup analysis: load libraries & functions & read ressource table + map boundaries data
print("1) setup analysis...")
source("scripts/_setup.R", encoding = "UTF-8")

# 2) read, restructure, analyse & save air pollutant emission data 
print("2) compile emission data...")
source("scripts/_compile_emission_data.R", encoding = "UTF-8")
  
# 3) read, restructure & save air quality monitoring data
print("3) compile monitoring data...")
source("scripts/_compile_monitoring_data.R", encoding = "UTF-8")

# 4) read, restructure & save air quality monitoring data
print("4) compile trends data...")
source("scripts/_compile_trend_data.R", encoding = "UTF-8")

# 5) read, restructure & analyse air quality and inhabitant raster data, derive population and ecosystem exposition & save data 
print("5) compile exposition data...")
source("scripts/_compile_exposition_data.R", encoding = "UTF-8")

# 6) calculate selected health-outcomes due to population air pollutant exposition & save data 
print("6) compile health-outcome data...")
source("scripts/_compile_outcomes.R", encoding = "UTF-8")

# internal process to produce *.html:

# 7) plot results for use in docs/...
# print("6) plot results...")
# source("scripts/_plot_airquality.R", encoding = "UTF-8")

# 8) render quarto website for plot display
# print("7) render *.qmd...")
# quarto::quarto_render(input = "docs/")
