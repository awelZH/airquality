# Build all plots of the report in the current session (for inspection and development; the Quarto pages
# in docs/ source the topic scripts themselves). Run from the project root.
# ---

source("scripts/_plot_setup.R", encoding = "UTF-8")

source("scripts/_plot_emissions.R", encoding = "UTF-8")  # -> plots_emissions
source("scripts/_plot_monitoring.R", encoding = "UTF-8") # -> plots_monitoring
source("scripts/_plot_trends.R", encoding = "UTF-8")     # -> plots_trends
source("scripts/_plot_exposition.R", encoding = "UTF-8") # -> plots_exposition
source("scripts/_plot_outcomes.R", encoding = "UTF-8")   # -> plots_outcomes
