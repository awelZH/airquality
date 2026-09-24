# Build all plots of the report in the current session (for inspection and development; the Quarto pages
# in report/ source the topic scripts themselves). Run from the project root.
# ---

source("report/plots/_plot_setup.R", encoding = "UTF-8")

source("report/plots/_plot_emissions.R", encoding = "UTF-8")  # -> plots_emissions
source("report/plots/_plot_monitoring.R", encoding = "UTF-8") # -> plots_monitoring
source("report/plots/_plot_trends.R", encoding = "UTF-8")     # -> plots_trends
source("report/plots/_plot_exposition.R", encoding = "UTF-8") # -> plots_exposition
source("report/plots/_plot_outcomes.R", encoding = "UTF-8")   # -> plots_outcomes
