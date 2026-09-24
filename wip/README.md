# Work in progress

Analyses that are still being developed and therefore run outside the targets pipeline
(`notes/plan_phase2b.md`, decision 7). Their outputs are part of the contract in `data/output/` and are
used by the report.

| Script | Outputs | Reads |
|---|---|---|
| `trends.R` – relative trends of emissions and immissions, meteo-normalised with random forests (about 30 min) | `data_airquality_trends_relative_y1.csv`, `data_airquality_trends_relative_aggregated_y1.csv` | `data_airquality_monitoring_y1.csv`, `data_emissions.csv` (pipeline outputs), `airquality.data` |

Run from the project root after the pipeline has updated its outputs, then build the report:

```r
targets::tar_make(names = !tidyselect::starts_with("report_"))
source("wip/trends.R", encoding = "UTF-8")
targets::tar_make()
```

The report warns (target `report_wip_check`) when a WIP output is older than the pipeline outputs it is
computed from. Once a script is finished it becomes a sub-analysis of the pipeline, as the health outcomes
did in phase 2b.
