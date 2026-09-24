# Step 2 plan: targets project

Read before working on phase 2b. Status of the plan in `CLAUDE.md`.

**Status 2026-09-24: steps 1–5 done** (commits `a5469c5` … `775c35c`): data in `data/` (`d576e4f`), Quarto
sources in `report/` (`3582e45`), pipelines incl. outcomes and report (`57a08fc`), package skeleton removed
(`775c35c`), `airquality.methods` pinned (`809fc94`). Not done from step 1/6: no `_targets.yaml` (the
defaults suffice), no `dev/` folder, no `tar_mermaid()` diagram in the docs, README not yet rewritten for the
pipeline. Step 7 (trends into the pipeline) waits for the user. Verification: on frozen inputs 11 of 12
outputs byte-identical to the old scripts, the health outcomes within 4.5e-11 (no CSV round trip); a second
`tar_make()` reruns only the 9 `tar_cue("always")` targets; the report renders through `report_site`
(6 min 40 s) with unchanged figures.

Approved 2026-09-18, refined 2026-09-19, revised 2026-09-24 (decisions 7 and 8). Goal: a `targets` pipeline that is readable step by step,
easy to debug, try out and extend, robust for the twice-yearly update, with unchanged output file
names and schemas.

## User decisions

1. **Outputs move to `data/output/`**; the external processes will be adjusted to the new path.
2. **Pure targets project, no package skeleton** (option B). No NAMESPACE, `man/`, `@export`,
   `load_all()`: targets tracks functions loaded with `tar_source()` automatically, and the stale
   NAMESPACE/`man/` plus the `load_all()` version prompt caused trouble in step 1. Given up:
   `?function` help pages and the `devtools::test()` shortcut; roxygen comments stay as in-code docs.
3. **Website stays rendered into `docs/`** (GitHub Pages), Quarto sources move to `report/`.
4. **`tod_nat_gatu.csv`** (non-public mortality data, gitignored) stays in the project, in
   `data/restricted/`; may become OGD later.
5. Derived parameters refitted on every run, coefficients logged (already implemented).
6. **One target list per sub-analysis, not per topic** (2026-09-19), so each part can be built and
   inspected on its own (`tar_make(names = starts_with("emis_rsd_"))`):

   | sub-analysis (target prefix) | pipeline file | outputs |
   |---|---|---|
   | `emis_emikat_` | `pipelines/emissions_emikat.R` | `data_emissions.csv` |
   | `emis_rsd_` | `pipelines/emissions_rsd.R` | the 3 `data_nox_*rsd*.csv` |
   | `mon_aq_` | `pipelines/monitoring_airquality.R` | `data_airquality_monitoring_y1.csv` |
   | `mon_ndep_` | `pipelines/monitoring_ndep.R` | `data_ndep_pars_monitoring_y1.csv`, `data_ndep_monitoring_y1.csv` |
   | `expo_pop_` | `pipelines/exposition_population.R` | the 2 weighted-mean files, `data_exposition_distribution_pollutants.csv` |
   | `expo_eco_` | `pipelines/exposition_ecosystems.R` | `data_exposition_distribution_ndep.csv` |
   | `outcomes_` | `pipelines/outcomes.R` | `data_health_outcomes.csv` |
   | `report_` | `pipelines/report.R` | `docs/` (plots built while rendering, decision 9) |
   | – (WIP, see 7) | `wip/trends.R` | the 2 trend files |

7. **Work in progress stays outside targets for now** (2026-09-19; revised 2026-09-24: the health
   outcomes are reworked and join the pipeline as sub-analysis `outcomes_`, reading the canton means
   of `expo_pop_` and the restricted mortality file as a `format = "file"` target; only the trends stay
   WIP). Consequences for phase 2b:
   * the WIP script reads pipeline outputs from `data/output/` (trends ←
     `data_airquality_monitoring_y1.csv`, `data_emissions.csv`) and writes its outputs there; it
     `source()`s the `R/` files; `wip/README.md` says how to run it
   * its 2 outputs are part of the contract and used by the report: the report tracks them as
     external `format = "file"` targets, and a check target warns when a WIP output is older than the
     pipeline outputs it is based on
   * full run in `run.R`: `tar_make()` without the report → WIP script (optional) → `tar_make()`;
     each step can also be run alone
   * phase 2a still applies to the trends in full: tests first, pure functions in `R/trends.R`, thin
     script, regression on frozen inputs, a seed for the random forest
8. **No `config.yml`** (2026-09-24): all constants stay in one R file of assignments (decision 7),
   `settings.R` in the project root (was `scripts/_settings.R`), now also with the paths
   (`path_output` from the environment variable `AIRQUALITY_OUTPUT_DIR`, default `data/output`, so the
   regression can redirect the outputs). `_targets.R` sources it; targets tracks the settings as
   globals, so a changed setting reruns exactly the targets that use it.

## Target layout

```
_targets.R            options, tar_source("R"), combine pipelines/*
_targets.yaml         targets project settings
settings.R            all analysis constants and paths (decision 7 and 8)
run.R                 human entry point: tar_make(), progress summary, quarto render
DESCRIPTION           dependency manifest only (renv snapshot.type = "explicit")
R/                    pure functions per topic (roxygen comments kept as in-code docs)
pipelines/            one target list per sub-analysis (decision 6), plus setup
wip/                  work in progress outside targets (decision 7): trends.R, README.md
data/meta|output|log/ from inst/extdata/… (contract: names, columns, format unchanged)
data/restricted/      non-public inputs; folder gitignored except README.md
report/               Quarto sources (*.qmd, _quarto.yml) and plot scripts; output-dir ../docs
docs/                 rendered website only
dev/                  interactive scripts (tar_load(), experiments)
notes/                decisions, findings per topic, plans (read on demand, see CLAUDE.md)
tests/testthat/       unit tests per R/ file; helper sources R/; dependency test
tests/regression/     frozen baseline + generic comparison for all outputs
```

## Phase 2a – improve per topic (within the current script structure)

Order confirmed 2026-09-18: emissions → monitoring → outcomes → trends → plots/report. Per topic:
thin script (read → prepare → aggregate → write, no helper functions defined inside, no hidden
globals), pure functions in `R/<topic>.R`, tests first, decisions documented in `notes/`, and every
function designed so that it can later become a target 1:1.

## Phase 2b – structural change (only after phase 2a)

1. **Skeleton**, no behaviour change: `_targets.R`, `_targets.yaml`, `settings.R` (decision 8),
   `run.R`, `pipelines/setup.R`; `tests/testthat/helper-source.R` sourcing `R/`; `tests/testthat.R` →
   `testthat::test_dir()`; renv `snapshot.type = "explicit"`; dependency test comparing
   `renv::dependencies()` with DESCRIPTION; `.gitignore` adds `_targets/`.
2. **Data move**: `git mv inst/extdata/{meta,output,log}` → `data/…`; `tod_nat_gatu.csv` →
   `data/restricted/` plus a committed README and gitignore rules; `ressources.csv` entry for the
   mortality source; update paths in `ressources.csv`, `prepare_ressources()`, `report/index.qmd`,
   schema test, `CLAUDE.md` and `notes/`. Output path from `settings.R`.
3. **Sub-analysis pipelines** from the functions improved in phase 2a, order: expo_pop (raster
   metadata target with `tar_cue("always")`; checks as targets before writing) → expo_eco →
   emis_emikat → emis_rsd → mon_aq → mon_ndep → outcomes → report (done as target `report_site` with `render_report()` instead of `tarchetypes::tar_quarto()`, which only sees `tar_read()` calls in the pages; it renders
   `report/` into `docs/`; it depends on the output CSVs as file targets, no plot targets; the plot
   scripts move along with the report and stay usable in the console, decision 9; WIP outputs as
   external file targets plus staleness check).
   Conventions: target names `<topic>_<subanalysis>_<stage>_<content>` (e.g. `emis_emikat_raw`,
   `expo_pop_out_weighted_means_canton`), `tar_plan()` syntax, outputs as `format = "file"` targets.
   Old scripts keep running until their sub-analysis is migrated.
4. **Remove the package skeleton** once all sub-analyses run in targets: NAMESPACE, `man/`,
   `@export` tags, `scripts/`, `analyse_airquality.R`; `tar_option_set(workspace_on_error = TRUE)`.
5. **airquality.methods**: done 2026-09-24, `renv.lock` pins `awelZH/airquality.methods@7252da2`. The generic
   pieces already moved there on 2026-09-21 (decision 10): municipality assignment, collector pixel
   redistribution, grouped legend, `check_names()`; `append_log()` replaced by `write_local_csv()`.
6. **Docs**: `CLAUDE.md` and `notes/` (structure, decisions, workflow `tar_make()` / `tar_load()` /
   `tar_workspace()`, `tar_mermaid()` diagram), README, `wip/README.md`.
7. **Later, when the user has finished it: integrate the WIP trends** as sub-analysis `trends_` (the
   outcomes joined the pipeline in phase 2b already, decision 7):
   targets' per-target seeds for the random forest; optional `crew` for the ~30 min of the
   trends; the report's external file targets then become normal dependencies.

## Verification of phase 2b

* `testthat::test_dir("tests/testthat")` green after every step; `tar_validate()`, `tar_manifest()`,
  `tar_visnetwork()` show the intended graph.
* Each sub-analysis builds alone, then regression on frozen inputs: identical outputs (all
  deterministic). A second `tar_make()` without changes skips everything except the metadata cues.
* The WIP scripts run from `wip/` against `data/output/`; the report warns when a WIP output is
  stale; a missing restricted file stops with the README hint.
* Rendering via `tar_make()` updates `docs/` with the same pages as before.
