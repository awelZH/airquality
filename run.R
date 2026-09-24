# Entry point of the analysis: build the outputs with targets, optionally update the work-in-progress trends,
# then render the report. Each step can be run on its own; details in CLAUDE.md and notes/plan_phase2b.md.
#
# Inspect the pipeline with targets::tar_visnetwork() or targets::tar_manifest(); read a result with
# targets::tar_read(<target>); after an error, targets::tar_workspace(<target>) restores its inputs.

# 1) outputs in data/output/ (the contract): downloads are checked on every run, everything else is rebuilt
#    only if its inputs, functions or settings changed
targets::tar_make(names = !tidyselect::starts_with("report_"))

# 2) work in progress outside the pipeline (about 30 min); only when the trends are to be updated
# source("wip/trends.R", encoding = "UTF-8")

# 3) report: renders report/ into docs/ (about 7 min) when an output or a report source changed
targets::tar_make()
