# Entry point of the analysis: build the outputs with targets, optionally update the work-in-progress trends,
# then render the report. Each step can be run on its own; details in CLAUDE.md and notes/plan_phase2b.md.
#
# Inspect the pipeline with targets::tar_visnetwork() or targets::tar_manifest(); read a result with
# targets::tar_read(<target>); after an error, targets::tar_workspace(<target>) restores its inputs.
#
# Rendering the report alone without checking the outputs: name report_sources as well, it lists the report
# sources on every run. With shortcut = TRUE it is skipped otherwise, so changed pages go unnoticed and the
# report is not rendered:
#   targets::tar_make(names = c("report_sources", "report_site"), shortcut = TRUE)

# 1) outputs in data/output/ (the contract): the versions of the downloads (opendata.swiss, data.geo.admin.ch)
#    are checked on every run and the data read only when they changed; everything else is rebuilt
#    only if its inputs, functions or settings changed
targets::tar_make(names = !tidyselect::starts_with("report_"))

# 2) work in progress outside the pipeline (about 30 min); only when the trends are to be updated
# source("wip/trends.R", encoding = "UTF-8")

# 3) report: renders report/ into docs/ (about 7 min) when an output, a trend file or a report source changed.
#    shortcut = TRUE skips checking the upstream targets again (step 1 just built them; the always-cued version
#    checks and the geolion map would otherwise run a second time); run alone, it does not notice outdated
#    outputs, so run step 1 first. The split exists only for step 2; once the trends are in the pipeline,
#    run.R becomes a single targets::tar_make() (notes/plan_phase2b.md, step 7)
targets::tar_make(names = tidyselect::starts_with("report_"), shortcut = TRUE)
