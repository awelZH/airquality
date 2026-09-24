# targets pipeline of the analysis: one target list per sub-analysis in pipelines/ (notes/plan_phase2b.md).
# Build with run.R or targets::tar_make(); inspect with tar_visnetwork(), tar_manifest(), tar_read(),
# tar_load() and, after an error, tar_workspace().

library(targets)
library(tarchetypes)

tar_option_set(workspace_on_error = TRUE)

# functions (R/), then the analysis settings and paths (settings.R), then the target lists (pipelines/);
# targets tracks the functions and settings the targets use
tar_source("R")
source("settings.R", encoding = "UTF-8")
tar_source("pipelines")

# the target lists of all pipeline files (objects named pipeline_*)
mget(ls(pattern = "^pipeline_"))
