# setup: the resource table and the municipality map, shared by several sub-analyses

pipeline_setup <- tar_plan(
  tar_target(setup_ressources_file, "data/meta/ressources.csv", format = "file"),
  setup_ressources = read_ressources(setup_ressources_file),

  # current municipal boundaries from geolion, used for all years, without the Kloster Fahr (decision 4)
  tar_target(
    setup_map_municipalities,
    airquality.methods::read_geolion_wfs(filter_ressources(setup_ressources, "municipalities"), version = "2.0.0", crs = crs) |>
      airquality.methods::drop_foreign_enclaves(),
    cue = tar_cue("always")
  )
)
