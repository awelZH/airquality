# exposition: sensitive ecosystems by exceedance of the critical loads for nitrogen (BAFU raster)
# (functions in R/exposition.R) -> data_exposition_distribution_ndep.csv

pipeline_exposition_ecosystems <- tar_plan(
  # state of the raster assets, checked on every run; the exceedance is read again only when it changed
  tar_target(expo_eco_assets, geo_admin_asset_state(ndep_collection), cue = tar_cue("always")),
  expo_eco_ndep = {
    expo_eco_assets
    read_ndep_exceedance(setup_map_municipalities)
  },

  # distribution by class of critical load exceedance (canton)
  expo_eco_distribution = aggregate_ndep_exposition_distrib(expo_eco_ndep),

  tar_target(expo_eco_out_distribution,
             write_output(expo_eco_distribution, "data_exposition_distribution_ndep.csv", dir = path_output),
             format = "file")
)
