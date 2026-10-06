# *============================== Run all scripts ==============================*
# Sources every analysis script in dependency order (see the Workflow section
# of README.md). Run from anywhere inside the repository:
#   source(here::here("src", "R", "run_all.R"))
#
# Each script runs in its own environment, so objects from one script cannot
# leak into the next.
#
# Set run_data_quality to TRUE to also run the data quality checks. They are
# not needed to reproduce the results; explore_dhis2_geojsons.R rebuilds
# data/shapefiles/DHIS2/dhis2_combined.geojson, which is already in the
# repository, so they run before data preparation when enabled.
# *============================================================================*

# *--------------------------------- options ----------------------------------*
run_data_quality <- FALSE

# *--------------------------------- scripts ----------------------------------*
data_quality_scripts <- c(
  "data_quality/shapefiles/explore_dhis2_geojsons.R",
  "data_quality/shapefiles/explore_ocha.R",
  "data_quality/shapefiles/explore_espen.R",
  "data_quality/shapefiles/dhis2_shapefile_quality.R",
  "data_quality/DHIS2/gmp_completeness.R",
  "data_quality/DHIS2/completeness_report.R"
)

analysis_scripts <- c(
  # Step 1: data preparation (idb_under2 and shapes before gmp_coverage)
  "data_preparation/idb_under2.R",
  "data_preparation/generate_dhis2_gmp_shapes.R",
  "data_preparation/gmp_coverage.R",

  # RQ1: urban / rural
  "RQ1_urban_rural/gmp_urban_rural_timeseries.R",
  "RQ1_urban_rural/wb_urban_rural.R",

  # RQ2 + RQ3: national standard and geographic variation
  # (gmp_overall_comparison reads gmp_urban_rural_timeseries outputs)
  "RQ2_RQ3_national_level_and_geographic_variation/gmp_overall_comparison.R",
  "RQ2_RQ3_national_level_and_geographic_variation/gmp_ind_coverage.R",
  "RQ2_RQ3_national_level_and_geographic_variation/gmp_regional_maps.R",
  "RQ2_RQ3_national_level_and_geographic_variation/map_gmp.R",
  "RQ2_RQ3_national_level_and_geographic_variation/gmp_regional_sd.R",
  "RQ2_RQ3_national_level_and_geographic_variation/analyse_idb_gmp_nut.R",

  # RQ4: underweight
  "RQ4_underweight/acute_malnutrition_trends.R"
)

scripts <- c(if (run_data_quality) data_quality_scripts, analysis_scripts)

# *----------------------------------- run ------------------------------------*
for (script in scripts) {
  message("\n==> ", script)
  source(here::here("src", "R", script), local = new.env())
}

message("\nDone: ran ", length(scripts), " scripts.")
