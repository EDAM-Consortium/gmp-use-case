# *========================= DHIS2 shapefile quality ==========================*
# Produces a per-level quality summary and a combined problems GeoJSON.
#
# Outputs in src/R/outputs/data_quality/not_used/shapefiles/dhis2_shapefile_quality/:
#   dhis2_shapefile_quality.csv  — per-level summary table
#   dhis2_problems.csv           — dhis2_problems.geojson as a table with centroids
#   dhis2_problems.geojson       — all flagged features with problem / problem_id
#                                  IP# = outside Ethiopia boundary
#                                  WD# = within-level name duplicate group
#                                  AD# = across-level name duplicate group
# *============================================================================*

# *--------------------------------- imports ----------------------------------*
library(dplyr)
library(tidyr)
library(sf)
library(readr)
library(stringr)

# *---------------------------------- paths -----------------------------------*
shp_dir <- here::here("data", "shapefiles", "DHIS2")
out_dir <- here::here("src", "R", "outputs", "data_quality", "not_used", "shapefiles", "dhis2_shapefile_quality")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# *----------------------------------- load -----------------------------------*
# Ethiopia boundary: union of level-2 (regional) polygons, same approach as
# explore_dhis2_geojsons.R
eth_union <- st_read(
  file.path(shp_dir, "ethiopia_dhis2_level2_points.geojson"),
  quiet = TRUE
) |>
  st_make_valid() |>
  st_union()

dhis2_all <- st_read(
  file.path(shp_dir, "dhis2_combined.geojson"),
  quiet = TRUE,
  fid_column_name = "dhis2_id"
) |>
  st_make_valid() |>
  mutate(name = str_trim(name))

centroids_xy <- st_coordinates(suppressWarnings(st_centroid(
  dhis2_all$geometry
)))
dhis2_all <- dhis2_all |>
  mutate(
    geom_type = as.character(st_geometry_type(geometry)),
    is_zero = centroids_xy[, 1] == 0 & centroids_xy[, 2] == 0,
    area_m2 = as.numeric(st_area(geometry)),
    outside_eth = as.numeric(st_distance(
      suppressWarnings(st_centroid(geometry)),
      eth_union
    )) >
      0
  )

normalise <- function(x) {
  x |>
    str_trim() |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9 ]", " ") |>
    str_squish()
}

dhis2_all <- dhis2_all |> mutate(name_norm = normalise(name))
base <- dhis2_all |> st_drop_geometry()

# *------------------------------- admin lookup -------------------------------*
admin_names <- c(
  "1" = "Regional",
  "2" = "Zonal",
  "3" = "Woreda",
  "4" = "PHCU",
  "5" = "Facilities"
)

# *------------------------- per-level summary table --------------------------*
geom_counts <- base |>
  group_by(level, geom_type) |>
  summarise(n = n(), .groups = "drop") |>
  pivot_wider(names_from = geom_type, values_from = n, values_fill = 0L)

n_features <- base |>
  group_by(level) |>
  summarise(n_features = n(), .groups = "drop")

outside_counts <- base |>
  filter(outside_eth) |>
  group_by(level) |>
  summarise(outside_ethiopia = n(), .groups = "drop")

dup_within_counts <- base |>
  group_by(name_norm, level) |>
  filter(n() > 1) |>
  ungroup() |>
  group_by(level) |>
  summarise(dup_within_level = n(), .groups = "drop")

dup_across_counts <- base |>
  group_by(name_norm) |>
  filter(n_distinct(level) > 1) |>
  ungroup() |>
  group_by(level) |>
  summarise(dup_across_levels = n(), .groups = "drop")

shapefile_quality <- n_features |>
  left_join(geom_counts, by = "level") |>
  left_join(outside_counts, by = "level") |>
  left_join(dup_within_counts, by = "level") |>
  left_join(dup_across_counts, by = "level") |>
  mutate(
    admin_level = as.integer(level) - 1L,
    admin_name = admin_names[as.character(admin_level)],
    outside_ethiopia = coalesce(outside_ethiopia, 0L),
    dup_within_level = coalesce(dup_within_level, 0L),
    dup_across_levels = coalesce(dup_across_levels, 0L)
  ) |>
  select(admin_level, admin_name, n_features, everything(), -level) |>
  arrange(admin_level)

print(as.data.frame(shapefile_quality), row.names = FALSE)
write_csv(shapefile_quality, file.path(out_dir, "dhis2_shapefile_quality.csv"))

# *----------------------------- problems GeoJSON -----------------------------*
# Add admin_level / admin_name to the sf object for use in all problem layers.
dhis2_all <- dhis2_all |>
  mutate(
    admin_level = as.integer(level) - 1L,
    admin_name = admin_names[as.character(admin_level)]
  )

shared_cols <- c(
  "id",
  "name",
  "name_norm",
  "admin_level",
  "admin_name",
  "geom_type",
  "is_zero",
  "area_m2",
  "outside_eth"
)

# IP# — one sequential ID per individual outside-boundary feature
ip_features <- dhis2_all |> filter(outside_eth)
ip_width    <- nchar(nrow(ip_features))
ip_problems <- ip_features |>
  mutate(
    problem    = "Incorrect point",
    problem_id = paste0("IP", sprintf(paste0("%0", ip_width, "d"), row_number()))
  ) |>
  select(all_of(shared_cols), problem, problem_id)

# WD# — all features in the same within-level duplicate group share one ID
wd_groups   <- dhis2_all |> group_by(name_norm, level) |> filter(n() > 1)
wd_width    <- nchar(n_distinct(interaction(wd_groups$name_norm, wd_groups$level)))
wd_problems <- wd_groups |>
  mutate(
    problem    = "Within duplication",
    problem_id = paste0("WD", sprintf(paste0("%0", wd_width, "d"), cur_group_id()))
  ) |>
  ungroup() |>
  select(all_of(shared_cols), problem, problem_id)

# AD# — all features with the same name (across levels) share one ID
ad_groups   <- dhis2_all |> group_by(name_norm) |> filter(n_distinct(level) > 1)
ad_width    <- nchar(n_distinct(ad_groups$name_norm))
ad_problems <- ad_groups |>
  mutate(
    problem    = "Across duplication",
    problem_id = paste0("AD", sprintf(paste0("%0", ad_width, "d"), cur_group_id()))
  ) |>
  ungroup() |>
  select(all_of(shared_cols), problem, problem_id)

problems_sf <- bind_rows(ip_problems, wd_problems, ad_problems) |>
  arrange(problem, problem_id)

st_write(
  problems_sf,
  file.path(out_dir, "dhis2_problems.geojson"),
  delete_dsn = TRUE,
  quiet = TRUE
)

centroids <- suppressWarnings(st_centroid(problems_sf))
centroid_coords <- st_coordinates(centroids)

write_csv(
  problems_sf |>
    st_drop_geometry() |>
    mutate(
      centroid_lon = centroid_coords[, 1],
      centroid_lat = centroid_coords[, 2]
    ),
  file.path(out_dir, "dhis2_problems.csv")
)

n_total  <- nrow(base)
n_points <- sum(base$geom_type == "POINT")

pct <- function(n, denom) sprintf("%.1f%%", n / denom * 100)

cat(
  "\nProblems GeoJSON —",
  nrow(ip_problems), paste0("(", pct(nrow(ip_problems), n_points), " of points)"),
  "incorrect points |",
  nrow(wd_problems), paste0("(", pct(nrow(wd_problems), n_total), " of all features)"),
  "within-dup rows |",
  nrow(ad_problems), paste0("(", pct(nrow(ad_problems), n_total), " of all features)"),
  "across-dup rows\n",
  "written to: dhis2_problems.geojson and dhis2_problems.csv\n"
)

# *============================================================================*
