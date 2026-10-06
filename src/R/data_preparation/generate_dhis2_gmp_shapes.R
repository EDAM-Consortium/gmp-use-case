# Script to map GMP values to DHIS2 shapefiles
library(dplyr)
library(readr)
library(sf)
library(stringr)
library(stringdist)

LEVELS <- c(1, 2, 3, 4, 5)
SELECTED_VARS <- c("NUT_Children <2 Years Weighted during GMP Session")

data_dir <- here::here("data", "GMP", "data_elements")
shp_dir <- here::here("data", "shapefiles", "DHIS2")
out_dir <- here::here("src", "R", "outputs", "data_preparation", "not_used", "generate_dhis2_gmp_shapes")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

type <- "de"
files <- list.files(
  data_dir,
  pattern = paste0(
    "malnutrition_",
    type,
    "_level(",
    paste(LEVELS, collapse = "|"),
    ")_.*\\.csv"
  ),
  full.names = TRUE
)

combined_gmp_data <- read_csv(files, show_col_types = FALSE) |>
  filter(data_element_name %in% SELECTED_VARS)

pre_disctint_count <- nrow(combined_gmp_data)
combined_gmp_data <- distinct(combined_gmp_data)
post_disctint_count <- nrow(combined_gmp_data)

if (pre_disctint_count != pre_disctint_count) {
  cat(
    "Removed ",
    pre_disctint_count - post_disctint_count,
    " non-distinct rows"
  )
}

# All data are monthly
combined_gmp_data |>
  distinct(period_type)

combined_gmp_data <- combined_gmp_data |>
  select(
    data_element_name,
    category_option_combo_name,
    period,
    org_unit_name, # For joining
    org_unit_level,
    org_unit_level_name,
    org_unit_hierarchy, # Could use for joining?
    value
  ) |>
  mutate(
    year = as.integer(substr(period, 1, 4)),
    month = as.integer(substr(period, 5, 6)),
    date = as.Date(paste(year, month, "01", sep = "-"))
  ) |>
  select(-period)

# -----------------------------------------------------------------
# Combined DHIS2 shapefile created by explore_shapefiles/explore_dhis2_geojsons
dhis2_all <- st_read(
  file.path(shp_dir, paste0("dhis2_combined.geojson")),
  quiet = TRUE,
  fid_column_name = "dhis2_id"
) |>
  st_make_valid() |>
  mutate(name = str_trim(name))

# Report duplicates before deduplication
centroids_xy <- st_coordinates(suppressWarnings(st_centroid(
  dhis2_all$geometry
)))
dhis2_all <- dhis2_all |>
  mutate(
    is_zero = centroids_xy[, 1] == 0 & centroids_xy[, 2] == 0,
    area = as.numeric(st_area(geometry))
  )

dup_within <- dhis2_all |>
  st_drop_geometry() |>
  group_by(name, level) |>
  filter(n() > 1) |>
  nrow()

dup_across <- dhis2_all |>
  st_drop_geometry() |>
  group_by(name) |>
  filter(n_distinct(level) > 1) |>
  nrow()

cat("Within-level name duplicates:", dup_within, "rows\n")
cat("Names appearing across multiple levels:", dup_across, "rows\n")

# Deduplicate within (name, level): prefer non-zero location, then largest polygon
# 0,0 features are only dropped when a valid-location duplicate exists
dhis2_all <- dhis2_all |>
  group_by(name) |>
  arrange(is_zero, desc(area), .by_group = TRUE) |>
  slice(1) |>
  ungroup() |>
  select(-is_zero, -area)

cat("Total DHIS2 features after deduplication:", nrow(dhis2_all), "\n")

# *--------------- 2. Name matching: GMP units → DHIS2 features ---------------*
normalise <- function(x) {
  x |>
    str_trim() |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9 ]", " ") |>
    str_squish()
}

strip_suffixes <- function(x) {
  x |>
    str_replace_all("\\bhealth center\\s+phcu\\b", "health center") |>
    str_replace_all("\\bphcu\\b", "health center") |>
    str_squish()
}

dhis2_shape_meta <- dhis2_all |>
  st_drop_geometry() |>
  select(dhis2_shape_name = name, dhis2_shape_level = level)

dhis2_shape_raw <- dhis2_shape_meta$dhis2_shape_name
dhis2_shape_norm <- normalise(dhis2_shape_raw)
dhis2_shape_core <- strip_suffixes(dhis2_shape_norm)

# GMP names that we want to match
gmp_units <- combined_gmp_data |>
  distinct(org_unit_name, org_unit_level)

# Detect names that normalise to the same value and collapse to one canonical form
name_map <- gmp_units |>
  mutate(norm_name = normalise(org_unit_name)) |>
  group_by(norm_name, org_unit_level) |>
  mutate(canonical_name = min(org_unit_name)) |>
  ungroup() |>
  select(org_unit_name, org_unit_level, canonical_name)

# Names that appear at multiple org unit levels in the GMP data are ambiguous:
# the same name could be a different place, or a data entry error.
cross_level_name_issues <- gmp_units |>
  group_by(org_unit_name) |>
  filter(n_distinct(org_unit_level) > 1) |>
  ungroup() |>
  arrange(org_unit_name, org_unit_level) |>
  mutate(issue = "same_name_multiple_org_levels")

if (nrow(cross_level_name_issues) > 0) {
  cat("Names appearing at multiple org unit levels:\n")
  print(
    as.data.frame(cross_level_name_issues |> select(-issue)),
    row.names = FALSE
  )
}

norm_dups <- name_map |> filter(org_unit_name != canonical_name)
if (nrow(norm_dups) > 0) {
  cat("Normalisation duplicates collapsed:", nrow(norm_dups), "\n")
  print(as.data.frame(norm_dups))
} else {
  cat("No normalisation duplicates found in GMP data\n")
}

# Overwriting org_unit_name with canonical name
combined_gmp_data <- combined_gmp_data |>
  left_join(name_map, by = c("org_unit_name", "org_unit_level")) |>
  mutate(org_unit_name = canonical_name) |>
  select(-canonical_name)

# Apply manual name overrides from manual_name_updates.csv
manual_overrides <- read_csv(
  here::here("src", "R", "data_preparation", "manual_name_updates.csv"),
  show_col_types = FALSE
) |>
  select(org_unit_name = gmp_name, override_name = nearest_shp_name)

if (nrow(manual_overrides) > 0) {
  cat("Applying", nrow(manual_overrides), "manual name overrides\n")
  combined_gmp_data <- combined_gmp_data |>
    left_join(manual_overrides, by = "org_unit_name") |>
    mutate(org_unit_name = coalesce(override_name, org_unit_name)) |>
    select(-override_name)
}

gmp_units <- combined_gmp_data |>
  distinct(org_unit_name, org_unit_level)

gmp_raw <- gmp_units$org_unit_name
gmp_norm <- normalise(gmp_raw)
gmp_core <- strip_suffixes(gmp_norm)

result <- gmp_units |>
  mutate(
    dhis2_shape_name = NA_character_,
    dhis2_shape_level = NA_character_,
    match_step = NA_character_
  )

unmatched <- rep(TRUE, nrow(result))

apply_step <- function(
  label,
  gmp_keys = NULL,
  dhis2_shape_keys = NULL,
  idx = NULL
) {
  rows <- which(unmatched)
  if (is.null(idx)) {
    idx <- match(gmp_keys[rows], dhis2_shape_keys)
  }
  hit <- !is.na(idx)
  matched_rows <- rows[hit]
  result$dhis2_shape_name[matched_rows] <<- dhis2_shape_raw[idx[hit]]
  result$dhis2_shape_level[
    matched_rows
  ] <<- dhis2_shape_meta$dhis2_shape_level[idx[hit]]
  result$match_step[matched_rows] <<- label
  unmatched[matched_rows] <<- FALSE

  n_matched <- sum(!unmatched)
  n_rows <- nrow(result)
  cat(
    label,
    "adds:",
    length(matched_rows),
    " | running total:",
    n_matched,
    "/",
    n_rows,
    "(",
    round(n_matched / n_rows * 100),
    "%)\n"
  )
  by_level <- result |>
    group_by(org_unit_level) |>
    summarise(
      matched = sum(!is.na(match_step)),
      total = n(),
      .groups = "drop"
    ) |>
    mutate(pct = round(matched / total * 100))
  print(as.data.frame(by_level), row.names = FALSE)
}

apply_step("exact_raw", gmp_keys = gmp_raw, dhis2_shape_keys = dhis2_shape_raw)
apply_step(
  "exact_normalised",
  gmp_keys = gmp_norm,
  dhis2_shape_keys = dhis2_shape_norm
)
apply_step(
  "suffix_stripped",
  gmp_keys = gmp_core,
  dhis2_shape_keys = dhis2_shape_core
)

# Fuzzy: Jaro-Winkler < 0.02, only on unmatched rows
# Use norm not core to prevent unnecessary matches
rows <- which(unmatched)
fuzzy_idx <- vapply(
  rows,
  \(i) {
    dists <- stringdist(gmp_norm[i], dhis2_shape_norm, method = "jw")
    best <- which.min(dists)
    if (dists[best] < 0.02) best else NA_integer_
  },
  integer(1)
)
apply_step("fuzzy", idx = fuzzy_idx)

# *------- 2b. Direct OCHA name matching for still-unmatched GMP units --------*
ocha_dir <- here::here("data", "shapefiles", "OCHA")

ocha_admin1 <- st_read(file.path(ocha_dir, "eth_admin1.shp"), quiet = TRUE) |>
  st_make_valid() |>
  st_transform(4326)
ocha_admin2 <- st_read(file.path(ocha_dir, "eth_admin2.shp"), quiet = TRUE) |>
  st_make_valid() |>
  st_transform(4326)
ocha_admin3 <- st_read(file.path(ocha_dir, "eth_admin3.shp"), quiet = TRUE) |>
  st_make_valid() |>
  st_transform(4326)

# Stack names from all three levels; lower-level fields are NA for higher-level matches
ocha_name_pool <- bind_rows(
  # admin1 primary
  ocha_admin1 |>
    st_drop_geometry() |>
    transmute(
      ocha_name = adm1_name,
      adm1_name,
      adm1_pcode,
      adm2_name = NA_character_,
      adm2_pcode = NA_character_,
      adm3_name = NA_character_,
      adm3_pcode = NA_character_,
      center_lat,
      center_lon
    ),
  # admin1 reference name (where different)
  ocha_admin1 |>
    st_drop_geometry() |>
    filter(!is.na(adm1_ref_n), adm1_ref_n != adm1_name) |>
    transmute(
      ocha_name = adm1_ref_n,
      adm1_name,
      adm1_pcode,
      adm2_name = NA_character_,
      adm2_pcode = NA_character_,
      adm3_name = NA_character_,
      adm3_pcode = NA_character_,
      center_lat,
      center_lon
    ),
  # admin2 primary
  ocha_admin2 |>
    st_drop_geometry() |>
    transmute(
      ocha_name = adm2_name,
      adm1_name,
      adm1_pcode,
      adm2_name,
      adm2_pcode,
      adm3_name = NA_character_,
      adm3_pcode = NA_character_,
      center_lat,
      center_lon
    ),
  # admin2 alternative
  ocha_admin2 |>
    st_drop_geometry() |>
    filter(!is.na(adm2_alt_n)) |>
    transmute(
      ocha_name = adm2_alt_n,
      adm1_name,
      adm1_pcode,
      adm2_name,
      adm2_pcode,
      adm3_name = NA_character_,
      adm3_pcode = NA_character_,
      center_lat,
      center_lon
    ),
  # admin3 primary
  ocha_admin3 |>
    st_drop_geometry() |>
    transmute(
      ocha_name = adm3_name,
      adm1_name,
      adm1_pcode,
      adm2_name,
      adm2_pcode,
      adm3_name,
      adm3_pcode,
      center_lat,
      center_lon
    ),
  # admin3 alternative
  ocha_admin3 |>
    st_drop_geometry() |>
    filter(!is.na(adm3_alt_n)) |>
    transmute(
      ocha_name = adm3_alt_n,
      adm1_name,
      adm1_pcode,
      adm2_name,
      adm2_pcode,
      adm3_name,
      adm3_pcode,
      center_lat,
      center_lon
    )
) |>
  distinct(ocha_name, .keep_all = TRUE)

gmp_core[gmp_core == "central ethiopian region"] = "central ethiopia"
gmp_core[gmp_core == "south west ethiopia region"] = "south west ethiopia"

ocha_raw_pool <- ocha_name_pool$ocha_name
ocha_norm_pool <- normalise(ocha_raw_pool)
ocha_core_pool <- strip_suffixes(ocha_norm_pool)

# Add OCHA columns to result for the summary file
result <- result |>
  mutate(
    ocha_adm3_name = NA_character_,
    ocha_adm2_name = NA_character_,
    ocha_adm1_name = NA_character_,
    ocha_adm3_pcode = NA_character_,
    ocha_adm2_pcode = NA_character_,
    ocha_adm1_pcode = NA_character_
  )

ocha_direct_matches <- tibble(
  org_unit_name = character(),
  org_unit_level = numeric(),
  adm1_name = character(),
  adm1_pcode = character(),
  adm2_name = character(),
  adm2_pcode = character(),
  adm3_name = character(),
  adm3_pcode = character(),
  center_lat = numeric(),
  center_lon = numeric()
)

apply_ocha_step <- function(
  label,
  gmp_keys = NULL,
  ocha_keys = NULL,
  idx = NULL
) {
  rows <- which(unmatched)
  if (is.null(idx)) {
    idx <- match(gmp_keys[rows], ocha_keys)
  }
  hit <- !is.na(idx)
  matched_rows <- rows[hit]
  matched_meta <- ocha_name_pool[idx[hit], ]

  result$match_step[matched_rows] <<- label
  result$ocha_adm3_name[matched_rows] <<- matched_meta$adm3_name
  result$ocha_adm2_name[matched_rows] <<- matched_meta$adm2_name
  result$ocha_adm1_name[matched_rows] <<- matched_meta$adm1_name
  result$ocha_adm3_pcode[matched_rows] <<- matched_meta$adm3_pcode
  result$ocha_adm2_pcode[matched_rows] <<- matched_meta$adm2_pcode
  result$ocha_adm1_pcode[matched_rows] <<- matched_meta$adm1_pcode
  unmatched[matched_rows] <<- FALSE

  ocha_direct_matches <<- bind_rows(
    ocha_direct_matches,
    result[matched_rows, c("org_unit_name", "org_unit_level")] |>
      bind_cols(
        matched_meta |>
          select(
            adm1_name,
            adm1_pcode,
            adm2_name,
            adm2_pcode,
            adm3_name,
            adm3_pcode,
            center_lat,
            center_lon
          )
      )
  )

  n_matched <- sum(!unmatched)
  n_rows <- nrow(result)
  cat(
    label,
    "adds:",
    length(matched_rows),
    "| running total:",
    n_matched,
    "/",
    n_rows,
    "(",
    round(n_matched / n_rows * 100),
    "%)\n"
  )
  by_level <- result |>
    group_by(org_unit_level) |>
    summarise(
      matched = sum(!is.na(match_step)),
      total = n(),
      .groups = "drop"
    ) |>
    mutate(pct = round(matched / total * 100))
  print(as.data.frame(by_level), row.names = FALSE)
}

apply_ocha_step("ocha_exact_raw", gmp_keys = gmp_raw, ocha_keys = ocha_raw_pool)
apply_ocha_step(
  "ocha_exact_normalised",
  gmp_keys = gmp_norm,
  ocha_keys = ocha_norm_pool
)

apply_ocha_step(
  "ocha_suffix_stripped",
  gmp_keys = gmp_core,
  ocha_keys = ocha_core_pool
)

rows <- which(unmatched)
ocha_fuzzy_idx <- vapply(
  rows,
  \(i) {
    dists <- stringdist(gmp_norm[i], ocha_norm_pool, method = "jw")
    best <- which.min(dists)
    if (dists[best] < 0.02) best else NA_integer_
  },
  integer(1)
)
apply_ocha_step("ocha_fuzzy", idx = ocha_fuzzy_idx)

# *---------- 2c. ESPEN name matching for still-unmatched GMP units -----------*
espen_all <- st_read(
  here::here("data", "shapefiles", "ESPEN", "ESPEN_IU_2024.shp"),
  quiet = TRUE
) |>
  filter(ADMIN0 == "Ethiopia") |>
  st_make_valid() |>
  st_transform(4326)

# Pre-compute IU centroids; aggregate to admin1/admin2 representative coords
espen_iu_centroids <- espen_all |>
  st_centroid() |>
  mutate(
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  ) |>
  st_drop_geometry() |>
  select(ADMIN1, ADMIN2, Alt_ADMIN2, IUs_NAME, Alt_IU_Nam, IU_CODE, lon, lat)

espen_admin1_centers <- espen_iu_centroids |>
  group_by(ADMIN1) |>
  summarise(lon = mean(lon), lat = mean(lat), .groups = "drop")

espen_admin2_centers <- espen_iu_centroids |>
  group_by(ADMIN1, ADMIN2) |>
  summarise(lon = mean(lon), lat = mean(lat), .groups = "drop")

# Stack names from all levels; iu_name/iu_code are NA for admin1/admin2 matches
espen_name_pool <- bind_rows(
  # admin1
  espen_admin1_centers |>
    transmute(
      espen_name = ADMIN1,
      iu_name = NA_character_,
      iu_code = NA_character_,
      lon,
      lat
    ),
  # admin2 primary
  espen_admin2_centers |>
    transmute(
      espen_name = ADMIN2,
      iu_name = NA_character_,
      iu_code = NA_character_,
      lon,
      lat
    ),
  # admin2 alternative
  espen_iu_centroids |>
    filter(!is.na(Alt_ADMIN2)) |>
    group_by(Alt_ADMIN2, ADMIN1) |>
    summarise(lon = mean(lon), lat = mean(lat), .groups = "drop") |>
    transmute(
      espen_name = Alt_ADMIN2,
      iu_name = NA_character_,
      iu_code = NA_character_,
      lon,
      lat
    ),
  # IU primary
  espen_iu_centroids |>
    transmute(
      espen_name = IUs_NAME,
      iu_name = IUs_NAME,
      iu_code = IU_CODE,
      lon,
      lat
    ),
  # IU alternative
  espen_iu_centroids |>
    filter(!is.na(Alt_IU_Nam)) |>
    transmute(
      espen_name = Alt_IU_Nam,
      iu_name = IUs_NAME,
      iu_code = IU_CODE,
      lon,
      lat
    )
) |>
  distinct(espen_name, .keep_all = TRUE)

espen_raw_pool <- espen_name_pool$espen_name
espen_norm_pool <- normalise(espen_raw_pool)
espen_core_pool <- strip_suffixes(espen_norm_pool)

# Add ESPEN columns to result for the summary file
result <- result |>
  mutate(
    espen_iu_name = NA_character_,
    espen_iu_code = NA_character_
  )

espen_direct_matches <- tibble(
  org_unit_name = character(),
  org_unit_level = numeric(),
  lon = numeric(),
  lat = numeric()
)

apply_espen_step <- function(
  label,
  gmp_keys = NULL,
  espen_keys = NULL,
  idx = NULL
) {
  rows <- which(unmatched)
  if (is.null(idx)) {
    idx <- match(gmp_keys[rows], espen_keys)
  }
  hit <- !is.na(idx)
  matched_rows <- rows[hit]
  matched_meta <- espen_name_pool[idx[hit], ]

  result$match_step[matched_rows] <<- label
  result$espen_iu_name[matched_rows] <<- matched_meta$iu_name
  result$espen_iu_code[matched_rows] <<- matched_meta$iu_code
  unmatched[matched_rows] <<- FALSE

  espen_direct_matches <<- bind_rows(
    espen_direct_matches,
    tibble(
      org_unit_name = result$org_unit_name[matched_rows],
      org_unit_level = result$org_unit_level[matched_rows],
      lon = matched_meta$lon,
      lat = matched_meta$lat
    )
  )

  n_matched <- sum(!unmatched)
  n_rows <- nrow(result)
  cat(
    label,
    "adds:",
    length(matched_rows),
    "| running total:",
    n_matched,
    "/",
    n_rows,
    "(",
    round(n_matched / n_rows * 100),
    "%)\n"
  )
  by_level <- result |>
    group_by(org_unit_level) |>
    summarise(
      matched = sum(!is.na(match_step)),
      total = n(),
      .groups = "drop"
    ) |>
    mutate(pct = round(matched / total * 100))
  print(as.data.frame(by_level), row.names = FALSE)
}

apply_espen_step(
  "espen_exact_raw",
  gmp_keys = gmp_raw,
  espen_keys = espen_raw_pool
)
apply_espen_step(
  "espen_exact_normalised",
  gmp_keys = gmp_norm,
  espen_keys = espen_norm_pool
)
apply_espen_step(
  "espen_suffix_stripped",
  gmp_keys = gmp_core,
  espen_keys = espen_core_pool
)

rows <- which(unmatched)
espen_fuzzy_idx <- vapply(
  rows,
  \(i) {
    dists <- stringdist(gmp_norm[i], espen_norm_pool, method = "jw")
    best <- which.min(dists)
    if (dists[best] < 0.02) best else NA_integer_
  },
  integer(1)
)
apply_espen_step("espen_fuzzy", idx = espen_fuzzy_idx)


write.csv(
  result,
  file.path(out_dir, "gmp_dhis2_shape_name_matches.csv"),
  row.names = FALSE
)

# *----------------- 3. Build name lookup from match results ------------------*
name_lookup <- result |>
  filter(!is.na(dhis2_shape_name)) |>
  select(
    org_unit_name,
    org_unit_level,
    dhis2_shape_name,
    dhis2_shape_level,
    match_step
  )

cat("Matched org units:", nrow(name_lookup), "/", nrow(result), "\n")

# *------------- 4. Extract coordinates from matched DHIS2 shapes -------------*
# Use centroids so polygons and points are treated uniformly
dhis2_centroids <- dhis2_all |>
  st_centroid() |>
  mutate(
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  ) |>
  st_drop_geometry() |>
  select(
    dhis2_shape_name = name,
    dhis2_shape_level = level,
    dhis2_id = id,
    lon,
    lat
  )

units_geo <- name_lookup |>
  left_join(dhis2_centroids, by = c("dhis2_shape_name", "dhis2_shape_level")) |>
  filter(!is.na(lon), !is.na(lat))

cat("Org units with coordinates:", nrow(units_geo), "/", nrow(result), "\n")

units_sf <- units_geo |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# *---- 5. Spatial join to OCHA boundaries at the appropriate admin level -----*
# ocha_admin1/2/3 already loaded in section 2b
# DHIS2 level 2 (region)   → OCHA admin1
# DHIS2 level 3 (zone)     → OCHA admin2
# DHIS2 level 4-5 (woreda/facility) → OCHA admin3
ocha_cols <- c(
  "adm3_name",
  "adm2_name",
  "adm1_name",
  "adm3_pcode",
  "adm2_pcode",
  "adm1_pcode"
)

# Helper: within join with nearest fallback; deduplicates on org_unit_name
# Returns ocha_join_type ("within"/"nearest") and ocha_nearest_dist_m (NA for within)
MAX_NEAREST_DIST_M <- 10000  # drop nearest-fallback assignments > 10 km

join_to_ocha <- function(pts_sf, ocha_sf, join_cols, exclude_zero_nearest = TRUE) {
  if (nrow(pts_sf) == 0) {
    return(tibble(
      org_unit_name = character(),
      org_unit_level = numeric(),
      dhis2_shape_level = character()
    ))
  }
  check_col <- join_cols[1]

  within <- st_join(pts_sf, ocha_sf[, join_cols], join = st_within) |>
    group_by(org_unit_name, org_unit_level) |>
    slice(1) |>
    ungroup()

  matched   <- within |> filter(!is.na(.data[[check_col]]))
  unmatched <- within |> filter(is.na(.data[[check_col]]))

  nearest_rows <- tibble()
  if (nrow(unmatched) > 0) {
    if (exclude_zero_nearest) {
      coords    <- st_coordinates(unmatched)
      is_zero   <- coords[, 1] == 0 & coords[, 2] == 0
      near_pts  <- unmatched[!is_zero, ]
      skipped   <- sum(is_zero)
      if (skipped > 0)
        cat("  [join_to_ocha] skipping", skipped,
            "point(s) at (0,0) from nearest fallback\n")
    } else {
      near_pts <- unmatched
    }

    if (nrow(near_pts) > 0) {
      nearest_idx <- st_nearest_feature(near_pts, ocha_sf)
      dist_m      <- as.numeric(st_distance(
        near_pts, ocha_sf[nearest_idx, ], by_element = TRUE
      ))
      in_range  <- dist_m <= MAX_NEAREST_DIST_M
      dropped   <- sum(!in_range)
      if (dropped > 0)
        cat("  [join_to_ocha] dropping", dropped,
            "point(s) with nearest dist >", MAX_NEAREST_DIST_M / 1000, "km\n")
      nearest_rows <- near_pts[in_range, ] |>
        st_drop_geometry() |>
        select(org_unit_name, org_unit_level, dhis2_shape_level) |>
        bind_cols(ocha_sf[nearest_idx[in_range], join_cols] |> st_drop_geometry()) |>
        mutate(ocha_join_type = "nearest", ocha_nearest_dist_m = dist_m[in_range])
    }
  }

  bind_rows(
    matched |>
      st_drop_geometry() |>
      select(org_unit_name, org_unit_level, dhis2_shape_level, all_of(join_cols)) |>
      mutate(ocha_join_type = "within", ocha_nearest_dist_m = NA_real_),
    nearest_rows
  )
}

lvl2_lookup <- join_to_ocha(
  units_sf |> filter(dhis2_shape_level == "2"),
  ocha_admin1,
  c("adm1_name", "adm1_pcode")
) |>
  mutate(
    adm2_name = NA_character_,
    adm2_pcode = NA_character_,
    adm3_name = NA_character_,
    adm3_pcode = NA_character_
  )

lvl3_lookup <- join_to_ocha(
  units_sf |> filter(dhis2_shape_level == "3"),
  ocha_admin2,
  c("adm1_name", "adm1_pcode", "adm2_name", "adm2_pcode")
) |>
  mutate(adm3_name = NA_character_, adm3_pcode = NA_character_)

lvl45_lookup <- join_to_ocha(
  units_sf |> filter(dhis2_shape_level %in% c("4", "5")),
  ocha_admin3,
  c(
    "adm1_name",
    "adm1_pcode",
    "adm2_name",
    "adm2_pcode",
    "adm3_name",
    "adm3_pcode"
  )
)

woreda_lookup <- bind_rows(lvl2_lookup, lvl3_lookup, lvl45_lookup)
cat("Total woreda lookup entries (DHIS2 path):", nrow(woreda_lookup), "\n")

# Add OCHA-direct matches, using their center coordinates for adm3 assignment
if (nrow(ocha_direct_matches) > 0) {
  ocha_direct_woreda <- ocha_direct_matches |>
    mutate(
      dhis2_shape_level = as.character(org_unit_level),
      ocha_join_type = "direct_name",
      ocha_nearest_dist_m = NA_real_
    ) |>
    select(
      org_unit_name,
      org_unit_level,
      dhis2_shape_level,
      all_of(ocha_cols),
      ocha_join_type,
      ocha_nearest_dist_m
    )

  woreda_lookup <- bind_rows(woreda_lookup, ocha_direct_woreda)
  cat("OCHA-direct entries added:", nrow(ocha_direct_woreda), "\n")
}

# ESPEN-matched units: use stored centroids then spatially join to OCHA
if (nrow(espen_direct_matches) > 0) {
  espen_units_sf <- espen_direct_matches |>
    mutate(dhis2_shape_level = as.character(org_unit_level)) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)

  espen_lvl2 <- join_to_ocha(
    espen_units_sf |> filter(dhis2_shape_level == "2"),
    ocha_admin1,
    c("adm1_name", "adm1_pcode")
  ) |>
    mutate(
      adm2_name = NA_character_,
      adm2_pcode = NA_character_,
      adm3_name = NA_character_,
      adm3_pcode = NA_character_
    )

  espen_lvl3 <- join_to_ocha(
    espen_units_sf |> filter(dhis2_shape_level == "3"),
    ocha_admin2,
    c("adm1_name", "adm1_pcode", "adm2_name", "adm2_pcode")
  ) |>
    mutate(adm3_name = NA_character_, adm3_pcode = NA_character_)

  espen_lvl45 <- join_to_ocha(
    espen_units_sf |> filter(dhis2_shape_level %in% c("4", "5")),
    ocha_admin3,
    c(
      "adm1_name",
      "adm1_pcode",
      "adm2_name",
      "adm2_pcode",
      "adm3_name",
      "adm3_pcode"
    )
  )

  espen_woreda <- bind_rows(espen_lvl2, espen_lvl3, espen_lvl45)
  woreda_lookup <- bind_rows(woreda_lookup, espen_woreda)
  cat("ESPEN-direct entries added:", nrow(espen_woreda), "\n")
}

dhis2_id_lookup <- units_geo |>
  select(org_unit_name, org_unit_level, dhis2_shape_level, dhis2_id)

woreda_lookup <- woreda_lookup |>
  left_join(
    dhis2_id_lookup,
    by = c("org_unit_name", "org_unit_level", "dhis2_shape_level")
  )

n_before <- nrow(woreda_lookup)

dup_keys <- woreda_lookup |>
  group_by(org_unit_name, org_unit_level) |>
  filter(n() > 1) |>
  ungroup()

if (nrow(dup_keys) > 0) {
  cat("Duplicate (org_unit_name, org_unit_level) keys before dedup:\n")
  print(
    as.data.frame(dup_keys |> arrange(org_unit_name, org_unit_level)),
    row.names = FALSE
  )
}

# Priority 1: dhis2_shape_level matches org_unit_level (exact level match).
# Priority 2 (fallback, only when no exact match): OCHA pcode non-null at the
#   expected level for that shape level.
woreda_lookup <- woreda_lookup |>
  group_by(org_unit_name, org_unit_level) |>
  arrange(
    desc(dhis2_shape_level == as.character(org_unit_level)),
    desc(case_when(
      org_unit_level == "2" ~ !is.na(adm1_pcode),
      org_unit_level == "3" ~ !is.na(adm2_pcode),
      org_unit_level %in% c("4", "5") ~ !is.na(adm3_pcode),
      TRUE ~ FALSE
    )),
    .by_group = TRUE
  ) |>
  slice(1) |>
  ungroup()

cat(
  "Total woreda lookup entries:",
  nrow(woreda_lookup),
  "(removed",
  n_before - nrow(woreda_lookup),
  "duplicate keys)\n"
)

# *------------------------------- Input issues -------------------------------*
input_issues <- bind_rows(
  cross_level_name_issues,
  if (nrow(dup_keys) > 0) {
    dup_keys |> mutate(issue = "duplicate_lookup_key")
  } else {
    NULL
  }
)

if (nrow(input_issues) > 0) {
  write_csv(input_issues, file.path(out_dir, "input_issues.csv"))
  cat("Input issues saved to input_issues.csv (", nrow(input_issues), "rows)\n")
}

# *--------------- 6. Join OCHA names back to full GMP dataset ----------------*
gmp_spatial <- combined_gmp_data |>
  # filter(org_unit_level == 4) |>
  left_join(
    woreda_lookup |> mutate(org_unit_level = as.double(org_unit_level)),
    by = c("org_unit_name", "org_unit_level")
  )

cat("Final rows:", nrow(gmp_spatial), "\n")
cat(
  "Rows with OCHA assignment:",
  sum(!is.na(gmp_spatial$adm3_name)),
  "\n"
)
cat(
  "Rows without OCHA assignment:",
  sum(is.na(gmp_spatial$adm3_name)),
  "\n"
)

write_csv(gmp_spatial, file.path(out_dir, "gmp_spatial.csv"))
cat("\nSaved gmp_spatial.csv\n")

# *============================================================================*
