# *========================= Explore DHIS2 Shapefiles =========================*
# Plots each admin level shapefile individually — polygons only, and a
# polygons + points overlay — with lon/lat axes for reference.
# *============================================================================*
library(ggplot2)
library(sf)
library(dplyr)

shp_dir <- here::here("data", "shapefiles", "DHIS2")
out_dir <- here::here("src", "R", "outputs", "data_quality", "not_used", "shapefiles", "explore_shapefiles", "DHIS2")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# *---------------- Points outside the l2 (Ethiopia) boundary -----------------*
cat("\n=== Points outside l2 boundary ===\n")

l2_sf <- st_read(
  file.path(shp_dir, "ethiopia_dhis2_level2_points.geojson"),
  quiet = TRUE
) |>
  st_make_valid()

eth_union <- st_union(l2_sf)

outside_boundary <- data.frame()

for (lvl in 3:6) {
  sf_obj <- st_read(
    file.path(shp_dir, paste0("ethiopia_dhis2_level", lvl, "_points.geojson")),
    quiet = TRUE
  )

  pts <- sf_obj |> filter(st_geometry_type(geometry) == "POINT")
  if (nrow(pts) == 0) {
    next
  }

  inside <- st_within(pts, eth_union, sparse = FALSE)[, 1]
  outside <- pts[!inside, ] |>
    st_drop_geometry() |>
    mutate(level = lvl) |>
    select(level, name, code)

  outside_boundary <- rbind(outside_boundary, outside)
}

print(outside_boundary)

# ----------------------------------
shp_files <- list.files(shp_dir, pattern = "\\.geojson$", full.names = TRUE)

base_theme <- list(
  theme_minimal(),
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 8),
    panel.grid = element_line(colour = "grey90")
  ),
  labs(x = "Longitude", y = "Latitude")
)

for (f in shp_files) {
  level_name <- tools::file_path_sans_ext(basename(f))
  cat("\n---", level_name, "---\n")

  sf_obj <- st_read(f, quiet = TRUE) |> st_make_valid()

  if (nrow(sf_obj) == 0) {
    cat("  EMPTY — skipping\n")
    next
  }

  geom_types <- st_geometry_type(sf_obj)
  cat("  Features:", nrow(sf_obj), "\n")
  print(table(geom_types))

  polys <- sf_obj |> filter(geom_types %in% c("POLYGON", "MULTIPOLYGON"))
  points <- sf_obj |> filter(geom_types == "POINT")

  # *----------------------------- Polygons only ------------------------------*
  if (nrow(polys) > 0) {
    p_poly <- ggplot() +
      geom_sf(
        data = polys,
        fill = "steelblue",
        alpha = 0.4,
        col = "black",
        lwd = 0.3
      ) +
      base_theme +
      labs(
        title = paste(level_name, "— polygons only"),
        subtitle = paste(nrow(polys), "polygon features")
      )
    ggsave(
      file.path(out_dir, paste0(level_name, "_polys.png")),
      plot = p_poly,
      width = 8,
      height = 7,
      dpi = 150
    )
  }

  # *----------------------- Overlay: polygons + points -----------------------*
  if (nrow(points) > 0) {
    inside <- st_within(points, eth_union, sparse = FALSE)[, 1]
    points_labelled <- points |> mutate(in_boundary = inside)

    p_overlay <- ggplot() +
      geom_sf(
        data = polys,
        fill = "steelblue",
        alpha = 0.3,
        col = "grey40",
        lwd = 0.3
      ) +
      geom_sf(
        data = points_labelled,
        aes(colour = in_boundary),
        size = 1.2,
        alpha = 0.7
      ) +
      scale_colour_manual(
        values = c("TRUE" = "forestgreen", "FALSE" = "firebrick"),
        labels = c("TRUE" = "Inside", "FALSE" = "Outside"),
        name = NULL
      ) +
      base_theme +
      labs(
        title = paste(level_name, "— polygons + points overlay"),
        subtitle = paste(nrow(polys), "polygons,", nrow(points), "points")
      )
    ggsave(
      file.path(out_dir, paste0(level_name, "_overlay.png")),
      plot = p_overlay,
      width = 8,
      height = 7,
      dpi = 150
    )

    # *---------------- Overlay: boundary-filtered points only ----------------*
    points_filtered <- points[inside, ]

    p_filtered <- ggplot() +
      geom_sf(
        data = polys,
        fill = "steelblue",
        alpha = 0.3,
        col = "grey40",
        lwd = 0.3
      ) +
      geom_sf(
        data = points_filtered,
        colour = "forestgreen",
        size = 1.2,
        alpha = 0.7
      ) +
      base_theme +
      labs(
        title = paste(level_name, "— polygons + filtered points"),
        subtitle = paste(
          nrow(points_filtered),
          "of",
          nrow(points),
          "points within Ethiopia boundary"
        )
      )
    ggsave(
      file.path(out_dir, paste0(level_name, "_filtered.png")),
      plot = p_filtered,
      width = 8,
      height = 7,
      dpi = 150
    )
  }

  cat("  Done\n")
}

cat("\nDone — outputs in", out_dir, "\n")

# *--------------------------- Master location file ---------------------------*
# For each feature across all levels: name, code, geometry type, and distance
# to the Ethiopia boundary. Distance == 0 means inside or on the border;
# distance > 0 is genuinely outside (value is metres).
cat("\n=== Building master location file ===\n")

master <- data.frame()

for (lvl in 1:6) {
  sf_obj <- st_read(
    file.path(shp_dir, paste0("ethiopia_dhis2_level", lvl, "_points.geojson")),
    quiet = TRUE
  ) |>
    st_make_valid()

  if (nrow(sf_obj) == 0) {
    next
  }

  geom_types <- st_geometry_type(sf_obj)

  # Compute distance to Ethiopia boundary for every feature.
  # st_distance returns 0 for features inside or touching the boundary.
  dist_m <- as.numeric(st_distance(sf_obj, eth_union))

  rows <- data.frame(
    level = lvl,
    name = sf_obj$name,
    code = sf_obj$code,
    geometry_type = as.character(geom_types),
    in_boundary = dist_m == 0,
    distance_m = round(dist_m, 1)
  )

  master <- rbind(master, rows)
  cat("  Level", lvl, "—", nrow(rows), "features\n")
}

cat("\nIn/out summary:\n")
print(table(master$level, master$in_boundary, dnn = c("level", "in_boundary")))

write.csv(
  master,
  file.path(out_dir, "dhis2_master_locations.csv"),
  row.names = FALSE
)
cat("\nSaved dhis2_master_locations.csv (", nrow(master), "rows )\n")

# *--------------- Combined GeoJSON with parent name & distance ---------------*
cat("\n=== Building combined GeoJSON ===\n")

sf_by_level <- list()

for (lvl in 1:6) {
  sf_obj <- st_read(
    file.path(shp_dir, paste0("ethiopia_dhis2_level", lvl, "_points.geojson")),
    quiet = TRUE,
    fid_column_name = "dhis2_id"
  ) |>
    st_make_valid()

  if (nrow(sf_obj) == 0) {
    next
  }
  sf_by_level[[lvl]] <- sf_obj
}

combined_sf <- do.call(rbind, sf_by_level)

id_name_lookup <- combined_sf |>
  st_drop_geometry() |>
  select(id, name)

combined_sf$parent_name <- id_name_lookup$name[match(
  combined_sf$parent,
  id_name_lookup$id
)]

combined_sf <- combined_sf |>
  mutate(
    distance_m = round(
      as.numeric(st_distance(st_centroid(combined_sf), eth_union)),
      3
    )
  )

list_cols <- names(which(sapply(st_drop_geometry(combined_sf), is.list)))
if (length(list_cols) > 0) {
  warning(
    "List columns found after rbind — collapsing to strings: ",
    paste(list_cols, collapse = ", ")
  )
  combined_sf[list_cols] <- lapply(
    st_drop_geometry(combined_sf)[list_cols],
    \(col) vapply(col, \(x) paste(x, collapse = ","), character(1))
  )
}

st_write(
  # Some GDAL versions read the feature id twice (id + id.1); drop the duplicate if present
  combined_sf |> select(-any_of("id.1")),
  file.path(shp_dir, "dhis2_combined.geojson"),
  delete_dsn = TRUE,
  quiet = TRUE
)
cat("Saved dhis2_combined.geojson (", nrow(combined_sf), "features)\n")

# *============================================================================*
