# *========================= Explore OCHA Shapefiles ==========================*
# Iterates over each OCHA admin shapefile and produces:
#   - polygons-only plot
#   - lines-only plot (where applicable)
#   - combo plot (polygons + lines together)
# *============================================================================*

library(ggplot2)
library(sf)
library(dplyr)
library(stringr)

shp_dir <- here::here("data", "shapefiles", "OCHA")
out_dir <- here::here("src", "R", "outputs", "data_quality", "not_used", "shapefiles", "explore_shapefiles", "OCHA")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ocha_files <- c(
  "eth_admin0" = "eth_admin0.shp",
  "eth_admin1" = "eth_admin1.shp",
  "eth_admin2" = "eth_admin2.shp",
  "eth_admin3" = "eth_admin3.shp",
  "eth_adminpoints" = "eth_adminpoints.shp",
  "eth_admincapitals" = "eth_admincapitals.shp",
  "eth_adminlines" = "eth_adminlines.shp"
)

base_theme <- list(
  theme_minimal(),
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 8),
    panel.grid = element_line(colour = "grey90")
  ),
  labs(x = "Longitude", y = "Latitude")
)

loaded <- list()

for (label in names(ocha_files)) {
  f <- file.path(shp_dir, ocha_files[[label]])
  cat("\n---", label, "---\n")

  sf_obj <- st_read(f, quiet = TRUE) |> st_make_valid()
  loaded[[label]] <- sf_obj

  if (nrow(sf_obj) == 0) {
    cat("  EMPTY — skipping\n")
    next
  }

  geom_types <- st_geometry_type(sf_obj)
  cat("  Features:", nrow(sf_obj), "\n")
  cat(
    "  Columns: ",
    paste(setdiff(names(sf_obj), "geometry"), collapse = ", "),
    "\n"
  )
  print(table(geom_types))

  polys <- sf_obj |> filter(geom_types %in% c("POLYGON", "MULTIPOLYGON"))
  points <- sf_obj |> filter(geom_types == "POINT")
  lines <- sf_obj |> filter(geom_types %in% c("LINESTRING", "MULTILINESTRING"))

  # *-------------------------------- Polygons --------------------------------*
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
        title = paste(label, "— polygons"),
        subtitle = paste(nrow(polys), "features")
      )
    ggsave(
      file.path(out_dir, paste0(label, "_polys.png")),
      plot = p_poly,
      width = 8,
      height = 7,
      dpi = 150
    )
  }

  # *--------------------------------- Points ---------------------------------*
  if (nrow(points) > 0) {
    p_pts <- ggplot() +
      geom_sf(data = points, colour = "steelblue", size = 2, alpha = 0.8) +
      base_theme +
      labs(
        title = paste(label, "— points"),
        subtitle = paste(nrow(points), "features")
      )
    ggsave(
      file.path(out_dir, paste0(label, "_points.png")),
      plot = p_pts,
      width = 8,
      height = 7,
      dpi = 150
    )
  }

  # *--------------------------------- Lines ----------------------------------*
  if (nrow(lines) > 0) {
    p_lines <- ggplot() +
      geom_sf(data = lines, col = "steelblue", lwd = 0.5) +
      base_theme +
      labs(
        title = paste(label, "— lines"),
        subtitle = paste(nrow(lines), "features")
      )
    ggsave(
      file.path(out_dir, paste0(label, "_lines.png")),
      plot = p_lines,
      width = 8,
      height = 7,
      dpi = 150
    )
  }

  # *------------------- Combo: all geometry types together -------------------*
  if (nrow(polys) + nrow(lines) + nrow(points) > 0) {
    p_combo <- ggplot()
    if (nrow(polys) > 0) {
      p_combo <- p_combo +
        geom_sf(
          data = polys,
          fill = "steelblue",
          alpha = 0.3,
          col = "grey40",
          lwd = 0.3
        )
    }
    if (nrow(lines) > 0) {
      p_combo <- p_combo +
        geom_sf(data = lines, col = "black", lwd = 0.5)
    }
    if (nrow(points) > 0) {
      p_combo <- p_combo +
        geom_sf(data = points, colour = "firebrick", size = 2, alpha = 0.8)
    }
    p_combo <- p_combo +
      base_theme +
      labs(
        title = paste(label, "— all geometry types"),
        subtitle = paste(
          nrow(polys),
          "polygons,",
          nrow(lines),
          "lines,",
          nrow(points),
          "points"
        )
      )
    ggsave(
      file.path(out_dir, paste0(label, "_combo.png")),
      plot = p_combo,
      width = 8,
      height = 7,
      dpi = 150
    )
  }

  cat("  Done\n")
}

cat("\nDone — outputs in", out_dir, "\n")

# *- Do adminpoints / adminlines / admincapitals overlap with polygon files? --*
normalise <- function(x) {
  x |>
    str_trim() |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9 ]", " ") |>
    str_squish()
}

poly_names_norm <- c(
  normalise(loaded$eth_admin1$adm1_name),
  normalise(loaded$eth_admin2$adm2_name),
  normalise(loaded$eth_admin3$adm3_name)
)

for (extra in c("eth_adminpoints", "eth_adminlines", "eth_admincapitals")) {
  nm <- loaded[[extra]]$name
  overlap <- nm[normalise(nm) %in% poly_names_norm]
  pct <- round(100 * length(overlap) / length(nm), 1)
  cat(
    "\n", extra, "— overlap with polygon files:",
    length(overlap),
    "/",
    length(nm),
    paste0("(", pct, "%)\n")
  )
  if (length(overlap) > 0 && length(overlap) <= 20) print(overlap)
}

# *============================================================================*
