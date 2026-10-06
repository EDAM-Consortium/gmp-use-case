# *========================= Explore ESPEN Shapefile ==========================*
# The ESPEN IU shapefile contains Implementation Unit (woreda-level) polygons
# for NTD programme planning. Attributes are admin identifiers and geometry
# stats only-no disease values are stored here.
#
# Plots three tiers after filtering to Ethiopia:
#   - ADMIN1 (regional) dissolved polygons
#   - ADMIN2 (zonal) dissolved polygons
#   - IU-level (ADMIN3) individual polygons
# *============================================================================*

library(ggplot2)
library(dplyr)
library(sf)

shp_dir <- here::here("data", "shapefiles", "ESPEN")
out_dir <- here::here("src", "R", "outputs", "data_quality", "not_used", "shapefiles", "explore_shapefiles", "ESPEN")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# *----------------------- Load and filter to Ethiopia ------------------------*
espen_global <- st_read(
  file.path(shp_dir, "ESPEN_IU_2024.shp"),
  quiet = TRUE
) |>
  st_make_valid()

cat("Global rows:", nrow(espen_global), "\n")

espen <- espen_global[espen_global$ADMIN0 == "Ethiopia", ]

cat("Ethiopia rows:", nrow(espen), "\n")
cat("Geometry types:\n")
print(table(st_geometry_type(espen)))

# *---------------------------- Attribute summary -----------------------------*
cat("\n=== Attribute summary ===\n")
cat(
  "Columns:",
  paste(setdiff(names(espen), "geometry"), collapse = ", "),
  "\n\n"
)

cat("Regions (ADMIN1):\n")
print(sort(unique(espen$ADMIN1)))

cat("\nZones (ADMIN2)-first 20:\n")
print(head(sort(unique(espen$ADMIN2)), 20))

cat("\nIU admin level breakdown:\n")
print(table(espen$IUs_ADM))

cat("\nArea range (Shape_Area):", round(range(espen$Shape_Area), 4), "\n")

cat("\nLast update values:\n")
print(table(espen$LASTUPDATE))

# *------------------------------ Dissolve tiers ------------------------------*
espen_admin1 <- espen |>
  group_by(ADMIN1) |>
  summarise(.groups = "drop")

espen_admin2 <- espen |>
  group_by(ADMIN1, ADMIN2) |>
  summarise(.groups = "drop")

cat("\nAdmin1 dissolved regions:", nrow(espen_admin1), "\n")
cat("Admin2 dissolved zones:  ", nrow(espen_admin2), "\n")
cat("IU-level polygons:       ", nrow(espen), "\n")

# *-------------------------------- Base theme --------------------------------*
base_theme <- list(
  theme_minimal(),
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 8),
    panel.grid = element_line(colour = "grey90"),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.4, "cm")
  ),
  labs(x = "Longitude", y = "Latitude")
)

# *-------------------- Tier 1: ADMIN1 (regional) polygons --------------------*
p_admin1 <- ggplot() +
  geom_sf(
    data = espen_admin1,
    aes(fill = ADMIN1),
    col = "white",
    lwd = 0.4,
    alpha = 0.8
  ) +
  base_theme +
  labs(
    title = "ESPEN-ADMIN1 (regional) dissolved polygons",
    subtitle = paste(nrow(espen_admin1), "regions"),
    fill = "Region"
  )
ggsave(
  file.path(out_dir, "espen_admin1.png"),
  plot = p_admin1,
  width = 9,
  height = 9,
  dpi = 150
)

# *--------------------- Tier 2: ADMIN2 (zonal) polygons ----------------------*
p_admin2 <- ggplot() +
  geom_sf(
    data = espen_admin2,
    aes(fill = ADMIN1),
    col = "white",
    lwd = 0.2,
    alpha = 0.8
  ) +
  base_theme +
  labs(
    title = "ESPEN-ADMIN2 (zonal) dissolved polygons",
    subtitle = paste(nrow(espen_admin2), "zones, coloured by region"),
    fill = "Region"
  )
ggsave(
  file.path(out_dir, "espen_admin2.png"),
  plot = p_admin2,
  width = 9,
  height = 9,
  dpi = 150
)

# *------------------------ Tier 3: IU-level polygons -------------------------*
p_iu <- ggplot() +
  geom_sf(
    data = espen,
    aes(fill = ADMIN1),
    col = "white",
    lwd = 0.1,
    alpha = 0.8
  ) +
  base_theme +
  labs(
    title = "ESPEN-IU-level polygons",
    subtitle = paste(nrow(espen), "IUs, coloured by region"),
    fill = "Region"
  )
ggsave(
  file.path(out_dir, "espen_iu.png"),
  plot = p_iu,
  width = 9,
  height = 9,
  dpi = 150
)

# *-------------- Tier 3 plain (no fill) for boundary inspection --------------*
p_iu_plain <- ggplot() +
  geom_sf(
    data = espen,
    fill = "steelblue",
    alpha = 0.4,
    col = "black",
    lwd = 0.1
  ) +
  base_theme +
  labs(
    title = "ESPEN-IU-level polygons (plain)",
    subtitle = paste(nrow(espen), "IUs")
  )
ggsave(
  file.path(out_dir, "espen_iu_plain.png"),
  plot = p_iu_plain,
  width = 9,
  height = 9,
  dpi = 150
)

cat("\nDone-outputs in", out_dir, "\n")

# *============================================================================*
