# *======================== GMP NUT coverage analysis =========================*
# Runs the same analysis for two coverage datasets:
#   (1) idb_gmp_nut.csv           — NUT / IDB area population denominator
#   (2) gmp_nut_coverage_idb.csv  — NUT / (WBP × IDB proportion) denominator
#
# For each dataset produces in
# src/R/outputs/RQ2_RQ3_national_level_and_geographic_variation/not_used/
# analyse_idb_gmp_nut/<label>/ (label = idb_gmp_nut or dhis2_idb):
#   coverage_summary.csv   — per-region overall coverage rates
#   region_growth.csv      — first/last year + absolute growth
#   map_coverage_regional.png
#   timeseries_coverage.png
# *============================================================================*

# *--------------------------------- imports ----------------------------------*
library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(scales)

source(here::here("src", "R", "plotting_helpers.R"))

# *---------------------------------- paths -----------------------------------*
idb_nut_csv <- here::here("src", "R", "outputs", "data_preparation", "not_used", "gmp_coverage", "idb_gmp_nut.csv")
dhis2_cov_csv <- here::here(
  "src",
  "R",
  "outputs",
  "data_preparation",
  "not_used",
  "gmp_coverage",
  "gmp_nut_coverage_idb.csv"
)
spatial_csv <- here::here(
  "src",
  "R",
  "outputs",
  "data_preparation",
  "not_used",
  "generate_dhis2_gmp_shapes",
  "gmp_spatial.csv"
)
ocha_dir <- here::here("data", "shapefiles", "OCHA")
base_out_dir <- here::here("src", "R", "outputs", "RQ2_RQ3_national_level_and_geographic_variation", "not_used", "analyse_idb_gmp_nut")

ocha_admin1 <- st_read(file.path(ocha_dir, "eth_admin1.shp"), quiet = TRUE) |>
  st_make_valid() |>
  st_transform(4326)

# *------------------------------ shared helpers ------------------------------*
safe_coverage <- function(nut, denom) {
  ifelse(is.na(denom) | denom == 0, NA_real_, round(100 * nut / denom, 2))
}

FILL_COLOURS <- c("#DC2626", "#FDE047", "#86EFAC", "#15803D")
FILL_VALUES <- c(0, 0.74, 0.75, 1)

x_start <- 2015

run_analysis <- function(regional_annual, label, ocha_sf) {
  # regional_annual: (area, year, nut, under2_est) — one row per (area, year),
  # area must match ocha_sf$adm1_name.
  out_dir <- file.path(base_out_dir, label)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  regional_by_year <- regional_annual |>
    mutate(
      under2_est = na_if(under2_est, 0),
      coverage_pct = safe_coverage(nut, under2_est)
    )

  overall_by_year <- regional_annual |>
    group_by(year) |>
    summarise(
      nut = sum(nut, na.rm = TRUE),
      under2_est = sum(under2_est, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      under2_est = na_if(under2_est, 0),
      coverage_pct = safe_coverage(nut, under2_est),
      area = "Overall"
    )

  overall_total <- regional_annual |>
    filter(!is.na(under2_est)) |>
    group_by(area) |>
    summarise(
      nut = sum(nut, na.rm = TRUE),
      under2_est = sum(under2_est, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      under2_est = na_if(under2_est, 0),
      coverage_pct = safe_coverage(nut, under2_est)
    ) |>
    arrange(coverage_pct)

  write_csv(overall_total, file.path(out_dir, "coverage_summary.csv"))
  cat("\n[", label, "] Coverage summary:\n")
  print(as.data.frame(overall_total), row.names = FALSE)

  region_growth <- regional_by_year |>
    filter(!is.na(coverage_pct), is.finite(coverage_pct)) |>
    group_by(area) |>
    summarise(
      first_year = year[which.min(year)],
      last_year = year[which.max(year)],
      coverage_first = coverage_pct[which.min(year)],
      coverage_last = coverage_pct[which.max(year)],
      .groups = "drop"
    ) |>
    mutate(growth = round(coverage_last - coverage_first, 2)) |>
    arrange(desc(growth))

  write_csv(region_growth, file.path(out_dir, "region_growth.csv"))
  cat("Lowest coverage:\n")
  print(slice_min(overall_total, coverage_pct, n = 3))
  cat("Biggest growth:\n")
  print(slice_max(region_growth, growth, n = 3))
  cat("Least growth:\n")
  print(slice_min(region_growth, growth, n = 3))

  # *---------------------------------- map -----------------------------------*
  ocha_cov <- ocha_sf |> left_join(overall_total, by = c("adm1_name" = "area"))

  unmapped <- ocha_cov |>
    st_drop_geometry() |>
    filter(is.na(coverage_pct)) |>
    pull(adm1_name)
  if (length(unmapped) > 0) {
    cat("Grey regions (no data):", paste(unmapped, collapse = ", "), "\n")
  }

  nat_cov <- round(
    100 *
      sum(overall_total$nut, na.rm = TRUE) /
      sum(overall_total$under2_est, na.rm = TRUE),
    1
  )
  inset <- paste0(
    "Regions with data: ",
    sum(!is.na(ocha_cov$coverage_pct)),
    " / ",
    nrow(ocha_sf),
    "\nNational coverage: ",
    nat_cov,
    "%"
  )

  map_plot <- ggplot() +
    geom_sf(
      data = ocha_cov,
      aes(fill = coverage_pct),
      colour = "grey70",
      lwd = 0.2
    ) +
    geom_sf(data = ocha_sf, fill = NA, colour = "black", lwd = 0.4) +
    scale_fill_gradientn(
      colours = FILL_COLOURS,
      values = FILL_VALUES,
      na.value = "grey92",
      name = "Coverage %",
      limits = c(0, 100),
      oob = squish,
      labels = \(x) paste0(x, "%")
    ) +
    annotate(
      "label",
      x = Inf,
      y = -Inf,
      hjust = 1.05,
      vjust = -0.15,
      label = inset,
      size = 3,
      fill = "white",
      alpha = 0.85
    ) +
    labs(
      title = paste("GMP coverage by region —", label),
      subtitle = "NUT children weighed / estimated under-2 population"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 13,
        margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        size = 10,
        colour = "grey40",
        margin = margin(b = 8)
      ),
      plot.margin = margin(10, 10, 10, 10),
      legend.position = "right"
    )

  ggsave(
    file.path(out_dir, "map_coverage_regional.png"),
    map_plot,
    width = 10,
    height = 9,
    dpi = 200
  )
  cat("Saved map_coverage_regional.png\n")

  # *------------------------------ time series -------------------------------*
  ts_data <- bind_rows(
    regional_by_year |> select(area, year, coverage_pct),
    overall_by_year |> select(area, year, coverage_pct)
  ) |>
    mutate(is_overall = area == "Overall")

  x_end <- max(ts_data$year[
    !is.na(ts_data$coverage_pct) &
      is.finite(ts_data$coverage_pct)
  ])
  x_breaks <- seq(x_start, x_end, by = 1)

  region_names <- sort(unique(ts_data$area[!ts_data$is_overall]))
  region_colours <- setNames(hue_pal()(length(region_names)), region_names)
  legend_breaks <- c(region_names, " ", "Overall")
  all_colours <- c(region_colours, " " = NA, "Overall" = "black")

  ts_plot <- ggplot(
    ts_data,
    aes(
      x = year,
      y = coverage_pct,
      colour = area,
      group = area,
      linetype = is_overall,
      linewidth = is_overall
    )
  ) +
    geom_line(alpha = 0.85) +
    geom_point(data = ts_data |> filter(is_overall), size = 2.5) +
    scale_colour_manual(
      values = all_colours,
      breaks = legend_breaks,
      labels = legend_breaks,
      na.value = NA,
      name = NULL
    ) +
    scale_linetype_manual(
      values = c("FALSE" = "solid", "TRUE" = "dashed"),
      guide = "none"
    ) +
    scale_linewidth_manual(
      values = c("FALSE" = 0.7, "TRUE" = 1.4),
      guide = "none"
    ) +
    scale_x_continuous(
      limits = c(x_start, x_end),
      breaks = x_breaks,
      labels = as.character(x_breaks)
    ) +
    scale_y_continuous(labels = \(x) paste0(x, "%")) +
    labs(
      title = paste("GMP coverage over time —", label),
      x = "Year",
      y = "Coverage %"
    )

  ts_plot <- format_gg_plot(ts_plot, cpal = NULL) +
    theme(
      legend.position = "right",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  ggsave(
    file.path(out_dir, "timeseries_coverage.png"),
    ts_plot,
    width = 14,
    height = 7,
    dpi = 150
  )
  cat("Saved timeseries_coverage.png\n")
}

# *--------------------- (1) IDB GMP NUT: idb_gmp_nut.csv ---------------------*
idb_nut <- read_csv(idb_nut_csv, show_col_types = FALSE) |>
  mutate(year = as.integer(year)) |>
  filter(adm_level == 1) |>
  group_by(area, year) |>
  summarise(
    nut = sum(nut, na.rm = TRUE),
    under2_est = sum(under2_est, na.rm = TRUE),
    .groups = "drop"
  )

run_analysis(idb_nut, "idb_gmp_nut", ocha_admin1)

# *------------------- (2) DHIS2 IDB: gmp_nut_coverage.csv --------------------*
# Join DHIS2 level-2 org units to OCHA adm1_name via gmp_spatial.
dhis2_to_ocha <- read_csv(spatial_csv, show_col_types = FALSE) |>
  filter(org_unit_level == 2, !is.na(adm1_name), adm1_name != "NA") |>
  distinct(org_unit_name, adm1_name)

# Facet by urban/rural
dhis2_idb <- read_csv(dhis2_cov_csv, show_col_types = FALSE) |>
  filter(org_unit_level == 2) |>
  mutate(year = as.integer(year), under2_est = as.numeric(idb_prop_under2_est)) |>
  left_join(dhis2_to_ocha, by = "org_unit_name") |>
  filter(!is.na(adm1_name), year < 2019) |>
  group_by(area = adm1_name, year) |>
  summarise(
    nut = sum(nut, na.rm = TRUE),
    under2_est = sum(under2_est, na.rm = TRUE),
    .groups = "drop"
  )

run_analysis(dhis2_idb, "dhis2_idb", ocha_admin1)

# *============================================================================*
