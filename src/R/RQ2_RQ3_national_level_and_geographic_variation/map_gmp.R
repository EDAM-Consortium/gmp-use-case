# *================================= GMP Map ==================================*
# Choropleth of GMP children-weighed values joined to OCHA admin boundaries.
# Produces maps at region (adm1), zone (adm2), and woreda (adm3) level.
#
# year/month in gmp_spatial are Ethiopian calendar values. These are converted
# to Gregorian: ETH months 1–4 → greg_year = eth_year + 7, months 5–13 → +8.
#
# Outputs in src/R/outputs/RQ2_RQ3_national_level_and_geographic_variation/:
#   not_used/map_gmp/
#     gmp_map_zonal_by_year.png     — 4×2 facet panel 2018–2025
#     gmp_map_zonal_2025.png        — single map for Gregorian 2025
#   used/map_gmp/ (slide figures, each with a CSV of its plotted data)
#     gmp_map_<level>_by_year.png   — as above, for level = regional, woreda and
#     gmp_map_<level>_2025.png        zonal_woreda_agg (woreda data on zone boundaries)
# *============================================================================*

library(dplyr)
library(ggplot2)
library(readr)
library(sf)
library(scales)

source(here::here("src", "R", "plotting_helpers.R"))

# *---------------------------------- Config ----------------------------------*
YEAR_MIN <- 2018L
YEAR_MAX <- 2025L
ZOOM_YEAR <- 2025L
FACET_NCOLS <- 4L # 4 columns → 2 rows for 8 years

ADMIN_LEVELS <- c(1L, 2L, 3L)
ADMIN_LEVEL_LABELS <- c(`1` = "regional", `2` = "zonal", `3` = "woreda")

ocha_dir <- here::here("data", "shapefiles", "OCHA")
out_dir <- here::here("src", "R", "outputs", "RQ2_RQ3_national_level_and_geographic_variation", "not_used", "map_gmp")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Regional and woreda maps (and the woreda-aggregated zonal maps) are used in
# the slides; the zonal maps are not.
USED_LEVELS <- c("regional", "woreda")

save_map <- function(p, filename, data, lvl, used, ...) {
  if (!used) {
    ggsave(file.path(out_dir, filename), p, ...)
    return(invisible())
  }
  cols <- c(paste0("adm", lvl, "_name"), paste0("adm", lvl, "_pcode"))
  if ("gregorian_year" %in% names(data)) cols <- c(cols, "gregorian_year")
  save_used_figure(
    p,
    out_dir,
    filename,
    cols = c(cols, "total_value"),
    data = data,
    ...
  )
}

# *--------------------------- Load OCHA boundaries ---------------------------*
ocha_admin1 <- st_read(file.path(ocha_dir, "eth_admin1.shp"), quiet = TRUE) |>
  st_make_valid() |>
  st_transform(4326)
ocha_admin2 <- st_read(file.path(ocha_dir, "eth_admin2.shp"), quiet = TRUE) |>
  st_make_valid() |>
  st_transform(4326)
ocha_admin3 <- st_read(file.path(ocha_dir, "eth_admin3.shp"), quiet = TRUE) |>
  st_make_valid() |>
  st_transform(4326)

ocha_layers <- list(
  `1` = list(sf = ocha_admin1, pcode = "adm1_pcode"),
  `2` = list(sf = ocha_admin2, pcode = "adm2_pcode"),
  `3` = list(sf = ocha_admin3, pcode = "adm3_pcode")
)

# *----------------------------- Load GMP spatial -----------------------------*
gmp_all <- read_csv(
  here::here("src", "R", "outputs", "data_preparation", "not_used", "generate_dhis2_gmp_shapes", "gmp_spatial.csv"),
  show_col_types = FALSE
) |>
  filter(between(year, 2000L, 2030L), between(month, 1L, 13L)) |>
  mutate(
    gregorian_year = as.integer(year) +
      if_else(as.integer(month) <= 4L, 7L, 8L)
  ) |>
  filter(gregorian_year >= YEAR_MIN, gregorian_year <= YEAR_MAX)

# *------------------------------- Colour scale -------------------------------*
fill_scale <- scale_fill_distiller(
  palette = "RdYlGn",
  direction = 1,
  na.value = "grey92",
  name = "Children\nweighed\nper month",
  labels = comma,
  limits = c(0, NA),
  oob = squish
)

# *--------------------------------- Map loop ---------------------------------*
for (lvl in ADMIN_LEVELS) {
  layer <- ocha_layers[[as.character(lvl)]]
  ocha_sf <- layer$sf
  pcode_col <- layer$pcode
  gmp_pcode <- paste0("adm", lvl, "_pcode")
  level_label <- tolower(ADMIN_LEVEL_LABELS[lvl])

  gmp_lvl_all <- gmp_all |>
    filter(org_unit_level == lvl + 1)

  gmp_lvl <- gmp_lvl_all |>
    filter(!is.na(.data[[gmp_pcode]]))

  cat(
    "\nLevel",
    lvl,
    "(",
    ADMIN_LEVEL_LABELS[lvl],
    "):",
    nrow(gmp_lvl),
    "rows\n"
  )

  if (nrow(gmp_lvl) == 0) {
    cat("  No data — skipping\n")
    next
  }

  years <- sort(unique(gmp_lvl$gregorian_year))

  # *----------------------------- 4×2 Facet map ------------------------------*
  gmp_agg_yr <- gmp_lvl |>
    group_by(.data[[gmp_pcode]], gregorian_year) |>
    summarise(total_value = mean(value, na.rm = TRUE), .groups = "drop") |>
    rename(!!pcode_col := 1)

  ocha_yr <- cross_join(ocha_sf, tibble(gregorian_year = years)) |>
    left_join(gmp_agg_yr, by = c(pcode_col, "gregorian_year"))

  # Single averaged inset across all year panels
  # nut_monthly_avg = national avg children weighed per month (sum / n months)
  # Sum all units per month, then average those monthly totals per year.
  # This matches shown_avg (sum of per-unit monthly means ≈ national monthly total).
  total_nut_yr <- gmp_lvl_all |>
    group_by(gregorian_year, date) |>
    summarise(monthly_total = sum(value, na.rm = TRUE), .groups = "drop") |>
    group_by(gregorian_year) |>
    summarise(
      nut_monthly_avg = mean(monthly_total, na.rm = TRUE),
      .groups = "drop"
    )

  # Numerator: monthly total for OCHA-matched units only
  shown_monthly_yr <- gmp_lvl |>
    group_by(gregorian_year, date) |>
    summarise(monthly_total = sum(value, na.rm = TRUE), .groups = "drop") |>
    group_by(gregorian_year) |>
    summarise(
      shown_monthly_avg = mean(monthly_total, na.rm = TRUE),
      .groups = "drop"
    )

  facet_avg <- ocha_yr |>
    st_drop_geometry() |>
    group_by(gregorian_year) |>
    summarise(
      n_with_data = sum(!is.na(total_value)),
      n_total = n(),
      avg_per_unit = mean(total_value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    left_join(shown_monthly_yr, by = "gregorian_year") |>
    left_join(total_nut_yr, by = "gregorian_year") |>
    summarise(
      avg_units = round(mean(n_with_data)),
      n_total = first(n_total),
      avg_monthly = round(mean(avg_per_unit, na.rm = TRUE)),
      avg_pct = round(mean(
        100 * shown_monthly_avg / nut_monthly_avg,
        na.rm = TRUE
      ))
    )

  facet_caption <- paste0(
    "Annual averages (",
    YEAR_MIN,
    "–",
    YEAR_MAX,
    "): ",
    facet_avg$avg_units,
    "/",
    facet_avg$n_total,
    " units with data  ·  ",
    format(facet_avg$avg_monthly, big.mark = ","),
    " avg children weighed/month per unit  ·",
    facet_avg$avg_pct,
    "% of NUT total"
  )

  n_rows <- ceiling(length(years) / FACET_NCOLS)
  panel_w <- 3.5

  p_facet <- ggplot() +
    geom_sf(
      data = ocha_yr,
      aes(fill = total_value),
      colour = "grey70",
      lwd = 0.05
    ) +
    geom_sf(data = ocha_admin1, fill = NA, colour = "black", lwd = 0.35) +
    fill_scale +
    facet_wrap(~gregorian_year, ncol = FACET_NCOLS) +
    labs(
      title = paste0(
        "Children <2 years weighed by Gregorian year (",
        ADMIN_LEVEL_LABELS[lvl],
        ")"
      ),
      caption = facet_caption
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
      plot.caption = element_text(
        size = 11,
        colour = "grey40",
        margin = margin(t = 6)
      ),
      plot.margin = margin(10, 10, 10, 10),
      strip.text = element_text(
        size = 9,
        face = "bold",
        margin = margin(b = 4)
      ),
      legend.position = "right"
    )

  facet_file <- paste0("gmp_map_", level_label, "_by_year.png")
  save_map(
    p_facet,
    facet_file,
    data = ocha_yr,
    lvl = lvl,
    used = level_label %in% USED_LEVELS,
    width = panel_w * FACET_NCOLS + 1.5,
    height = panel_w * n_rows + 1.5,
    dpi = 200,
    limitsize = FALSE
  )
  cat("  Saved", facet_file, "\n")

  # *---------------------------- Single map: 2025 ----------------------------*
  gmp_agg_zoom <- gmp_lvl |>
    filter(gregorian_year == ZOOM_YEAR) |>
    group_by(.data[[gmp_pcode]]) |>
    summarise(total_value = mean(value, na.rm = TRUE), .groups = "drop") |>
    rename(!!pcode_col := 1)

  if (nrow(gmp_agg_zoom) == 0) {
    cat("  No data for Gregorian", ZOOM_YEAR, "— skipping single-year map\n")
    next
  }

  ocha_plot_zoom <- ocha_sf |>
    left_join(gmp_agg_zoom, by = pcode_col)

  zoom_lvl_all <- gmp_lvl_all |> filter(gregorian_year == ZOOM_YEAR)
  nut_monthly_zoom <- zoom_lvl_all |>
    group_by(date) |>
    summarise(t = sum(value, na.rm = TRUE), .groups = "drop") |>
    pull(t) |>
    mean(na.rm = TRUE)
  # Numerator: matched units only
  shown_monthly_zoom <- gmp_lvl |>
    filter(gregorian_year == ZOOM_YEAR) |>
    group_by(date) |>
    summarise(t = sum(value, na.rm = TRUE), .groups = "drop") |>
    pull(t) |>
    mean(na.rm = TRUE)
  pct_shown_zoom <- round(100 * shown_monthly_zoom / nut_monthly_zoom)
  n_with_data_zoom <- sum(!is.na(ocha_plot_zoom$total_value))

  inset_text_zoom <- paste0(
    "Admin units with data: ",
    n_with_data_zoom,
    " / ",
    nrow(ocha_sf),
    "\nAvg / month per unit: ",
    format(
      round(mean(ocha_plot_zoom$total_value, na.rm = TRUE)),
      big.mark = ","
    ),
    "\n% of NUT total: ",
    pct_shown_zoom,
    "%"
  )

  p_zoom <- ggplot() +
    geom_sf(
      data = ocha_plot_zoom,
      aes(fill = total_value),
      colour = "grey70",
      lwd = 0.1
    ) +
    geom_sf(data = ocha_admin1, fill = NA, colour = "black", lwd = 0.45) +
    fill_scale +
    annotate(
      "label",
      x = Inf,
      y = -Inf,
      hjust = 1.05,
      vjust = -0.15,
      label = inset_text_zoom,
      size = 3.5,
      fill = "white",
      alpha = 0.85
    ) +
    labs(
      title = paste0(
        "Children <2 years weighed by Gregorian year (",
        ADMIN_LEVEL_LABELS[lvl],
        ")"
      ),
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

  zoom_file <- paste0("gmp_map_", level_label, "_", ZOOM_YEAR, ".png")
  save_map(
    p_zoom,
    zoom_file,
    data = ocha_plot_zoom,
    lvl = lvl,
    used = level_label %in% USED_LEVELS,
    width = 10,
    height = 9,
    dpi = 200
  )
  cat("  Saved", zoom_file, "\n")
}

# *----------- Zonal map: woreda data aggregated to zone boundaries -----------*
# Uses org_unit_level == 4 (woreda) rows summed by adm2_pcode, so the colour
# reflects bottom-up woreda coverage rather than zone-level reporting.

gmp_wor <- gmp_all |>
  filter(org_unit_level == 4, !is.na(adm2_pcode))

gmp_wor_all <- gmp_all |>
  filter(org_unit_level == 4)

cat("\nZonal (woreda-aggregated):", nrow(gmp_wor), "rows\n")

if (nrow(gmp_wor) > 0) {
  years_wor <- sort(unique(gmp_wor$gregorian_year))

  # *----------------------------- 4×2 Facet map ------------------------------*
  gmp_agg_wor_yr <- gmp_wor |>
    group_by(adm2_pcode, gregorian_year) |>
    summarise(total_value = mean(value, na.rm = TRUE), .groups = "drop")

  ocha_wor_yr <- cross_join(ocha_admin2, tibble(gregorian_year = years_wor)) |>
    left_join(gmp_agg_wor_yr, by = c("adm2_pcode", "gregorian_year"))

  total_nut_wor_yr <- gmp_wor_all |>
    group_by(gregorian_year, date) |>
    summarise(monthly_total = sum(value, na.rm = TRUE), .groups = "drop") |>
    group_by(gregorian_year) |>
    summarise(
      nut_monthly_avg = mean(monthly_total, na.rm = TRUE),
      .groups = "drop"
    )

  shown_monthly_wor_yr <- gmp_wor |>
    group_by(gregorian_year, date) |>
    summarise(monthly_total = sum(value, na.rm = TRUE), .groups = "drop") |>
    group_by(gregorian_year) |>
    summarise(
      shown_monthly_avg = mean(monthly_total, na.rm = TRUE),
      .groups = "drop"
    )

  wor_avg <- ocha_wor_yr |>
    st_drop_geometry() |>
    group_by(gregorian_year) |>
    summarise(
      n_with_data = sum(!is.na(total_value)),
      n_total = n(),
      avg_per_unit = mean(total_value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    left_join(shown_monthly_wor_yr, by = "gregorian_year") |>
    left_join(total_nut_wor_yr, by = "gregorian_year") |>
    summarise(
      avg_units = round(mean(n_with_data)),
      n_total = first(n_total),
      avg_monthly = round(mean(avg_per_unit, na.rm = TRUE)),
      avg_pct = round(mean(
        100 * shown_monthly_avg / nut_monthly_avg,
        na.rm = TRUE
      ))
    )

  wor_caption <- paste0(
    "Annual averages (",
    YEAR_MIN,
    "–",
    YEAR_MAX,
    "): ",
    wor_avg$avg_units,
    "/",
    wor_avg$n_total,
    " zones with data  ·  ",
    format(wor_avg$avg_monthly, big.mark = ","),
    " children weighed/month on average ·  ",
    wor_avg$avg_pct,
    "% of NUT total"
  )

  n_rows_wor <- ceiling(length(years_wor) / FACET_NCOLS)
  panel_w <- 3.5

  p_wor_facet <- ggplot() +
    geom_sf(
      data = ocha_wor_yr,
      aes(fill = total_value),
      colour = "grey70",
      lwd = 0.05
    ) +
    geom_sf(data = ocha_admin1, fill = NA, colour = "black", lwd = 0.35) +
    fill_scale +
    facet_wrap(~gregorian_year, ncol = FACET_NCOLS) +
    labs(
      title = "Children <2 years weighed by Gregorian year (zonal - woreda data aggregated)",
      caption = wor_caption
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
      plot.caption = element_text(
        size = 11,
        colour = "grey40",
        margin = margin(t = 6)
      ),
      plot.margin = margin(10, 10, 10, 10),
      strip.text = element_text(
        size = 9,
        face = "bold",
        margin = margin(b = 4)
      ),
      legend.position = "right"
    )

  save_map(
    p_wor_facet,
    "gmp_map_zonal_woreda_agg_by_year.png",
    data = ocha_wor_yr,
    lvl = 2,
    used = TRUE,
    width = panel_w * FACET_NCOLS + 1.5,
    height = panel_w * n_rows_wor + 1.5,
    dpi = 200,
    limitsize = FALSE
  )
  cat("  Saved gmp_map_zonal_woreda_agg_by_year.png\n")

  # *---------------------------- Single map: 2025 ----------------------------*
  gmp_agg_wor_zoom <- gmp_wor |>
    filter(gregorian_year == ZOOM_YEAR) |>
    group_by(adm2_pcode) |>
    summarise(total_value = mean(value, na.rm = TRUE), .groups = "drop")

  if (nrow(gmp_agg_wor_zoom) == 0) {
    cat(
      "  No woreda data for Gregorian",
      ZOOM_YEAR,
      "— skipping single-year map\n"
    )
  } else {
    ocha_wor_zoom <- ocha_admin2 |>
      left_join(gmp_agg_wor_zoom, by = "adm2_pcode")

    wor_zoom_all <- gmp_wor_all |> filter(gregorian_year == ZOOM_YEAR)
    nut_monthly_wor_zoom <- wor_zoom_all |>
      group_by(date) |>
      summarise(t = sum(value, na.rm = TRUE), .groups = "drop") |>
      pull(t) |>
      mean(na.rm = TRUE)
    shown_monthly_wor_zoom <- gmp_wor |>
      filter(gregorian_year == ZOOM_YEAR) |>
      group_by(date) |>
      summarise(t = sum(value, na.rm = TRUE), .groups = "drop") |>
      pull(t) |>
      mean(na.rm = TRUE)
    pct_wor_zoom <- round(100 * shown_monthly_wor_zoom / nut_monthly_wor_zoom)
    n_with_wor_zoom <- sum(!is.na(ocha_wor_zoom$total_value))

    inset_wor_zoom <- paste0(
      "Zones with data: ",
      n_with_wor_zoom,
      " / ",
      nrow(ocha_admin2),
      "\nAvg / month per unit: ",
      format(
        round(mean(ocha_wor_zoom$total_value, na.rm = TRUE)),
        big.mark = ","
      ),
      "\n% of NUT total: ",
      pct_wor_zoom,
      "%"
    )

    p_wor_zoom <- ggplot() +
      geom_sf(
        data = ocha_wor_zoom,
        aes(fill = total_value),
        colour = "grey70",
        lwd = 0.1
      ) +
      geom_sf(data = ocha_admin1, fill = NA, colour = "black", lwd = 0.45) +
      fill_scale +
      annotate(
        "label",
        x = Inf,
        y = -Inf,
        hjust = 1.05,
        vjust = -0.15,
        label = inset_wor_zoom,
        size = 3.5,
        fill = "white",
        alpha = 0.85
      ) +
      labs(
        title = "Children <2 years weighed by Gregorian year (zonal - woreda data aggregated)",
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

    save_map(
      p_wor_zoom,
      paste0("gmp_map_zonal_woreda_agg_", ZOOM_YEAR, ".png"),
      data = ocha_wor_zoom,
      lvl = 2,
      used = TRUE,
      width = 10,
      height = 9,
      dpi = 200
    )
    cat("  Saved gmp_map_zonal_woreda_agg_", ZOOM_YEAR, ".png\n", sep = "")
  }
}

cat("\nDone. Outputs in", out_dir, "\n")

# *============================================================================*
