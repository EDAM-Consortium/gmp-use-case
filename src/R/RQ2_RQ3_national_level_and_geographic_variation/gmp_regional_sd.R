# *=============== GMP regional SD — children <2 weighed, 2025 ================*
# For each region, computes the mean and standard deviation of monthly children
# weighed during Gregorian 2025 using ALL level-2 (regional) data — i.e. the
# full set of DHIS2-reported regional totals, not filtered to OCHA-matched rows.
#
# Age-band categories (0–5 months, 6–23 months) are summed before aggregation
# so the SD reflects whole-region monthly totals, not per-band variability.
#
# Outputs in src/R/outputs/RQ2_RQ3_national_level_and_geographic_variation/:
#   not_used/gmp_regional_sd/
#     gmp_regional_sd_2025.csv   — mean, SD, CV and N months per region
#     gmp_regional_sd_2025.png   — choropleth of SD (OCHA adm1)
#   used/gmp_regional_sd/ (slide figures, each with a CSV of its plotted data)
#     gmp_regional_cv_2025.png   — choropleth of CV = SD / mean (OCHA adm1)
# *============================================================================*

library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(scales)

source(here::here("src", "R", "plotting_helpers.R"))

# *---------------------------------- config ----------------------------------*
ZOOM_YEAR <- 2025L

gmp_path <- here::here("src", "R", "outputs", "data_preparation", "not_used", "generate_dhis2_gmp_shapes", "gmp_spatial.csv")
ocha_dir <- here::here("data", "shapefiles", "OCHA")
out_dir <- here::here("src", "R", "outputs", "RQ2_RQ3_national_level_and_geographic_variation", "not_used", "gmp_regional_sd")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# *----------------------------------- load -----------------------------------*
ocha_adm1 <- st_read(file.path(ocha_dir, "eth_admin1.shp"), quiet = TRUE) |>
  st_make_valid() |>
  st_transform(4326)

gmp_raw <- read_csv(gmp_path, show_col_types = FALSE) |>
  filter(
    org_unit_level == 2, # regional-level rows only
    between(year, 2000L, 2030L),
    between(month, 1L, 13L)
  ) |>
  mutate(
    gregorian_year = as.integer(year) +
      if_else(as.integer(month) <= 4L, 7L, 8L)
  ) |>
  filter(gregorian_year == ZOOM_YEAR)

cat("Level-2 rows for Gregorian", ZOOM_YEAR, ":", nrow(gmp_raw), "\n")
cat("Regions:", n_distinct(gmp_raw$org_unit_name), "\n\n")

# *--------- Sum age bands → one total per (region × Ethiopian month) ---------*
monthly_totals <- gmp_raw |>
  group_by(org_unit_name, adm1_pcode, month) |>
  summarise(monthly_value = sum(value, na.rm = TRUE), .groups = "drop")

# *------------------------- Mean, SD, CV per region --------------------------*
regional_sd <- monthly_totals |>
  group_by(org_unit_name, adm1_pcode) |>
  summarise(
    n_months = n(),
    mean_value = round(mean(monthly_value, na.rm = TRUE)),
    sd_value = round(sd(monthly_value, na.rm = TRUE)),
    cv_pct = round(
      sd(monthly_value, na.rm = TRUE) /
        mean(monthly_value, na.rm = TRUE) *
        100,
      1
    ),
    .groups = "drop"
  ) |>
  arrange(desc(sd_value))

# *----------- National-level SD (sum all regions per month first) ------------*
national_sd <- monthly_totals |>
  group_by(month) |>
  summarise(
    monthly_value = sum(monthly_value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  summarise(
    org_unit_name = "National",
    adm1_pcode = NA_character_,
    n_months = n(),
    mean_value = round(mean(monthly_value, na.rm = TRUE)),
    sd_value = round(sd(monthly_value, na.rm = TRUE)),
    cv_pct = round(
      sd(monthly_value, na.rm = TRUE) /
        mean(monthly_value, na.rm = TRUE) *
        100,
      1
    )
  )

cat("National SD summary:\n")
print(as.data.frame(national_sd), row.names = FALSE)
cat("\n")

sd_summary <- bind_rows(national_sd, regional_sd)
write_csv(sd_summary, file.path(out_dir, "gmp_regional_sd_2025.csv"))
cat("Regional SD summary:\n")
print(as.data.frame(regional_sd), row.names = FALSE)

# *---------------------------- Shared map helper -----------------------------*
make_sd_map <- function(fill_col, title, legend_name, label_fn = comma) {
  plot_sf <- ocha_adm1 |>
    left_join(regional_sd, by = "adm1_pcode")

  ggplot(plot_sf) +
    geom_sf(aes(fill = .data[[fill_col]]), colour = "white", linewidth = 0.3) +
    geom_sf(data = ocha_adm1, fill = NA, colour = "black", linewidth = 0.5) +
    scale_fill_distiller(
      palette = "RdYlGn",
      direction = -1, # high SD = red (more variable)
      na.value = "grey88",
      name = legend_name,
      labels = label_fn,
      oob = squish
    ) +
    labs(title = title) +
    theme_void(base_size = 11) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 13,
        margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        size = 10,
        colour = "grey40",
        margin = margin(b = 6)
      ),
      legend.position = "right"
    )
}

# *---------------------------------- SD map ----------------------------------*
p_sd <- make_sd_map(
  fill_col = "sd_value",
  title = "SD of monthly children <2 weighed by region",
  legend_name = "SD\n(children)"
)

ggsave(
  file.path(out_dir, "gmp_regional_sd_2025.png"),
  p_sd,
  width = 9,
  height = 8,
  dpi = 180
)
cat("\nSaved gmp_regional_sd_2025.png\n")

# *---------------------------------- CV map ----------------------------------*
p_cv <- make_sd_map(
  fill_col = "cv_pct",
  title = "Coefficient of variation (CV) of children <2 weighed per month by region",
  legend_name = "CV (%)",
  label_fn = \(x) paste0(x, "%")
)

save_used_figure(
  p_cv,
  out_dir,
  "gmp_regional_cv_2025.png",
  cols = c("adm1_name", "adm1_pcode", "cv_pct"),
  width = 9,
  height = 8,
  dpi = 180
)
cat("Saved gmp_regional_cv_2025.png\n")

cat("\nDone. Outputs in", out_dir, "\n")

# *============================================================================*
