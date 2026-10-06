# *========================== GMP data completeness ===========================*
# Reads all GMP data element CSVs and reports completeness per year for each
# variable, broken down by admin level.
#
# Completeness = (org units that reported in year) / (max org units ever seen
# for that variable × level combination across all years) × 100
#
# Outputs in src/R/outputs/data_quality/not_used/DHIS2/gmp_completeness/:
#   gmp_completeness_by_year.csv
#   gmp_completeness_by_year.png
# *============================================================================*

# *--------------------------------- imports ----------------------------------*
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

# *---------------------------------- paths -----------------------------------*
data_dir <- here::here("data", "GMP", "data_elements")
out_dir  <- here::here("src", "R", "outputs", "data_quality", "not_used", "DHIS2", "gmp_completeness")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# *----------------------------------- load -----------------------------------*
files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)

combined_gmp_data <- read_csv(files, show_col_types = FALSE) |>
  mutate(
    year  = as.integer(substr(period, 1, 4)),
    month = as.integer(substr(period, 5, 6))
  )

cat("Loaded", nrow(combined_gmp_data), "rows from", length(files), "files\n")
cat("Variables:", n_distinct(combined_gmp_data$data_element_name), "\n")
cat("Year range:", min(combined_gmp_data$year), "–", max(combined_gmp_data$year), "\n\n")

# *------------------------------- completeness -------------------------------*
# Denominator: max distinct org units seen for each (variable, level) pair
# across all years — treats the largest observed set as the "expected" pool.
max_units <- combined_gmp_data |>
  group_by(data_element_name, org_unit_level, org_unit_level_name, year) |>
  summarise(n_units = n_distinct(org_unit_name), .groups = "drop") |>
  group_by(data_element_name, org_unit_level, org_unit_level_name) |>
  summarise(max_units = max(n_units), .groups = "drop")

# Numerator: distinct org units that reported in each (variable, level, year)
reported <- combined_gmp_data |>
  group_by(data_element_name, org_unit_level, org_unit_level_name, year) |>
  summarise(n_reported = n_distinct(org_unit_name), .groups = "drop")

completeness <- reported |>
  left_join(max_units, by = c("data_element_name", "org_unit_level", "org_unit_level_name")) |>
  mutate(completeness_pct = round(n_reported / max_units * 100, 1)) |>
  arrange(data_element_name, org_unit_level, year)

write_csv(completeness, file.path(out_dir, "gmp_completeness_by_year.csv"))
cat("Saved gmp_completeness_by_year.csv (", nrow(completeness), "rows)\n\n")
print(as.data.frame(completeness), row.names = FALSE)

# *----------------------------------- plot -----------------------------------*
p <- completeness |>
  mutate(
    label = paste0(data_element_name, "\n(", org_unit_level_name, ")")
  ) |>
  ggplot(aes(x = year, y = completeness_pct, colour = org_unit_level_name)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  facet_wrap(~ data_element_name, ncol = 2, labeller = label_wrap_gen(40)) +
  scale_y_continuous(limits = c(0, NA), labels = \(x) paste0(x, "%")) +
  scale_x_continuous(breaks = \(x) seq(ceiling(x[1]), floor(x[2]), by = 2)) +
  labs(
    title   = "GMP data element completeness by year",
    x       = "Year",
    y       = "% of org units reporting",
    colour  = "Admin level"
  ) +
  theme_minimal() +
  theme(
    strip.text       = element_text(size = 7),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "top"
  )

ggsave(
  file.path(out_dir, "gmp_completeness_by_year.png"),
  p,
  width  = 14,
  height = ceiling(n_distinct(completeness$data_element_name) / 2) * 3.5,
  dpi    = 150
)
cat("Saved gmp_completeness_by_year.png\n")

# *============================================================================*
