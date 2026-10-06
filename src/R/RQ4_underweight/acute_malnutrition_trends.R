# *=================== Acute malnutrition – national trends ===================*
# Monthly and annual national counts of babies screened with moderate (MAM) and
# severe (SAM) acute malnutrition, used as a proxy for question 4.
#
# MAM and SAM data elements (_2017_ prefix) exist only at national level in
# DHIS2, so the analysis is national only. There is no suitable denominator,
# so counts are not converted to proportions.
#
# Outputs in src/R/outputs/RQ4_underweight/used/acute_malnutrition_trends/
# (slide figures, each with a CSV of its plotted data):
#   trend_comparison.png         — monthly counts (MAM, SAM, overall)
#   trend_comparison_annual.png  — average monthly count per year
# *============================================================================*

# *--------------------------------- imports ----------------------------------*
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(scales)

source(here::here("src", "R", "plotting_helpers.R"))

# *---------------------------------- config ----------------------------------*
# AGE_BANDS: category_option_combo_name values to include.
#   "0 - 5 Months"  — infants only (default)
#   "6 - 59 months" — older children
#   NULL            — all available bands (summed)
AGE_BANDS <- "0 - 5 Months"

# Gregorian year window for the plots
YEAR_MIN <- 2018L
YEAR_MAX <- 2025L

TEXT_INCREASE <- 0 # passed to format_gg_plot(); negative = smaller text
PLOT_W <- 12
PLOT_H <- 6
PLOT_DPI <- 150

MAM_VAR <- "_2017_Number of children <5 year screened and have moderate acute malnutrition"
SAM_VAR <- "_2017_Number of children <5 year screened and have severe acute malnutrition"

# *---------------------------------- paths -----------------------------------*
de_dir <- here::here("data", "GMP", "data_elements")
# Both figures are used in the slides, so save_used_figure() writes them to
# the matching used/ folder.
out_dir <- here::here("src", "R", "outputs", "RQ4_underweight", "not_used", "acute_malnutrition_trends")

# *----------------------------------- load -----------------------------------*
raw <- lapply(
  list.files(de_dir, pattern = "malnutrition_de_level.*\\.csv", full.names = TRUE),
  read_csv,
  show_col_types = FALSE,
  col_types = cols(.default = "c")
) |>
  bind_rows() |>
  distinct() |>
  filter(data_element_name %in% c(MAM_VAR, SAM_VAR)) |>
  mutate(
    value = as.numeric(value),
    org_unit_level = as.integer(org_unit_level),
    greg_date = as.Date(paste0(substr(gregorian_date, 1, 7), "-01")),
    greg_year = as.integer(substr(gregorian_date, 1, 4))
  )

if (!is.null(AGE_BANDS)) {
  raw <- raw |> filter(category_option_combo_name %in% AGE_BANDS)
  age_label <- paste(AGE_BANDS, collapse = " + ")
} else {
  age_label <- "all age bands"
}

cat("Age bands included:", age_label, "\n")
cat(
  "Levels with MAM/SAM data:",
  paste(sort(unique(raw$org_unit_level_name)), collapse = ", "),
  "\n\n"
)

# *------------------------- monthly national totals --------------------------*
# Deduplicate by the natural key before summing (removes duplicated pulls).
monthly_nat <- raw |>
  filter(org_unit_level == 1L) |>
  distinct(
    data_element_name,
    category_option_combo_id,
    period,
    urban_rural_id,
    .keep_all = TRUE
  ) |>
  group_by(data_element_name, greg_date) |>
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(
    names_from = data_element_name,
    values_from = total,
    values_fill = 0
  ) |>
  rename(mam = all_of(MAM_VAR), sam = all_of(SAM_VAR)) |>
  mutate(overall = mam + sam) |>
  filter(
    greg_date >= as.Date(paste0(YEAR_MIN, "-01-01")),
    greg_date <= as.Date(paste0(YEAR_MAX, "-12-31"))
  )

# *------------------------------ plot settings -------------------------------*
METRIC_COLOURS <- c(
  Moderate = "#D97706",
  Severe = "#DC2626",
  Overall = "#2563EB"
)

legend_inside <- theme(
  legend.position = "inside",
  legend.position.inside = c(0.9, 0.9)
)

# Year-start breaks for the monthly x-axis
year_breaks <- seq(
  as.Date(paste0(YEAR_MIN, "-01-01")),
  as.Date(paste0(YEAR_MAX, "-01-01")),
  by = "year"
)

# Long format with readable metric labels, in legend order
to_long_metrics <- function(df) {
  df |>
    pivot_longer(
      c(mam, sam, overall),
      names_to = "metric",
      values_to = "count"
    ) |>
    mutate(
      metric = recode(
        metric,
        mam = "Moderate",
        sam = "Severe",
        overall = "Overall"
      ),
      metric = factor(metric, levels = names(METRIC_COLOURS))
    )
}

# *------------------- monthly plot: MAM vs SAM vs overall --------------------*
monthly_long <- to_long_metrics(monthly_nat)

p_comparison <- monthly_long |>
  ggplot(aes(x = greg_date, y = count, colour = metric, group = metric)) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = METRIC_COLOURS, name = NULL) +
  scale_x_date(breaks = year_breaks, date_labels = "%Y", minor_breaks = NULL) +
  scale_y_continuous(labels = comma, limits = c(0, NA)) +
  labs(
    title = paste0(
      "Number of babies (",
      tolower(age_label),
      ") screened per month with acute malnutrition"
    ),
    x = "Gregorian year",
    y = "Number of babies"
  )

p_comparison <- format_gg_plot(p_comparison, text_increase = TEXT_INCREASE) +
  legend_inside

save_used_figure(
  p_comparison,
  out_dir,
  "trend_comparison.png",
  cols = c("greg_date", "metric", "count"),
  width = PLOT_W,
  height = PLOT_H,
  dpi = PLOT_DPI
)

# *--------------- annual plot: average monthly count per year ----------------*
annual_long <- monthly_nat |>
  mutate(greg_year = as.integer(format(greg_date, "%Y"))) |>
  group_by(greg_year) |>
  summarise(
    across(c(mam, sam, overall), \(x) mean(x, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  to_long_metrics()

p_comparison_annual <- annual_long |>
  filter(greg_year != 2023) |>
  ggplot(aes(x = greg_year, y = count, colour = metric, group = metric)) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = METRIC_COLOURS, name = NULL) +
  scale_x_continuous(breaks = YEAR_MIN:YEAR_MAX) +
  scale_y_continuous(labels = comma, limits = c(0, NA)) +
  labs(
    title = paste0(
      "Average number of babies (",
      tolower(age_label),
      ") screened per month with acute malnutrition"
    ),
    x = "Gregorian year",
    y = "Average number of babies per month"
  )

p_comparison_annual <- format_gg_plot(
  p_comparison_annual,
  text_increase = TEXT_INCREASE
) +
  legend_inside

save_used_figure(
  p_comparison_annual,
  out_dir,
  "trend_comparison_annual.png",
  cols = c("greg_year", "metric", "count"),
  width = PLOT_W,
  height = PLOT_H,
  dpi = PLOT_DPI
)
