# *========================== GMP indicator coverage ==========================*
# Uses malnutrition_ind_level*.csv + malnutrition_de_level*.csv files.
# Indicator: NUT_ % of Children < 2 years participated in GMP
# Data elem:  NUT_Children <2 Years Weighted during GMP Session
#
# Outputs in src/R/outputs/RQ2_RQ3_national_level_and_geographic_variation/:
#   not_used/gmp_ind_coverage/
#     01_national_trend.png             - national % over time
#     03_summary_range.csv              - range / >100% summary + NUT weighed totals
#     05_stacked_bands_by_level.png     - coverage bands by admin level, all years
#     06_stacked_bands_zonal.png        - coverage bands per zone (sorted)
#   used/gmp_ind_coverage/ (slide figures, each with a CSV of its plotted data)
#     04_boxplot_no_outliers.png                - boxplot (outliers hidden) + stats table
#     05_stacked_bands_by_level_latest_year.png - coverage bands by admin level, latest year
# *============================================================================*

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(sf)
library(stringr)
library(scales)
library(patchwork)
library(gridExtra)

source(here::here("src", "R", "plotting_helpers.R"))

# *---------------------------------- paths -----------------------------------*
ind_dir <- here::here("data", "GMP", "indicators")
de_dir <- here::here("data", "GMP", "data_elements")
ocha_dir <- here::here("data", "shapefiles", "OCHA")
out_dir <- here::here("src", "R", "outputs", "RQ2_RQ3_national_level_and_geographic_variation", "not_used", "gmp_ind_coverage")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

GMP_PCT_VAR <- "NUT_ % of Children < 2 years participated in GMP"
NUT_WEIGH_VAR <- "NUT_Children <2 Years Weighted during GMP Session"

LEVEL_ORDER <- c("National", "Regional", "Zonal", "Woreda", "PHCU", "Facility")

normalise_level <- function(x) {
  case_when(
    str_to_lower(x) == "national" ~ "National",
    str_to_lower(x) == "regional" ~ "Regional",
    str_to_lower(x) == "zonal" ~ "Zonal",
    str_to_lower(x) %in% c("woreda", "wereda") ~ "Woreda",
    str_to_lower(x) == "phcu" ~ "PHCU",
    str_to_lower(x) %in% c("facility", "facilities") ~ "Facility",
    TRUE ~ x
  )
}

# *------------------ Ethiopian → Gregorian date conversion -------------------*
eth_month_greg <- data.frame(
  eth_month = 1:12,
  greg_month = c(9, 10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8),
  greg_day = c(11, 11, 10, 10, 9, 8, 10, 9, 9, 8, 8, 7),
  yr_offset = c(7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8)
)

eth_to_gregorian <- function(eth_year, eth_month) {
  lkp <- eth_month_greg[match(eth_month, eth_month_greg$eth_month), ]
  as.Date(sprintf(
    "%04d-%02d-%02d",
    eth_year + lkp$yr_offset,
    lkp$greg_month,
    lkp$greg_day
  ))
}

# *----------------------------- load indicators ------------------------------*
ind_files <- list.files(
  ind_dir,
  pattern = "malnutrition_ind_level.*\\.csv",
  full.names = TRUE
)

ind_raw <- lapply(
  ind_files,
  read_csv,
  show_col_types = FALSE,
  col_types = cols(.default = "c")
) |>
  bind_rows() |>
  distinct() |>
  filter(data_element_name == GMP_PCT_VAR) |>
  mutate(
    value = as.numeric(value),
    eth_year = as.integer(substr(period, 1, 4)),
    eth_month = as.integer(substr(period, 5, 6)),
    gregorian_date = eth_to_gregorian(eth_year, eth_month),
    gregorian_year = as.integer(format(gregorian_date, "%Y")),
    org_unit_level = as.integer(org_unit_level)
  )

cat(
  "Indicator rows:",
  nrow(ind_raw),
  "  levels:",
  paste(sort(unique(ind_raw$org_unit_level)), collapse = ", "),
  "\n\n"
)

# *---------------------- load NUT weighed data elements ----------------------*
de_files <- list.files(
  de_dir,
  pattern = "malnutrition_de_level.*\\.csv",
  full.names = TRUE
)

nut_weighed <- lapply(
  de_files,
  read_csv,
  show_col_types = FALSE,
  col_types = cols(.default = "c")
) |>
  bind_rows() |>
  distinct() |>
  filter(data_element_name == NUT_WEIGH_VAR) |>
  mutate(
    value = as.numeric(value),
    org_unit_level = as.integer(org_unit_level)
  ) |>
  group_by(period, org_unit_name, org_unit_level) |>
  summarise(nut_weighed = sum(value, na.rm = TRUE), .groups = "drop")

cat("NUT weighed rows (after summing age bands):", nrow(nut_weighed), "\n\n")

# Join NUT weighed onto indicator rows
ind_raw <- ind_raw |>
  left_join(nut_weighed, by = c("period", "org_unit_name", "org_unit_level"))

# *----------------------- (1) National trend - level 1 -----------------------*
national <- ind_raw |>
  filter(org_unit_level == 1) |>
  group_by(gregorian_date, gregorian_year) |>
  summarise(pct = mean(value, na.rm = TRUE), .groups = "drop") |>
  arrange(gregorian_date)

p_national <- ggplot(national, aes(gregorian_date, pct)) +
  geom_line(colour = "#2166ac", linewidth = 0.7) +
  geom_point(size = 1.2, colour = "#2166ac") +
  geom_hline(
    yintercept = 100,
    linetype = "dashed",
    colour = "firebrick",
    linewidth = 0.5
  ) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "National GMP coverage for children <2 years",
    x = NULL,
    y = "GMP coverage (%)"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(out_dir, "01_national_trend.png"),
  p_national,
  width = 10,
  height = 5,
  dpi = 150
)
cat("Saved 01_national_trend.png\n")

# *------------------- (3) Range summary across all levels --------------------*
summary_by_level <- ind_raw |>
  mutate(level_label = normalise_level(org_unit_level_name)) |>
  group_by(org_unit_level, level_label) |>
  summarise(
    n_obs = n(),
    n_missing = sum(is.na(value)),
    n_below75 = sum(value < 75, na.rm = TRUE),
    n_75_to_100 = sum(value >= 75 & value <= 100, na.rm = TRUE),
    n_above100 = sum(value > 100, na.rm = TRUE),
    pct_below75 = round(100 * n_below75 / (n_obs - n_missing), 1),
    pct_75_to_100 = round(100 * n_75_to_100 / (n_obs - n_missing), 1),
    pct_above100 = round(100 * n_above100 / (n_obs - n_missing), 1),
    min_pct = round(min(value, na.rm = TRUE), 1),
    p25 = round(quantile(value, 0.25, na.rm = TRUE), 1),
    median_pct = round(median(value, na.rm = TRUE), 1),
    p75 = round(quantile(value, 0.75, na.rm = TRUE), 1),
    max_pct = round(max(value, na.rm = TRUE), 1),
    total_nut_weighed = sum(nut_weighed, na.rm = TRUE),
    date_min = min(gregorian_date, na.rm = TRUE),
    date_max = max(gregorian_date, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    level_label = factor(level_label, levels = LEVEL_ORDER),
    date_range = paste0(
      format(date_min, "%b %Y"),
      " - ",
      format(date_max, "%b %Y")
    )
  ) |>
  arrange(level_label)

write_csv(summary_by_level, file.path(out_dir, "03_summary_range.csv"))
cat("\n03_summary_range.csv:\n")
print(summary_by_level)

# *--------- (4) Boxplot (no outliers) - level labels as plain names ----------*
box_data <- ind_raw |>
  filter(!is.na(value), !is.na(org_unit_level_name)) |>
  mutate(
    level_label = factor(
      normalise_level(org_unit_level_name),
      levels = LEVEL_ORDER
    )
  )

box_limits <- box_data |>
  group_by(level_label) |>
  summarise(
    q1 = quantile(value, 0.25, na.rm = TRUE),
    q3 = quantile(value, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    lower = q1 - 1.5 * iqr,
    upper = q3 + 1.5 * iqr,
    .groups = "drop"
  )

p_box <- ggplot(box_data, aes(x = level_label, y = value)) +
  geom_boxplot(
    outlier.shape = NA,
    fill = "#cce5ff",
    colour = "#2166ac",
    linewidth = 0.5
  ) +
  geom_hline(
    yintercept = 75,
    linetype = "dotted",
    colour = "grey30",
    linewidth = 0.6
  ) +
  geom_hline(
    yintercept = 100,
    linetype = "dashed",
    colour = "firebrick",
    linewidth = 0.6
  ) +
  annotate(
    "text",
    x = -Inf,
    y = 79,
    label = "75% national standard",
    hjust = -0.05,
    size = 3,
    colour = "grey30"
  ) +
  annotate(
    "text",
    x = -Inf,
    y = 104,
    label = ">100%",
    hjust = -0.05,
    size = 3,
    colour = "firebrick"
  ) +
  coord_cartesian(
    ylim = c(0, min(max(box_limits$upper), 150))
  ) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "GMP coverage distribution by admin level (outliers hidden)",
    x = NULL,
    y = "GMP coverage (%)"
  )

p_box <- format_gg_plot(p_box, text_increase = 0)

save_used_figure(
  p_box,
  out_dir,
  "04_boxplot_no_outliers.png",
  cols = c("level_label", "value"),
  width = 12,
  height = 7,
  dpi = 150
)
cat("Saved 04_boxplot_no_outliers.png\n")

# *-------------- (5) Stacked coverage bands - all admin levels ---------------*
# One bar per admin level; percentages pooled across all org units and time
# points within each level. Two versions: overall and latest year.
BAND_LEVELS <- c("<75%", "75-100%", ">100%")
BAND_COLOURS <- c(
  "<75%" = "#d6604d",
  "75-100%" = "#4dac26",
  ">100%" = "#4393c3"
)

make_band_df <- function(df) {
  df |>
    filter(!is.na(value), !is.na(org_unit_level_name)) |>
    mutate(
      level_label = factor(
        normalise_level(org_unit_level_name),
        levels = LEVEL_ORDER
      ),
      band = factor(
        case_when(
          value < 75 ~ "<75%",
          value <= 100 ~ "75-100%",
          TRUE ~ ">100%"
        ),
        levels = BAND_LEVELS
      )
    ) |>
    count(level_label, band) |>
    group_by(level_label) |>
    mutate(prop = n / sum(n)) |>
    ungroup()
}

make_stacked_plot <- function(
  band_df,
  title,
  x_var = level_label,
  bar_width = 0.7,
  hide_x = FALSE,
  annotate_red = FALSE
) {
  p <- ggplot(band_df, aes(x = {{ x_var }}, y = prop, fill = band)) +
    geom_bar(
      stat = "identity",
      position = position_stack(reverse = TRUE),
      width = bar_width
    ) +
    scale_fill_manual(
      values = BAND_COLOURS,
      name = "Coverage band",
      guide = guide_legend(reverse = FALSE)
    ) +
    scale_y_continuous(labels = label_percent(), expand = c(0, 0)) +
    labs(title = title, x = NULL, y = "Percentage of reporting periods")

  if (annotate_red) {
    p <- p +
      geom_text(
        aes(label = ifelse(prop >= 0.03, percent(prop, accuracy = 1), "")),
        position = position_stack(vjust = 0.5, reverse = TRUE),
        colour = "white",
        size = 3,
        fontface = "bold"
      )
  }

  if (hide_x) {
    p <- p +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  p
}

# Overall
p_stacked <- make_stacked_plot(
  make_band_df(ind_raw),
  "Percentage of reporting periods per coverage band by admin level (all years)",
  annotate_red = TRUE
)
p_stacked <- format_gg_plot(p_stacked, text_increase = 0)

ggsave(
  file.path(out_dir, "05_stacked_bands_by_level.png"),
  p_stacked,
  width = 10,
  height = 6,
  dpi = 150
)
cat("Saved 05_stacked_bands_by_level.png\n")

# 2025 only
p_stacked_last <- make_stacked_plot(
  make_band_df(filter(ind_raw, gregorian_year == 2025)),
  "Percentage of reporting periods per coverage band by admin level (2025)",
  annotate_red = TRUE
)
p_stacked_last <- format_gg_plot(p_stacked_last, text_increase = 0)

save_used_figure(
  p_stacked_last,
  out_dir,
  "05_stacked_bands_by_level_latest_year.png",
  cols = c("level_label", "band", "n", "prop"),
  width = 10,
  height = 6,
  dpi = 150
)
cat("Saved 05_stacked_bands_by_level_latest_year.png\n")

# *-------------- (6) Stacked coverage bands - one bar per zone ---------------*
zone_bands <- ind_raw |>
  filter(normalise_level(org_unit_level_name) == "Zonal", !is.na(value)) |>
  mutate(
    band = factor(
      case_when(
        value < 75 ~ "<75%",
        value <= 100 ~ "75-100%",
        TRUE ~ ">100%"
      ),
      levels = BAND_LEVELS
    )
  ) |>
  count(org_unit_name, band) |>
  group_by(org_unit_name) |>
  mutate(prop = n / sum(n)) |>
  ungroup()

zone_order <- c(
  zone_bands |>
    filter(band == "75-100%") |>
    arrange(desc(prop)) |>
    pull(org_unit_name),
  setdiff(
    unique(zone_bands$org_unit_name),
    zone_bands |> filter(band == "75-100%") |> pull(org_unit_name)
  )
)

zone_bands <- zone_bands |>
  mutate(org_unit_name = factor(org_unit_name, levels = zone_order))

p_zone <- ggplot(zone_bands, aes(x = org_unit_name, y = prop, fill = band)) +
  geom_bar(
    stat = "identity",
    position = position_stack(reverse = TRUE),
    width = 0.8
  ) +
  scale_fill_manual(
    values = BAND_COLOURS,
    name = "Coverage band",
    guide = guide_legend(reverse = FALSE)
  ) +
  scale_y_continuous(labels = label_percent(), expand = c(0, 0)) +
  labs(
    title = "Percentage of reporting periods per coverage band - Zonal level",
    subtitle = paste0(
      "n = ",
      n_distinct(zone_bands$org_unit_name),
      " zones; sorted by highest percentage in 75-100% band"
    ),
    x = NULL,
    y = "Percentage of reporting periods"
  )

p_zone <- format_gg_plot(p_zone, text_increase = 0) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

ggsave(
  file.path(out_dir, "06_stacked_bands_zonal.png"),
  p_zone,
  width = 14,
  height = 6,
  dpi = 150
)
cat("Saved 06_stacked_bands_zonal.png\n")

cat("\nDone. Outputs in", out_dir, "\n")

# *============================================================================*
