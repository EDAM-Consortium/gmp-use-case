# *========================= GMP completeness report ==========================*
# Reads all GMP indicator CSVs, deduplicates, and produces in
# src/R/outputs/data_quality/not_used/DHIS2/completeness_report/:
#   - date_ranges.csv         per (variable, level): date span, n_actual/expected, % complete
#   - completeness_by_year.csv per (variable, level, year): same metrics annually
#   - completeness_heatmap.pdf  overall % complete by variable × admin level
#   - completeness_by_year.pdf  % complete over time, faceted by variable
# *============================================================================*

# *--------------------------------- imports ----------------------------------*
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
library(scales)

source(here::here("src", "R", "plotting_helpers.R"))

# *---------------------------------- paths -----------------------------------*
data_dir <- here::here("data", "GMP", "indicators")

out_dir <- here::here("src", "R", "outputs", "data_quality", "not_used", "DHIS2", "completeness_report")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# *----------------------------------- load -----------------------------------*
# Read per file and bind: levels 1–2 have no urban_rural columns (NA after bind)
all_data <- lapply(
  list.files(
    data_dir,
    pattern = "malnutrition_ind_level.*\\.csv",
    full.names = TRUE
  ),
  read_csv,
  show_col_types = FALSE
) |>
  bind_rows()

n_before <- nrow(all_data)
all_data <- distinct(all_data)
cat(
  "Removed",
  n_before - nrow(all_data),
  "duplicate rows;",
  nrow(all_data),
  "remaining\n"
)

all_data <- all_data |>
  mutate(
    year = as.integer(substr(period, 1, 4)),
    date = as.Date(paste(year, substr(period, 5, 6), "01", sep = "-")),
    org_unit_level_name = recode(org_unit_level_name, "Wereda" = "Woreda")
  )

# *------------------------------- completeness -------------------------------*
# Completeness = n_actual / n_expected where:
#   n_actual   = distinct (facility, month, combo, urban_rural) tuples reported
#   n_expected = n_facilities × n_months × n_groups
#
# Computed at two granularities from the same logic:
#   date_ranges        — overall (variable × level)
#   completeness_by_year — annual  (variable × level × year)

compute_completeness <- function(df, ...) {
  df |>
    group_by(data_element_name, org_unit_level, org_unit_level_name, ...) |>
    summarise(
      start_date = min(date),
      end_date = max(date),
      n_groups = n_distinct(interaction(
        # category_option_combo_name,
        urban_rural
      )),
      n_facilities = n_distinct(org_unit_name),
      n_months = n_distinct(date),
      n_actual = n_distinct(interaction(
        org_unit_name,
        date,
        # category_option_combo_name,
        urban_rural
      )),
      .groups = "drop"
    ) |>
    mutate(
      n_expected = n_facilities * n_months * n_groups,
      pct_complete = round(100 * n_actual / n_expected, 1)
    )
}

date_ranges <- compute_completeness(all_data)
completeness_by_year <- compute_completeness(all_data, year)

cat("\nDate range per variable × level:\n")
print(as.data.frame(date_ranges), row.names = FALSE)

overall_start <- min(date_ranges$start_date)
overall_end <- max(date_ranges$end_date)

# *--------------------------------- heatmap ----------------------------------*
LEVEL_ORDER <- c(
  "National",
  "Regional",
  "Zonal",
  "Woreda",
  "PHCU",
  "Facilities",
  "Average"
)

master_complete <- date_ranges |>
  group_by(data_element_name) |>
  summarise(
    mean_pct_complete = round(mean(pct_complete), 1),
    .groups = "drop"
  ) |>
  arrange(desc(mean_pct_complete))

var_order <- master_complete |>
  arrange(mean_pct_complete) |>
  mutate(data_element_name = str_trunc(data_element_name, 100)) |>
  pull(data_element_name)

level_labels <- date_ranges |>
  group_by(org_unit_level_name) |>
  summarise(n_expected = max(n_expected), .groups = "drop") |>
  mutate(label = paste0(org_unit_level_name, "\n(n=", comma(n_expected), ")"))

label_vec <- setNames(level_labels$label, level_labels$org_unit_level_name)
label_vec["Average"] <- "Average"

average_rows <- master_complete |>
  mutate(
    data_element_name = str_trunc(data_element_name, 100),
    org_unit_level_name = "Average",
    pct_complete = mean_pct_complete
  ) |>
  select(data_element_name, org_unit_level_name, pct_complete)

heatmap_data <- date_ranges |>
  mutate(data_element_name = str_trunc(data_element_name, 100)) |>
  complete(
    data_element_name,
    org_unit_level_name,
    fill = list(pct_complete = 0)
  ) |>
  bind_rows(average_rows) |>
  mutate(
    org_unit_level_name = factor(org_unit_level_name, levels = LEVEL_ORDER),
    data_element_name = factor(data_element_name, levels = var_order)
  )

heatmap_plot <- ggplot(
  heatmap_data,
  aes(x = org_unit_level_name, y = data_element_name, fill = pct_complete)
) +
  geom_tile(colour = "white") +
  geom_text(
    aes(label = paste0(pct_complete, "%")),
    size = 3.5,
    fontface = "bold"
  ) +
  scale_x_discrete(labels = label_vec) +
  scale_fill_gradient2(
    low = "red",
    mid = "orange",
    high = "darkgreen",
    midpoint = 50,
    limits = c(0, 100),
    name = "% complete"
  ) +
  labs(
    title = "GMP & malnutrition indicator completeness by admin level",
    x = "Admin level",
    y = NULL
  )

heatmap_plot <- format_gg_plot(heatmap_plot) +
  theme(
    axis.text.y = element_text(size = 15),
  )

# *-------------------------------- line plot ---------------------------------*
line_plot <- completeness_by_year |>
  mutate(data_element_name = str_trunc(data_element_name, 50)) |>
  ggplot(aes(x = year, y = pct_complete, colour = org_unit_level_name)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  facet_wrap(~data_element_name, ncol = 3, labeller = label_wrap_gen(60)) +
  scale_y_continuous(limits = c(0, 100), labels = \(x) paste0(x, "%")) +
  scale_x_continuous(breaks = \(x) seq(ceiling(x[1]), floor(x[2]), by = 5)) +
  labs(
    title = "GMP completeness by year and admin level",
    x = "Year",
    y = "% complete",
    colour = "Admin level"
  ) +
  theme_bw()

line_plot <- format_gg_plot(line_plot)

# *----------------------------------- save -----------------------------------*
ggsave(
  file.path(out_dir, "completeness_heatmap.pdf"),
  heatmap_plot,
  width = 18,
  height = 8,
  dpi = 150,
  device = cairo_pdf
)

ggsave(
  file.path(out_dir, "completeness_by_year.pdf"),
  line_plot,
  width = 18,
  height = 15,
  dpi = 200,
  device = cairo_pdf
)

write_csv(date_ranges, file.path(out_dir, "date_ranges.csv"))
write_csv(completeness_by_year, file.path(out_dir, "completeness_by_year.csv"))

# *============================================================================*
