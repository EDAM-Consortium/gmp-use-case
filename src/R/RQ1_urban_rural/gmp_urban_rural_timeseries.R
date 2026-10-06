# *=============== GMP Urban/Rural split – children <2 weighed ================*
# Analyses NUT_Children <2 Years Weighted during GMP Session at national level,
# broken down by urban vs rural category option.
#
# Produces:
#   1. Annual time-series of absolute counts and urban/rural share (%)
#   2. Summary for ETH year 2017 (periods 201701–201712)
#   3. Urban/rural population share from WBP-Total population-Aggregated
#      for the same year
#
# Outputs in src/R/outputs/RQ1_urban_rural/:
#   not_used/gmp_urban_rural/
#     timeseries_urban_rural.csv
#     timeseries_urban_rural.png
#     greg2024_summary.csv
#     gmp_urban_rural_summary.csv        — read by gmp_overall_comparison.R
#     panel_pct_share.png                — urban/rural share %
#     panel_abs_with_gmp.png             — raw counts + GMP participation
#     wbp_population_split.png           — WBP urban_rural pop
#     wbp_population_split_idb.png       — IDB × WBP share pop
#     wbp_population_split_wbp_total.png — WBP total × WBP share pop
#     panel_wbp_urban_rural_no_gmp.png   — coverage: WBP urban_rural denom
#     panel_idb_split_no_gmp.png         — coverage: IDB denom
#   used/gmp_urban_rural/ (slide figures, each with a CSV of its plotted data)
#     panel_pct_share_with_pop.png       — urban/rural share % with population share
#     panel_abs_no_gmp.png               — raw counts
#     panel_wbp_total_no_gmp.png         — coverage: WBP total denom
# *============================================================================*
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(scales)
library(patchwork)

source(here::here("src", "R", "plotting_helpers.R"))

data_file <- here::here(
  "data",
  "GMP",
  "data_elements",
  "malnutrition_de_level1_National.csv"
)
ind_file <- here::here(
  "data",
  "GMP",
  "indicators",
  "malnutrition_ind_level1_National.csv"
)
out_dir <- here::here("src", "R", "outputs", "RQ1_urban_rural", "not_used", "gmp_urban_rural")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

nat <- read_csv(data_file, show_col_types = FALSE) |>
  mutate(
    eth_year = as.integer(substr(period, 1, 4)),
    eth_month = as.integer(substr(period, 5, 6)),
    value = as.numeric(value)
  )

# *----------------------- GMP participation indicator ------------------------*
ind <- read_csv(ind_file, show_col_types = FALSE) |>
  filter(
    data_element_name == "NUT_ % of Children < 2 years participated in GMP"
  ) |>
  mutate(
    eth_month = as.integer(substr(period, 5, 6)),
    greg_year = as.integer(substr(period, 1, 4)) +
      if_else(eth_month <= 4L, 7L, 8L),
    value = as.numeric(value)
  )

gmp_pct_annual <- ind |>
  filter(greg_year >= 2018, greg_year <= 2025) |>
  group_by(greg_year) |>
  summarise(gmp_pct = round(mean(value, na.rm = TRUE), 1), .groups = "drop")

# *------------------------- NUT: children <2 weighed -------------------------*
nut <- nat |>
  filter(
    data_element_name == "NUT_Children <2 Years Weighted during GMP Session"
  )

cat("NUT rows:", nrow(nut), "\n")
cat(
  "Category options:",
  paste(unique(nut$category_option_combo_name), collapse = ", "),
  "\n"
)
cat(
  "Urban/rural values:",
  paste(unique(nut$urban_rural), collapse = ", "),
  "\n\n"
)

# Sum both age bands within each calendar month, then average across months
# in the Gregorian year. Coverage divides mean by IDB annual estimate.
nut_annual <- nut |>
  mutate(
    greg_year = as.integer(substr(gregorian_date, 1, 4)),
    eth_year_n = as.integer(substr(period, 1, 4)),
    eth_mon_n = as.integer(substr(period, 5, 6))
  ) |>
  filter(!(eth_year_n == 2018 & eth_mon_n == 9)) |>
  group_by(greg_year, eth_month, urban_rural) |>
  summarise(monthly_total = sum(value, na.rm = TRUE), .groups = "drop") |>
  group_by(greg_year, urban_rural) |>
  summarise(
    monthly_mean = mean(monthly_total, na.rm = TRUE),
    n_months = n(),
    .groups = "drop"
  )

nut_annual <- nut_annual |>
  group_by(greg_year) |>
  mutate(
    year_total = sum(monthly_mean),
    pct = round(monthly_mean / year_total * 100, 1)
  ) |>
  ungroup()

write_csv(nut_annual, file.path(out_dir, "timeseries_urban_rural.csv"))

nut_wide <- nut_annual |>
  select(greg_year, urban_rural, pct, year_total) |>
  pivot_wider(
    names_from = urban_rural,
    values_from = pct,
    names_glue = "{urban_rural}_pct"
  ) |>
  rename(total_weighed = year_total) |>
  arrange(greg_year)

cat("Time-series (NUT) – urban/rural share with annual total:\n")
print(as.data.frame(nut_wide), row.names = FALSE)
cat("\n")

# *--------------------------- WBP population split ---------------------------*
wbp <- nat |>
  filter(data_element_name == "WBP-Total population-Aggregated")

wbp_annual <- wbp |>
  mutate(greg_year = as.integer(substr(gregorian_date, 1, 4))) |>
  group_by(greg_year, urban_rural) |>
  summarise(pop = mean(value, na.rm = TRUE), .groups = "drop")

wbp_annual <- wbp_annual |>
  group_by(greg_year) |>
  mutate(
    total_pop = sum(pop),
    pop_pct = round(pop / total_pop * 100, 1)
  ) |>
  ungroup()

# *-------------------------- Gregorian 2024 summary --------------------------*
nut_2024 <- nut_annual |> filter(greg_year == 2024)
wbp_2024 <- wbp_annual |> filter(greg_year == 2024)

summary_2024 <- nut_2024 |>
  select(urban_rural, weighed = monthly_mean, weighed_pct = pct) |>
  left_join(
    wbp_2024 |> select(urban_rural, population = pop, pop_pct),
    by = "urban_rural"
  )

write_csv(summary_2024, file.path(out_dir, "greg2024_summary.csv"))
cat("Gregorian 2024 summary:\n")
print(as.data.frame(summary_2024), row.names = FALSE)
cat("\n")

# *--------------------- IDB under-2 population estimate ----------------------*
idb_file <- here::here("data", "IDB", "idb_under2_population.csv")

idb <- read_csv(idb_file, show_col_types = FALSE) |>
  filter(adm_level == 0, area == "Ethiopia") |>
  select(
    greg_year = year,
    idb_pop = pop,
    idb_proportion = proportion,
    idb_under2_national = under2_est
  )

# WBP population × IDB proportion, grouped by Gregorian year.
idb_annual <- nat |>
  filter(data_element_name == "WBP-Total population-Aggregated") |>
  mutate(greg_year = as.integer(substr(gregorian_date, 1, 4))) |>
  left_join(idb, by = "greg_year") |>
  mutate(idb_under2_est = as.numeric(value) * idb_proportion / 100) |>
  filter(greg_year >= 2018, greg_year <= 2025) |>
  group_by(greg_year, urban_rural) |>
  summarise(
    idb_under2_est = mean(idb_under2_est, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(urban_rural = factor(urban_rural, levels = c("Urban", "Rural")))

cat("IDB-based under-2 estimates (Gregorian 2018–2025):\n")
print(as.data.frame(idb_annual), row.names = FALSE)
cat("\n")

# WBP "Urban" (~60M) is Ethiopia's actual rural population and vice versa.
idb_annual_flipped <- nat |>
  filter(data_element_name == "WBP-Total population-Aggregated") |>
  mutate(
    greg_year = as.integer(substr(gregorian_date, 1, 4)),
    urban_rural = if_else(urban_rural == "Urban", "Rural", "Urban")
  ) |>
  left_join(idb, by = "greg_year") |>
  mutate(idb_under2_est = as.numeric(value) * idb_proportion / 100) |>
  filter(greg_year >= 2018, greg_year <= 2025) |>
  group_by(greg_year, urban_rural) |>
  summarise(
    idb_under2_est = mean(idb_under2_est, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(urban_rural = factor(urban_rural, levels = c("Urban", "Rural")))

# *----------------------- GMP coverage by urban/rural ------------------------*
# Coverage = NUT children <2 weighed / (WBP population × IDB proportion)
make_coverage <- function(idb_est) {
  nut_annual |>
    filter(greg_year >= 2018, greg_year <= 2025) |>
    left_join(
      idb_est |> select(greg_year, urban_rural, idb_under2_est),
      by = c("greg_year", "urban_rural")
    ) |>
    mutate(
      coverage_pct = round(monthly_mean / idb_under2_est * 100, 1),
      urban_rural = factor(urban_rural, levels = c("Urban", "Rural"))
    )
}

coverage_annual <- make_coverage(idb_annual)
coverage_annual_flipped <- make_coverage(idb_annual_flipped)

# Alternative denominator: IDB total national under-2 split by WBP % urban
# (flipped labels). Distributes IDB's independently-estimated under-2 total
# across urban/rural using the corrected WBP population shares.
# Average WBP population across all months in each ETH year (both Gregorian
# years are included naturally since we group only by eth_year).
wbp_pct_urban <- nat |>
  filter(data_element_name == "WBP-Total population-Aggregated") |>
  mutate(
    greg_year = as.integer(substr(gregorian_date, 1, 4)),
    urban_rural = if_else(urban_rural == "Urban", "Rural", "Urban")
  ) |>
  group_by(greg_year, urban_rural) |>
  summarise(pop = mean(as.numeric(value), na.rm = TRUE), .groups = "drop") |>
  group_by(greg_year) |>
  mutate(pct_urban = pop[urban_rural == "Urban"] / sum(pop)) |>
  ungroup() |>
  select(greg_year, pct_urban) |>
  distinct()

# WBP-Total population indicator — national level, averaged per Gregorian year.
# Computed here so it is available for both the coverage and population-split plots.
wbp_total_indicator <- read_csv(ind_file, show_col_types = FALSE) |>
  filter(data_element_name == "WBP-Total population") |>
  mutate(
    value = as.numeric(value),
    eth_month = as.integer(substr(period, 5, 6)),
    greg_year = as.integer(substr(period, 1, 4)) +
      if_else(eth_month <= 4L, 7L, 8L)
  ) |>
  filter(greg_year >= 2018, greg_year <= 2025) |>
  group_by(greg_year) |>
  summarise(wbp_total = mean(value, na.rm = TRUE), .groups = "drop")

idb_split_denom <- nat |>
  filter(data_element_name == "WBP-Total population-Aggregated") |>
  mutate(greg_year = as.integer(substr(gregorian_date, 1, 4))) |>
  select(greg_year) |>
  distinct() |>
  left_join(idb |> select(greg_year, idb_under2_national), by = "greg_year") |>
  group_by(greg_year) |>
  summarise(
    idb_under2_national = mean(idb_under2_national, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(wbp_pct_urban, by = "greg_year") |>
  filter(greg_year >= 2018, greg_year <= 2025) |>
  mutate(
    urban_denom = idb_under2_national * pct_urban,
    rural_denom = idb_under2_national * (1 - pct_urban)
  ) |>
  pivot_longer(
    c(urban_denom, rural_denom),
    names_to = "urban_rural",
    values_to = "idb_split_denom"
  ) |>
  mutate(
    urban_rural = factor(
      if_else(urban_rural == "urban_denom", "Urban", "Rural"),
      levels = c("Urban", "Rural")
    ),
    pct_urban = if_else(urban_rural == "Rural", 1 - pct_urban, pct_urban)
  )


coverage_idb_split <- nut_annual |>
  filter(greg_year >= 2018, greg_year <= 2025) |>
  left_join(
    idb_split_denom |> select(greg_year, urban_rural, idb_split_denom),
    by = c("greg_year", "urban_rural")
  ) |>
  mutate(
    coverage_pct = round(monthly_mean / idb_split_denom * 100, 1),
    urban_rural = factor(urban_rural, levels = c("Urban", "Rural"))
  )

# Coverage using WBP-Total indicator × WBP urban/rural proportions × IDB proportion
coverage_wbp_total <- nut_annual |>
  filter(greg_year >= 2018, greg_year <= 2025) |>
  left_join(
    wbp_total_indicator |>
      left_join(wbp_pct_urban, by = "greg_year") |>
      left_join(idb |> select(greg_year, idb_proportion), by = "greg_year") |>
      mutate(
        Urban = wbp_total * pct_urban * idb_proportion / 100,
        Rural = wbp_total * (1 - pct_urban) * idb_proportion / 100
      ) |>
      pivot_longer(
        c(Urban, Rural),
        names_to = "urban_rural",
        values_to = "wbp_total_under2"
      ),
    by = c("greg_year", "urban_rural")
  ) |>
  mutate(
    coverage_pct = round(monthly_mean / wbp_total_under2 * 100, 1),
    urban_rural = factor(urban_rural, levels = c("Urban", "Rural"))
  )

cat("GMP coverage – IDB national under-2 split by WBP % urban:\n")
print(
  as.data.frame(
    coverage_idb_split |>
      select(
        greg_year,
        urban_rural,
        monthly_mean,
        idb_split_denom,
        coverage_pct
      )
  ),
  row.names = FALSE
)
cat("\n")

cat("GMP coverage – original WBP labels:\n")
print(
  as.data.frame(
    coverage_annual |>
      select(greg_year, urban_rural, monthly_mean, idb_under2_est, coverage_pct)
  ),
  row.names = FALSE
)
cat("\nGMP coverage – flipped WBP labels:\n")
print(
  as.data.frame(
    coverage_annual_flipped |>
      select(greg_year, urban_rural, monthly_mean, idb_under2_est, coverage_pct)
  ),
  row.names = FALSE
)
cat("\n")

# *-------------------------------- CSV export --------------------------------*
# One row per (gregorian_year, urban_rural). Columns:
#   - WBP population (labels corrected) + share
#   - IDB total population + urban/rural split
#   - IDB under-2 proportion + national under-2 estimate
#   - Denom 1 (wbp_urban_rural): WBP urban_rural data element × IDB proportion
#   - Denom 2 (idb_split):       IDB national under-2 × WBP urban/rural share
#   - Denom 3 (wbp_total):       WBP-Total indicator × WBP urban/rural share × IDB proportion
#   - NUT monthly mean + urban/rural share
#   - GMP participation %
#   - Coverage % under all three denominators

# WBP population by gregorian year, labels corrected
wbp_greg <- nat |>
  filter(data_element_name == "WBP-Total population-Aggregated") |>
  mutate(
    greg_year = as.integer(substr(gregorian_date, 1, 4)),
    urban_rural = if_else(urban_rural == "Urban", "Rural", "Urban")
  ) |>
  group_by(greg_year, urban_rural) |>
  summarise(
    wbp_pop = mean(as.numeric(value), na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(greg_year) |>
  mutate(
    wbp_pop_pct = round(wbp_pop / sum(wbp_pop) * 100, 1),
    pct_urban = wbp_pop[urban_rural == "Urban"] / sum(wbp_pop)
  ) |>
  ungroup()

# NUT monthly mean by gregorian year
nut_greg <- nut |>
  mutate(greg_year = as.integer(substr(gregorian_date, 1, 4))) |>
  group_by(greg_year, eth_month, urban_rural) |>
  summarise(monthly_total = sum(value, na.rm = TRUE), .groups = "drop") |>
  group_by(greg_year, urban_rural) |>
  summarise(
    nut_monthly_mean = mean(monthly_total, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(greg_year) |>
  mutate(
    nut_share_pct = round(nut_monthly_mean / sum(nut_monthly_mean) * 100, 1)
  ) |>
  ungroup()

# GMP participation is already keyed by greg_year — rename for the join
gmp_greg <- gmp_pct_annual |> rename(gmp_participation_pct = gmp_pct)

# Build master CSV — one row per (gregorian_year, urban_rural)
summary_csv <- wbp_greg |>
  select(greg_year, urban_rural, wbp_pop, wbp_pop_pct, pct_urban) |>
  left_join(idb, by = "greg_year") |>
  left_join(wbp_total_indicator, by = "greg_year") |>
  mutate(
    idb_pop_split = idb_pop *
      if_else(urban_rural == "Urban", pct_urban, 1 - pct_urban),
    wbp_total_pop = wbp_total *
      if_else(urban_rural == "Urban", pct_urban, 1 - pct_urban),
    denom_1_wbp_urban_rural = wbp_pop * idb_proportion / 100,
    denom_2_idb_split = idb_under2_national *
      if_else(urban_rural == "Urban", pct_urban, 1 - pct_urban),
    denom_3_wbp_total = wbp_total *
      if_else(urban_rural == "Urban", pct_urban, 1 - pct_urban) *
      idb_proportion /
      100
  ) |>
  left_join(nut_greg, by = c("greg_year", "urban_rural")) |>
  left_join(gmp_greg, by = "greg_year") |>
  mutate(
    wbp_pop_pct = wbp_pop_pct / 100,
    idb_proportion = idb_proportion / 100,
    nut_coverage_wbp_urban_rural = round(
      nut_monthly_mean / denom_1_wbp_urban_rural * 100,
      1
    ),
    nut_coverage_idb = round(nut_monthly_mean / denom_2_idb_split * 100, 1),
    nut_coverage_wbp_total = round(
      nut_monthly_mean / denom_3_wbp_total * 100,
      1
    )
  ) |>
  filter(greg_year >= 2018, greg_year <= 2025) |>
  arrange(greg_year, urban_rural) |>
  select(
    greg_year,
    urban_rural,
    wbp_urban_rural_pop = wbp_pop,
    wbp_urban_rural_pop_pct = wbp_pop_pct,
    wbp_total_pop,
    worldbank_pop_pct = pct_urban,
    idb_pop_split,
    idb_proportion,
    denom_1_wbp_urban_rural,
    denom_2_idb_split,
    denom_3_wbp_total,
    nut_monthly_mean,
    nut_share_pct,
    gmp_participation_pct,
    nut_coverage_wbp_urban_rural,
    nut_coverage_idb,
    nut_coverage_wbp_total
  )

write_csv(summary_csv, file.path(out_dir, "gmp_urban_rural_summary.csv"))
cat("Saved gmp_urban_rural_summary.csv (", nrow(summary_csv), "rows)\n\n")
print(as.data.frame(summary_csv), row.names = FALSE)
cat("\n")

# *----------------------------------- plot -----------------------------------*
TEXT_INCREASE <- 0

save_plot <- function(p, filename, width = 10, height = 6) {
  ggsave(
    file.path(out_dir, filename),
    p,
    width = width,
    height = height,
    dpi = 150
  )
  cat("Saved", filename, "\n")
}

URBAN_RURAL_COLOURS <- c(Urban = "#2166AC", Rural = "#4DAC26")
GMP_COLOUR <- "#B8860B"

plot_df <- nut_annual |>
  filter(greg_year >= 2018, greg_year <= 2025) |>
  mutate(urban_rural = factor(urban_rural, levels = c("Urban", "Rural")))

# *--------------- shared helper: add GMP participation overlay ---------------*
# left_max:    maximum of the left axis (in the same units as the plot's y)
# left_labels: label formatter for left axis (e.g. \(x) paste0(x, "M") or "%")
add_gmp_overlay <- function(p, left_max, left_labels = \(x) paste0(x, "M")) {
  gmp_scale <- left_max / 100
  suppressWarnings(
    p +
      geom_line(
        data = gmp_pct_annual,
        mapping = aes(
          x = greg_year,
          y = gmp_pct * gmp_scale,
          colour = "GMP participation"
        ),
        linewidth = 0.9,
        linetype = "dashed",
        inherit.aes = FALSE
      ) +
      scale_colour_manual(
        values = c(URBAN_RURAL_COLOURS, "GMP participation" = GMP_COLOUR)
      ) +
      scale_y_continuous(
        labels = left_labels,
        sec.axis = sec_axis(
          ~ . / gmp_scale,
          name = "GMP participation (%)",
          labels = \(x) paste0(x, "%")
        )
      )
  )
}

gmp_theme <- function(p) {
  format_gg_plot(p, text_increase = TEXT_INCREASE) +
    theme(
      axis.text.y.right = element_text(colour = GMP_COLOUR),
      axis.title.y.right = element_text(colour = GMP_COLOUR),
      legend.position = "inside",
      legend.position.inside = c(0.12, 0.85)
    )
}

# *----------------------- panel B: urban/rural share % -----------------------*
p_pct <- ggplot(plot_df, aes(x = greg_year, y = pct, colour = urban_rural)) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = URBAN_RURAL_COLOURS) +
  scale_x_continuous(breaks = 2018:2025) +
  scale_y_continuous(limits = c(0, 100), labels = \(x) paste0(x, "%")) +
  labs(
    title = "Urban vs Rural Share of Children <2 Weighed during GMP Session",
    x = "Gregorian year",
    y = "Share of total weighed (%)",
    colour = NULL
  )

p_pct <- format_gg_plot(p_pct, text_increase = TEXT_INCREASE) +
  theme(legend.position = "inside", legend.position.inside = c(0.25, 0.5))
save_plot(p_pct, "panel_pct_share.png")

# Second version: overlay population proportion as dotted lines.
# Combine weighed share and population proportion into one long dataframe so
# linetype maps cleanly to a single legend entry per series type.
wbp_pop_pct_df <- wbp_annual |>
  filter(greg_year >= 2018, greg_year <= 2025) |>
  mutate(
    urban_rural = if_else(urban_rural == "Urban", "Rural", "Urban"),
    urban_rural = factor(urban_rural, levels = c("Urban", "Rural"))
  )

wb_pop_pct_df <- read_csv(
  here::here("data", "worldbank", "urban_rural_percentage.csv"),
  show_col_types = FALSE
) |>
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "greg_year",
    values_to = "urban_pct"
  ) |>
  mutate(greg_year = as.integer(greg_year)) |>
  filter(!is.na(urban_pct), greg_year >= 2018, greg_year <= 2025) |>
  transmute(
    greg_year,
    Urban = round(urban_pct, 1),
    Rural = round(100 - urban_pct, 1)
  ) |>
  pivot_longer(
    c(Urban, Rural),
    names_to = "urban_rural",
    values_to = "value"
  ) |>
  mutate(
    urban_rural = factor(urban_rural, levels = c("Urban", "Rural")),
    series = "World Bank population share"
  )

pct_combined <- bind_rows(
  plot_df |>
    select(greg_year, urban_rural, value = pct) |>
    mutate(series = "Share of children weighed"),
  wb_pop_pct_df |>
    select(greg_year, urban_rural, value, series)
)

p_pct_with_pop <- ggplot(
  pct_combined,
  aes(x = greg_year, y = value, colour = urban_rural, linetype = series)
) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = URBAN_RURAL_COLOURS, name = NULL) +
  scale_linetype_manual(
    values = c(
      "Share of children weighed" = "solid",
      "World Bank population share" = "dashed"
    ),
    name = NULL
  ) +
  scale_x_continuous(breaks = 2018:2025) +
  scale_y_continuous(limits = c(0, 100), labels = \(x) paste0(x, "%")) +
  labs(
    title = "Urban vs Rural Share of Children <2 Weighed during GMP Session",
    x = "Gregorian year",
    y = "Share (%)"
  )

p_pct_with_pop <- format_gg_plot(
  p_pct_with_pop,
  text_increase = TEXT_INCREASE
) +
  theme(legend.position = "inside", legend.position.inside = c(0.2, 0.5))
save_used_figure(
  p_pct_with_pop,
  out_dir,
  "panel_pct_share_with_pop.png",
  cols = c("greg_year", "urban_rural", "series", "value"),
  width = 10,
  height = 6,
  dpi = 150
)

# *------------------------- panel A: raw GMP counts --------------------------*
p_abs_base <- ggplot(
  plot_df,
  aes(x = greg_year, y = monthly_mean / 1e6, colour = urban_rural)
) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = URBAN_RURAL_COLOURS) +
  scale_x_continuous(breaks = 2018:2025) +
  scale_y_continuous(limits = c(0, 3), labels = \(x) paste0(x, "M")) +
  labs(
    title = "Children <2 Years Weighed during GMP Session",
    x = "Gregorian year",
    y = "Children weighed (millions)",
    colour = NULL
  )

p_abs_no_gmp <- format_gg_plot(p_abs_base, text_increase = TEXT_INCREASE) +
  theme(legend.position = "inside", legend.position.inside = c(0.1125, 0.875))

save_used_figure(
  p_abs_no_gmp,
  out_dir,
  "panel_abs_no_gmp.png",
  cols = c("greg_year", "urban_rural", "monthly_mean"),
  width = 10,
  height = 6,
  dpi = 150
)

p_abs_gmp <- gmp_theme(add_gmp_overlay(
  p_abs_base,
  max(plot_df$monthly_mean / 1e6, na.rm = TRUE)
))
save_plot(p_abs_gmp, "panel_abs_with_gmp.png")

# *----------------------- Total population split plots -----------------------*
POP_COLOURS <- c(URBAN_RURAL_COLOURS, Total = "#888888")

make_pop_split_plot <- function(df_long, subtitle) {
  ggplot(df_long, aes(x = greg_year, y = pop / 1e6, colour = urban_rural)) +
    geom_line(linewidth = 0.9) +
    scale_colour_manual(values = POP_COLOURS) +
    scale_x_continuous(breaks = 2018:2025) +
    scale_y_continuous(limits = c(0, NA), labels = \(x) paste0(x, "M")) +
    labs(
      title = "Total Population – Urban vs Rural Split",
      subtitle = subtitle,
      x = "Gregorian year",
      y = "Population (millions)",
      colour = NULL
    )
}

# Version 1: WBP total population with labels corrected (flip Urban ↔ Rural)
wbp_v1 <- wbp_annual |>
  filter(greg_year >= 2018, greg_year <= 2025) |>
  mutate(
    urban_rural = if_else(urban_rural == "Urban", "Rural", "Urban"),
    urban_rural = factor(urban_rural, levels = c("Urban", "Rural", "Total"))
  )

wbp_v1_total <- wbp_v1 |>
  group_by(greg_year) |>
  summarise(pop = sum(pop), .groups = "drop") |>
  mutate(urban_rural = factor("Total", levels = c("Urban", "Rural", "Total")))

p_pop_v1 <- make_pop_split_plot(
  bind_rows(wbp_v1, wbp_v1_total),
  subtitle = "WBP urban_rural: data element split (urban/rural labels corrected)"
)
p_pop_v1 <- format_gg_plot(p_pop_v1, text_increase = TEXT_INCREASE) +
  theme(legend.position = "inside", legend.position.inside = c(0.12, 0.15))
save_plot(p_pop_v1, "wbp_population_split.png")

# Version 2: IDB total population × corrected WBP urban/rural proportions
idb_pop_annual <- nat |>
  filter(data_element_name == "WBP-Total population-Aggregated") |>
  mutate(greg_year = as.integer(substr(gregorian_date, 1, 4))) |>
  select(greg_year) |>
  distinct() |>
  left_join(idb |> select(greg_year, idb_pop), by = "greg_year") |>
  group_by(greg_year) |>
  summarise(idb_pop = mean(idb_pop, na.rm = TRUE), .groups = "drop") |>
  left_join(wbp_pct_urban, by = "greg_year") |>
  filter(greg_year >= 2018, greg_year <= 2025) |>
  mutate(
    Urban = idb_pop * pct_urban,
    Rural = idb_pop * (1 - pct_urban),
    Total = idb_pop
  ) |>
  pivot_longer(
    c(Urban, Rural, Total),
    names_to = "urban_rural",
    values_to = "pop"
  ) |>
  mutate(
    urban_rural = factor(urban_rural, levels = c("Urban", "Rural", "Total"))
  )

p_pop_v2 <- make_pop_split_plot(
  idb_pop_annual,
  subtitle = "IDB total population × corrected WBP urban/rural share"
)
p_pop_v2 <- format_gg_plot(p_pop_v2, text_increase = TEXT_INCREASE) +
  theme(legend.position = "inside", legend.position.inside = c(0.12, 0.15))
save_plot(p_pop_v2, "wbp_population_split_idb.png")

# Version 3: WBP-Total population indicator × corrected WBP urban/rural proportions
# wbp_total_indicator is already computed above (before coverage section)
wbp_total_pop_split <- wbp_total_indicator |>
  left_join(wbp_pct_urban, by = "greg_year") |>
  mutate(
    Urban = wbp_total * pct_urban,
    Rural = wbp_total * (1 - pct_urban),
    Total = wbp_total
  ) |>
  pivot_longer(
    c(Urban, Rural, Total),
    names_to = "urban_rural",
    values_to = "pop"
  ) |>
  mutate(
    urban_rural = factor(urban_rural, levels = c("Urban", "Rural", "Total"))
  )

p_pop_v3 <- make_pop_split_plot(
  wbp_total_pop_split,
  subtitle = "WBP total: WBP-Total population indicator × WBP urban/rural share"
)
p_pop_v3 <- format_gg_plot(p_pop_v3, text_increase = TEXT_INCREASE) +
  theme(legend.position = "inside", legend.position.inside = c(0.12, 0.15))
save_plot(p_pop_v3, "wbp_population_split_wbp_total.png")

# *----------------------- panel A: GMP coverage plots ------------------------*
pct_labels <- \(x) paste0(x, "%")

make_coverage_panel <- function(
  cov_df,
  subtitle = "Denominator: WBP population × IDB proportion (labels corrected)"
) {
  ggplot(cov_df, aes(x = greg_year, y = coverage_pct, colour = urban_rural)) +
    geom_line(linewidth = 0.9) +
    geom_hline(
      yintercept = 75,
      linetype = "dotted",
      linewidth = 0.7,
      colour = "grey30"
    ) +
    annotate(
      "text",
      x = 2018,
      y = 77,
      label = "National standard",
      hjust = 0,
      vjust = 0,
      size = 4,
      colour = "grey30"
    ) +
    scale_colour_manual(values = URBAN_RURAL_COLOURS) +
    scale_x_continuous(breaks = 2018:2025) +
    scale_y_continuous(limits = c(0, 100), labels = pct_labels) +
    labs(
      title = "National GMP coverage estimates for children <2 years",
      subtitle = subtitle,
      x = "Gregorian year",
      y = "GMP coverage (%)",
      colour = NULL
    )
}

# Panel 1: WBP urban_rural — WBP data element (urban/rural disaggregated) × IDB proportion
p_coverage_wbp_ur <- make_coverage_panel(
  coverage_annual_flipped,
  subtitle = "Denominator: WBP data element urban/rural × IDB proportion"
)
p_coverage_wbp_ur <- format_gg_plot(
  p_coverage_wbp_ur,
  text_increase = TEXT_INCREASE
) +
  theme(legend.position = "inside", legend.position.inside = c(0.88, 0.88))
save_plot(p_coverage_wbp_ur, "panel_wbp_urban_rural_no_gmp.png")

# Panel 2: IDB — IDB national under-2 estimate × WBP urban/rural share
p_coverage_idb_split_base <- make_coverage_panel(
  coverage_idb_split,
  subtitle = "Denominator: IDB national under-2 estimate × WBP urban/rural share"
)
p_coverage_idb_split_no_gmp <- format_gg_plot(
  p_coverage_idb_split_base,
  text_increase = TEXT_INCREASE
) +
  theme(legend.position = "inside", legend.position.inside = c(0.88, 0.88))
save_plot(p_coverage_idb_split_no_gmp, "panel_idb_split_no_gmp.png")

# Panel 3: WBP total — WBP-Total indicator × WBP urban/rural share × IDB proportion
p_coverage_wbp_total <- make_coverage_panel(
  coverage_wbp_total,
  subtitle = "Denominator: scenario 2 × World Bank (urban/rural) proportion"
)
p_coverage_wbp_total <- format_gg_plot(
  p_coverage_wbp_total,
  text_increase = TEXT_INCREASE
) +
  theme(legend.position = "inside", legend.position.inside = c(0.9, 0.9))
save_used_figure(
  p_coverage_wbp_total,
  out_dir,
  "panel_wbp_total_no_gmp.png",
  cols = c("greg_year", "urban_rural", "coverage_pct"),
  width = 10,
  height = 6,
  dpi = 150
)

# *------------------------------ combined plots ------------------------------*
gmp_caption <- "Sources: DHIS2 GMP data elements; IDB under-2 population estimates"

save_plot(
  (p_abs_no_gmp / p_pct_with_pop) +
    plot_annotation(
      caption = "Source: DHIS2 GMP data elements, national level"
    ),
  "timeseries_urban_rural.png",
  width = 10,
  height = 12
)

# *============================================================================*
