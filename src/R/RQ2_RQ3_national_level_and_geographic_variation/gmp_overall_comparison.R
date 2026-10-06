# *================= GMP coverage — four-scenario comparison ==================*
# Compares four estimates of national GMP coverage over time (2018–2025).
#
#   Scenario 1: DHIS2 indicator  — NUT_ % of Children < 2 years in GMP
#   Scenario 2: WBP total        — NUT weighed / denom_3_wbp_total
#   Scenario 3: WBP urban/rural  — NUT weighed / denom_1_wbp_urban_rural
#   Scenario 4: IDB              — NUT weighed / denom_2_idb_split
#
# All denominators are summed to national totals from gmp_urban_rural_summary.csv.
#
# Outputs in src/R/outputs/RQ2_RQ3_national_level_and_geographic_variation/:
#   not_used/gmp_scenario_comparison/
#     scenario_comparison.csv      — data underlying all four scenarios
#     children_weighed.png         — national monthly mean children weighed
#   used/gmp_scenario_comparison/ (slide figures, each with a CSV of its plotted data)
#     scenario_comparison.png      — four-line coverage time series
#     denominator_comparison.png   — under-2 population estimates by denominator
# *============================================================================*

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(scales)

source(here::here("src", "R", "plotting_helpers.R"))

# *---------------------------------- Config ----------------------------------*
TEXT_INCREASE <- 0 # passed to format_gg_plot(); negative = smaller text
PLOT_WIDTH <- 10 # inches
PLOT_HEIGHT <- 6 # inches
# *----------------------------------------------------------------------------*

in_csv <- here::here("src", "R", "outputs", "RQ1_urban_rural", "not_used", "gmp_urban_rural", "gmp_urban_rural_summary.csv")
out_dir <- here::here("src", "R", "outputs", "RQ2_RQ3_national_level_and_geographic_variation", "not_used", "gmp_scenario_comparison")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(in_csv)) {
  stop("Run gmp_urban_rural_timeseries.R first to generate ", in_csv)
}

# Sum urban + rural rows to national totals
raw <- read_csv(in_csv, show_col_types = FALSE) |>
  group_by(greg_year) |>
  summarise(
    nut_monthly_mean = sum(nut_monthly_mean, na.rm = TRUE),
    denom_wbp_urban_rural = sum(denom_1_wbp_urban_rural, na.rm = TRUE),
    denom_idb = sum(denom_2_idb_split, na.rm = TRUE),
    denom_wbp_total = sum(denom_3_wbp_total, na.rm = TRUE),
    gmp_participation_pct = first(gmp_participation_pct),
    .groups = "drop"
  ) |>
  mutate(
    denom_dhis2 = nut_monthly_mean / (gmp_participation_pct / 100)
  )

# *-------------------------------- Scenarios ---------------------------------*
scenario1 <- raw |>
  transmute(
    greg_year,
    pct = gmp_participation_pct,
    scenario = "1: DHIS2 indicator"
  )

scenario2 <- raw |>
  transmute(
    greg_year,
    pct = round(nut_monthly_mean / denom_wbp_total * 100, 1),
    scenario = "2: WBP total"
  )

scenario3 <- raw |>
  transmute(
    greg_year,
    pct = round(nut_monthly_mean / denom_wbp_urban_rural * 100, 1),
    scenario = "3: WBP urban/rural"
  )

scenario4 <- raw |>
  transmute(
    greg_year,
    pct = round(nut_monthly_mean / denom_idb * 100, 1),
    scenario = "4: IDB"
  )

plot_data <- bind_rows(scenario1, scenario2, scenario3, scenario4) |>
  arrange(scenario, greg_year)

# *-------------------------------- CSV export --------------------------------*
csv_out <- raw |>
  mutate(
    scenario1_pct = round(gmp_participation_pct, 1),
    scenario2_pct = round(nut_monthly_mean / denom_wbp_total * 100, 1),
    scenario3_pct = round(nut_monthly_mean / denom_wbp_urban_rural * 100, 1),
    scenario4_pct = round(nut_monthly_mean / denom_idb * 100, 1)
  ) |>
  select(
    greg_year,
    nut_monthly_mean,
    denom_dhis2,
    denom_wbp_urban_rural,
    denom_wbp_total,
    denom_idb,
    gmp_participation_pct,
    scenario1_pct,
    scenario2_pct,
    scenario3_pct,
    scenario4_pct
  )

write_csv(csv_out, file.path(out_dir, "scenario_comparison.csv"))
cat("Saved scenario_comparison.csv (", nrow(csv_out), "rows)\n")

# *------------------------------ Coverage plot -------------------------------*
scenario_colours <- c(
  "1: DHIS2 indicator" = "#2166ac",
  "2: WBP total" = "#f4a742",
  "3: WBP urban/rural" = "#d6604d",
  "4: IDB" = "#4dac26"
)

p <- ggplot(plot_data, aes(greg_year, pct, colour = scenario)) +
  geom_line(linewidth = 0.9) +
  geom_hline(
    yintercept = 75,
    linetype = "dotted",
    colour = "grey40",
    linewidth = 0.6
  ) +
  scale_colour_manual(name = "Scenario", values = scenario_colours) +
  scale_x_continuous(breaks = sort(unique(plot_data$greg_year))) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    limits = c(0, 100),
    breaks = seq(0, 100, by = 25)
  ) +
  labs(
    title = "National GMP coverage estimates for children <2 years",
    x = "Gregorian year",
    y = "GMP coverage (%)"
  ) +
  theme_minimal(base_size = 12)

p <- format_gg_plot(p, text_increase = TEXT_INCREASE) +
  theme(
    legend.position = c(0.175, 0.55),
    legend.background = element_rect(
      fill = alpha("white", 0.85),
      colour = "grey80"
    ),
    legend.margin = margin(4, 8, 4, 8),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10, face = "bold")
  )

save_used_figure(
  p,
  out_dir,
  "scenario_comparison.png",
  cols = c("greg_year", "scenario", "pct"),
  width = PLOT_WIDTH,
  height = PLOT_HEIGHT
)
cat("Saved scenario_comparison.png\n")

# *----------------------- Denominator comparison plot ------------------------*
# Shows the three under-2 population denominators in absolute terms (millions)
# to illustrate how much the choice of denominator source affects coverage.
denom_long <- raw |>
  select(
    greg_year,
    "1: DHIS2 indicator" = denom_dhis2,
    "2: WBP total" = denom_wbp_total,
    "3: WBP urban/rural" = denom_wbp_urban_rural,
    "4: IDB" = denom_idb
  ) |>
  pivot_longer(-greg_year, names_to = "denominator", values_to = "under2_est")

p_denom <- ggplot(
  denom_long,
  aes(greg_year, under2_est / 1e6, colour = denominator)
) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(name = "Scenario", values = scenario_colours) +
  scale_x_continuous(breaks = sort(unique(denom_long$greg_year))) +
  scale_y_continuous(
    labels = \(x) paste0(x, "M"),
  ) +
  labs(
    title = "Children <2 population projections",
    x = "Gregorian year",
    y = "Children (millions)"
  ) +
  theme_minimal(base_size = 12)

p_denom <- format_gg_plot(p_denom, text_increase = TEXT_INCREASE) +
  theme(
    legend.position = c(0.175, 0.25),
    legend.background = element_rect(
      fill = alpha("white", 0.85),
      colour = "grey80"
    ),
    legend.margin = margin(4, 8, 4, 8),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10, face = "bold")
  )

save_used_figure(
  p_denom,
  out_dir,
  "denominator_comparison.png",
  cols = c("greg_year", scenario = "denominator", "under2_est"),
  width = PLOT_WIDTH,
  height = PLOT_HEIGHT
)
cat("Saved denominator_comparison.png\n")

# *-------------------------- Children weighed plot ---------------------------*
p_nut <- ggplot(raw, aes(greg_year, nut_monthly_mean / 1e6)) +
  geom_line(linewidth = 0.9, colour = "#2166ac") +
  scale_x_continuous(breaks = sort(unique(raw$greg_year))) +
  scale_y_continuous(
    labels = \(x) paste0(x, "M"),
    limits = c(0, NA)
  ) +
  labs(
    title = "Average Children <2 years weighed per month<br>during GMP sessions",
    x = "Gregorian year",
    y = "Children weighed (millions)"
  ) +
  theme_minimal(base_size = 11)

p_nut <- format_gg_plot(p_nut, text_increase = TEXT_INCREASE)

ggsave(
  file.path(out_dir, "children_weighed.png"),
  p_nut,
  width = PLOT_WIDTH,
  height = PLOT_HEIGHT,
)
cat("Saved children_weighed.png\n")

cat("\nDone. Outputs in", out_dir, "\n")

# *============================================================================*
