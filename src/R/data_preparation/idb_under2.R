# *===================== IDB under-2 population estimates =====================*
# Derives the proportion of Ethiopia's population aged ≤2 for each year from
# the IDB single-year age pyramid, then applies that proportion to every area
# in Ethiopia.xlsx to produce ready-to-join under-2 estimates.
#
# Outputs in src/R/outputs/data_preparation/:
#   not_used/idb_under2/
#     idb_under2_proportion.csv      — national proportion per year (2005–2026)
#     idb_under2_population.csv      — under-2 estimates per (area, admin level, year)
#     idb_under2_proportion.png      — dual-axis chart: total pop + % aged ≤2
#     idb_under2_proportion_v2.png   — same + DHIS2 WBP total pop; split legend
#     idb_under2_proportion_only.png — under-2 proportion (%) alone
#     (the _v2 and _only plots are also saved without titles as *_notitle.png)
#   used/idb_under2/ (slide figures, each with a CSV of its plotted data)
#     idb_under2_proportion_notitle.png — dual-axis chart without titles
# *============================================================================*

# *--------------------------------- imports ----------------------------------*
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)

source(here::here("src", "R", "plotting_helpers.R"))

# *---------------------------------- paths -----------------------------------*
pyramid_csv <- here::here(
  "data",
  "IDB",
  "IDB_29-05-2026_pop_pyramid.csv"
)
eth_xlsx <- here::here("data", "IDB", "Ethiopia.xlsx")
out_dir <- here::here("src", "R", "outputs", "data_preparation", "not_used", "idb_under2")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# *----------------- 1. National proportion from age pyramid ------------------*
raw <- read_csv(pyramid_csv, show_col_types = FALSE) |>
  rename(year = Year, group = GROUP, population = Population) |>
  mutate(
    population = as.numeric(gsub(",", "", population)),
    year = as.integer(year)
  )

totals <- raw |>
  filter(is.na(group)) |>
  select(year, total_pop = population)

under2_national <- raw |>
  filter(group %in% c("0", "1", "2")) |>
  group_by(year) |>
  summarise(under2_pop = sum(population), .groups = "drop")

proportion <- inner_join(totals, under2_national, by = "year") |>
  mutate(proportion = round(under2_pop / total_pop * 100, 4)) |>
  arrange(year)

write_csv(proportion, file.path(out_dir, "idb_under2_proportion.csv"))
message("Saved idb_under2_proportion.csv (", nrow(proportion), " rows)")

# *-------------------- 2. Under-2 estimates for all areas --------------------*
# Apply the national proportion to each area's total population.
# Note: assumes proportionate age structure sub-nationally.
eth <- read_excel(eth_xlsx) |>
  select(
    adm_level = ADM_LEVEL,
    code = CODE,
    area = BASENAME,
    year = YR,
    pop = POP,
    pop_male = MPOP,
    pop_female = FPOP
  ) |>
  mutate(year = as.integer(year))

pyramid_years <- sort(unique(proportion$year))
xlsx_years <- sort(unique(eth$year))
kept_years <- intersect(pyramid_years, xlsx_years)
dropped_years <- setdiff(xlsx_years, pyramid_years)

message(
  "Population pyramid years: ",
  min(pyramid_years),
  "–",
  max(pyramid_years)
)
message("Ethiopia.xlsx years:      ", min(xlsx_years), "–", max(xlsx_years))
message(
  "Keeping ",
  length(kept_years),
  " overlapping years (",
  min(kept_years),
  "–",
  max(kept_years),
  ")"
)
if (length(dropped_years) > 0) {
  message(
    "Dropping xlsx years with no pyramid proportion: ",
    paste(dropped_years, collapse = ", ")
  )
}

under2_by_area <- eth |>
  filter(year %in% kept_years) |>
  left_join(proportion |> select(year, proportion), by = "year") |>
  mutate(
    under2_est = round(pop * proportion / 100),
    under2_est_male = round(pop_male * proportion / 100),
    under2_est_female = round(pop_female * proportion / 100)
  ) |>
  select(
    adm_level,
    code,
    area,
    year,
    pop,
    pop_male,
    pop_female,
    proportion,
    under2_est,
    under2_est_male,
    under2_est_female
  ) |>
  arrange(adm_level, area, year)

write_csv(under2_by_area, file.path(out_dir, "idb_under2_population.csv"))
message("Saved idb_under2_population.csv (", nrow(under2_by_area), " rows)")

# *------------------------- 3. Shared plot settings --------------------------*
TEXT_INCREASE <- 0
PLOT_W <- 12
PLOT_H <- 6
PLOT_DPI <- 150

legend_inside <- theme(
  legend.position = c(0.08, 0.75),
  legend.justification = c("left", "top"),
  legend.background = element_rect(
    fill = "white",
    colour = "grey80",
    linewidth = 0.3
  )
)

# *--------------------------------- 3. Plot ----------------------------------*
proportion_plot <- proportion |> filter(year <= 2025)

scale_factor <- max(proportion_plot$total_pop) / max(proportion_plot$proportion)
colour_pop <- "black"
colour_pct <- "#DC2626"

p <- proportion_plot |>
  ggplot(aes(x = year)) +
  geom_line(aes(y = total_pop, colour = "Total population"), linewidth = 0.9) +
  geom_line(
    aes(y = proportion * scale_factor, colour = "% aged ≤2"),
    linewidth = 0.9,
    linetype = "dashed"
  ) +
  scale_colour_manual(
    values = c("Total population" = colour_pop, "% aged ≤2" = colour_pct),
    name = NULL
  ) +
  scale_y_continuous(
    name = "Population",
    labels = \(x) paste0(round(x / 1e6, 0), "M"),
    sec.axis = sec_axis(
      transform = \(x) x / scale_factor,
      name = "% of total population",
      labels = \(x) paste0(round(x, 1), "%")
    )
  ) +
  scale_x_continuous(
    breaks = seq(min(proportion_plot$year), max(proportion_plot$year), by = 2),
    labels = seq(min(proportion_plot$year), max(proportion_plot$year), by = 2)
  ) +
  labs(
    title = "US Census Bureau’s IDB Ethiopian population projections",
    # subtitle = "",
    x = "Gregorian year",
    colour = NULL
  )

p <- format_gg_plot(p, cpal = NULL, text_increase = TEXT_INCREASE) +
  theme(
    axis.title.y = element_text(colour = colour_pop),
    axis.text.y = element_text(colour = colour_pop),
    axis.title.y.right = element_text(colour = colour_pct),
    axis.text.y.right = element_text(colour = colour_pct)
  ) +
  legend_inside

ggsave(
  file.path(out_dir, "idb_under2_proportion.png"),
  p,
  width = PLOT_W,
  height = PLOT_H,
  dpi = PLOT_DPI
)
message("Saved idb_under2_proportion.png")

save_used_figure(
  p + labs(title = NULL, subtitle = NULL),
  out_dir,
  "idb_under2_proportion_notitle.png",
  cols = c("year", "total_pop", "proportion"),
  width = PLOT_W,
  height = PLOT_H,
  dpi = PLOT_DPI
)
message("Saved idb_under2_proportion_notitle.png")

# *--------------- 4. Plot v2: DHIS2 WBP total population added ---------------*
# Adds the DHIS2 WBP-Total population as a blue line alongside IDB.
# Legend split: colour group "Total population" (DHIS2 + IDB solid lines);
# linetype group for the % aged ≤2 dashed reference line.

ind_file <- here::here(
  "data",
  "GMP",
  "indicators",
  "malnutrition_ind_level1_National.csv"
)

dhis2_pop <- read_csv(
  ind_file,
  col_types = cols(.default = "c"),
  show_col_types = FALSE
) |>
  filter(data_element_name == "WBP-Total population") |>
  mutate(
    value = as.numeric(value),
    eth_year = as.integer(substr(period, 1, 4)),
    eth_month = as.integer(substr(period, 5, 6)),
    year = eth_year + if_else(eth_month <= 4L, 7L, 8L)
  ) |>
  filter(year <= 2025) |>
  group_by(year) |>
  summarise(total_pop = mean(value, na.rm = TRUE), .groups = "drop")

# Recalculate scale_factor to cover both IDB and DHIS2 population ranges
scale_factor_v2 <- max(
  c(proportion_plot$total_pop, dhis2_pop$total_pop),
  na.rm = TRUE
) /
  max(proportion_plot$proportion)

colour_dhis2 <- "#2166ac"

p_v2 <- proportion_plot |>
  ggplot(aes(x = year)) +
  geom_line(
    aes(y = total_pop, colour = "IDB total population"),
    linewidth = 0.9
  ) +
  geom_line(
    aes(y = proportion * scale_factor_v2, colour = "% aged ≤2 (IDB)"),
    linewidth = 0.9,
    linetype = "dashed"
  ) +
  geom_line(
    data = dhis2_pop,
    aes(y = total_pop, colour = "DHIS2 total population"),
    linewidth = 0.9
  ) +
  scale_colour_manual(
    name = NULL,
    values = c(
      "IDB total population" = colour_pop,
      "DHIS2 total population" = colour_dhis2,
      "% aged ≤2 (IDB)" = colour_pct
    ),
    guide = guide_legend(
      override.aes = list(linetype = c("solid", "solid", "dashed"))
    )
  ) +
  scale_y_continuous(
    name = "Population",
    labels = \(x) paste0(round(x / 1e6, 0), "M"),
    sec.axis = sec_axis(
      transform = \(x) x / scale_factor_v2,
      name = "% of total population",
      labels = \(x) paste0(round(x, 1), "%")
    )
  ) +
  scale_x_continuous(
    breaks = seq(min(proportion_plot$year), max(proportion_plot$year), by = 2),
    labels = seq(min(proportion_plot$year), max(proportion_plot$year), by = 2)
  ) +
  labs(
    title = "Ethiopian population projections",
    x = "Year"
  )

p_v2 <- format_gg_plot(p_v2, cpal = NULL, text_increase = TEXT_INCREASE) +
  theme(
    axis.title.y = element_text(colour = colour_pop),
    axis.text.y = element_text(colour = colour_pop),
    axis.title.y.right = element_text(colour = colour_pct),
    axis.text.y.right = element_text(colour = colour_pct)
  ) +
  legend_inside

ggsave(
  file.path(out_dir, "idb_under2_proportion_v2.png"),
  p_v2,
  width = PLOT_W,
  height = PLOT_H,
  dpi = PLOT_DPI
)
message("Saved idb_under2_proportion_v2.png")

ggsave(
  file.path(out_dir, "idb_under2_proportion_v2_notitle.png"),
  p_v2 + labs(title = NULL, subtitle = NULL),
  width = PLOT_W,
  height = PLOT_H,
  dpi = PLOT_DPI
)
message("Saved idb_under2_proportion_v2_notitle.png")

# *-------------------- 5. Plot: under-2 proportion alone ---------------------*
p_pct <- proportion_plot |>
  ggplot(aes(x = year, y = proportion)) +
  geom_line(linewidth = 0.9, colour = colour_pct) +
  scale_x_continuous(
    breaks = seq(min(proportion_plot$year), max(proportion_plot$year), by = 2),
    labels = seq(min(proportion_plot$year), max(proportion_plot$year), by = 2)
  ) +
  scale_y_continuous(
    labels = \(x) paste0(round(x, 1), "%"),
    limits = c(0, NA)
  ) +
  labs(
    title = "IDB estimate: proportion of Ethiopia's population aged ≤2",
    x = "Year",
    y = "% of total population"
  )

p_pct <- format_gg_plot(p_pct, cpal = NULL, text_increase = TEXT_INCREASE)

ggsave(
  file.path(out_dir, "idb_under2_proportion_only.png"),
  p_pct,
  width = PLOT_W,
  height = PLOT_H,
  dpi = PLOT_DPI
)
message("Saved idb_under2_proportion_only.png")

ggsave(
  file.path(out_dir, "idb_under2_proportion_only_notitle.png"),
  p_pct + labs(title = NULL, subtitle = NULL),
  width = PLOT_W,
  height = PLOT_H,
  dpi = PLOT_DPI
)
message("Saved idb_under2_proportion_only_notitle.png")

# *============================================================================*
