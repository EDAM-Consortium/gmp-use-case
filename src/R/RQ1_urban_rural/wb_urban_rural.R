# *================= World Bank Urban/Rural Population Share ==================*
# Plots Ethiopia's urban vs rural share of total population from the World Bank
# urban_rural_percentage.csv (SP.URB.TOTL.IN.ZS indicator).
#
# Outputs in src/R/outputs/RQ1_urban_rural/:
#   not_used/wb_urban_rural/
#     wb_urban_rural_pct.png
#   used/wb_urban_rural/ (slide figures, each with a CSV of its plotted data)
#     wb_urban_rural_pct_notitle.png
# *============================================================================*
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

source(here::here("src", "R", "plotting_helpers.R"))

wb_file <- here::here(
  "data",
  "worldbank",
  "urban_rural_percentage.csv"
)
out_dir <- here::here("src", "R", "outputs", "RQ1_urban_rural", "not_used", "wb_urban_rural")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

TEXT_INCREASE <- 0
URBAN_RURAL_COLOURS <- c(Urban = "#2166AC", Rural = "#4DAC26")

wb_long <- read_csv(wb_file, show_col_types = FALSE) |>
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "year",
    values_to = "urban_pct"
  ) |>
  mutate(year = as.integer(year)) |>
  filter(!is.na(urban_pct)) |>
  select(year, urban_pct) |>
  mutate(Urban = round(urban_pct, 1), Rural = round(100 - urban_pct, 1)) |>
  select(year, Urban, Rural) |>
  pivot_longer(c(Urban, Rural), names_to = "urban_rural", values_to = "pct") |>
  mutate(urban_rural = factor(urban_rural, levels = c("Urban", "Rural")))

p <- ggplot(
  wb_long |> filter(year >= 2005, year <= 2025),
  aes(x = year, y = pct, colour = urban_rural)
) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = URBAN_RURAL_COLOURS) +
  scale_x_continuous(breaks = seq(2005, 2025, by = 2)) +
  scale_y_continuous(limits = c(0, 100), labels = \(x) paste0(x, "%")) +
  labs(
    title = "Urban vs Rural Share of Total Population",
    subtitle = "World Bank estimates",
    x = "Gregorian year",
    y = "Share of total population (%)",
    colour = NULL
  )

p <- format_gg_plot(p, text_increase = TEXT_INCREASE) +
  theme(
    legend.position = c(0.08, 0.5),
    legend.justification = c("left", "center"),
    legend.background = element_rect(
      fill = "white",
      colour = "grey80",
      linewidth = 0.3
    )
  )

PLOT_W <- 10
PLOT_H <- 6
PLOT_DPI <- 150

ggsave(
  file.path(out_dir, "wb_urban_rural_pct.png"),
  p,
  width = PLOT_W,
  height = PLOT_H,
  dpi = PLOT_DPI
)
message("Saved wb_urban_rural_pct.png")

save_used_figure(
  p + labs(title = NULL, subtitle = NULL),
  out_dir,
  "wb_urban_rural_pct_notitle.png",
  cols = c("year", "urban_rural", "pct"),
  width = PLOT_W,
  height = PLOT_H,
  dpi = PLOT_DPI
)
message("Saved wb_urban_rural_pct_notitle.png")

# *============================================================================*
