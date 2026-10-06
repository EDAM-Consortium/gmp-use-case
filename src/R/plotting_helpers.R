# *============================= Plotting helpers =============================*
# Shared ggplot utilities for the GMP analysis.
#
# Usage:
#   source(here::here("src", "R", "plotting_helpers.R"))
#   p <- ggplot(...) + geom_line()
#   format_gg_plot(p, cpal = "eth_levels")
# *============================================================================*

library(ggplot2)
library(ggtext) # element_markdown

# *----------------------------- colour palettes ------------------------------*
imperial_grey <- "#EBEBE0"

# Ethiopia admin level colours — consistent across all plots
ETH_LEVEL_COLOURS <- c(
  "National" = "#2563EB",
  "Regional" = "#16A34A",
  "Zonal" = "#D97706",
  "Woreda" = "#DC2626",
  "PHCU" = "#7C3AED",
  "Facilities" = "#0891B2"
)


# *------------------------------ format_gg_plot ------------------------------*
format_gg_plot <- function(
  ggplot,
  text_increase = 5,
  strip_increase = 5,
  cpal = NULL
) {
  if (!is.null(cpal)) {
    if (cpal == "ETH") {
      cpal = ETH_LEVEL_COLOURS
    }

    ggplot <- ggplot +
      scale_fill_manual(values = cpal) +
      scale_colour_manual(values = cpal)
  }

  ggplot +
    theme_bw() +
    theme(
      text = element_text(family = "Lato"),
      legend.position = "top",
      panel.border = element_blank(),
      axis.line = element_line(color = "black"),
      legend.key.size = unit(0.5, "cm"),
      legend.background = element_rect(
        colour = imperial_grey,
        linewidth = 0.4,
        fill = "white",
        linetype = "solid"
      ),
      panel.grid.major = element_line(colour = imperial_grey, linewidth = 0.3),
      panel.grid.minor = element_line(colour = imperial_grey, linewidth = 0.3),
      plot.margin = margin(10, 30, 10, 10),
      axis.text.x = element_text(size = 15 + text_increase),
      axis.title.x = element_text(size = 18 + text_increase),
      axis.text.y = element_text(size = 15 + text_increase),
      axis.title.y = element_text(size = 18 + text_increase),
      legend.title = element_text(size = 18 + text_increase, family = "Lato"),
      legend.text = element_text(size = 15 + text_increase, family = "Lato"),
      strip.text.x = element_text(size = 15 + strip_increase),
      plot.title = element_markdown(size = 20 + text_increase, hjust = 0.5),
      plot.subtitle = element_markdown(size = 18 + text_increase, hjust = 0.5),
      strip.background = element_rect(
        colour = "black",
        fill = "white",
        linewidth = 1
      )
    )
}

# *-------------------------------- labellers ---------------------------------*
# Maps org_unit_level (numeric or name string) to a readable label.
admin_level_labeller <- as_labeller(c(
  "0" = "National",
  "1" = "Regional",
  "2" = "Zonal",
  "3" = "Woreda",
  "4" = "PHCU",
  "5" = "Facilities"
))

# *------------------------- figures used in reports --------------------------*
# Saves a figure used in the "Use case update 8 June" slides, with a CSV of
# the data columns it plots. out_dir is the script's not_used/ folder; the
# figure goes to the matching used/ folder instead.
# cols: columns to keep; a named vector renames them (new = "old").
save_used_figure <- function(p, out_dir, filename, cols, data = p$data, ...) {
  stopifnot(grepl("/not_used/", out_dir, fixed = TRUE))
  used_dir <- sub("/not_used/", "/used/", out_dir, fixed = TRUE)
  dir.create(used_dir, showWarnings = FALSE, recursive = TRUE)
  ggsave(file.path(used_dir, filename), p, ...)
  if (inherits(data, "sf")) {
    data <- sf::st_drop_geometry(data)
  }
  readr::write_csv(
    dplyr::select(data, dplyr::all_of(cols)),
    file.path(used_dir, sub("\\.png$", ".csv", filename))
  )
  cat("Saved", file.path(used_dir, filename), "and its CSV\n")
}

# *============================================================================*
