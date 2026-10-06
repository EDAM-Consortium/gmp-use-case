# *=============== GMP regional maps - two scenarios over time ================*
# Produces regional choropleth maps for two coverage scenarios at adm1 level.
#
#   Scenario 1: DHIS2 indicator  - NUT_ % of Children < 2 years in GMP
#               Source: malnutrition_ind_level2_Regional.csv
#
#   Scenario 2: WBP denominator  - NUT weighed / (WBP pop × IDB proportion)
#               Source: malnutrition_de_level2_Regional.csv + idb_under2_proportion
#
# OCHA join uses normalised fuzzy name matching (from generate_ocha_gmp_shapes.R).
# Coverage is computed as national totals (no urban/rural split).
#
# Outputs in src/R/outputs/RQ2_RQ3_national_level_and_geographic_variation/:
#   not_used/gmp_regional_maps/
#     regional_coverage.csv         - per (region × year), both scenarios
#     scenario1_map_by_year.png     - faceted over all years
#   used/gmp_regional_maps/ (slide figures, each with a CSV of its plotted data)
#     scenario2_map_by_year.png     - faceted over all years
#     scenario1_map_2025.png        - single map for Gregorian 2025
#     scenario2_map_2025.png        - single map for Gregorian 2025
# *============================================================================*

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(sf)
library(stringr)
library(stringdist)
library(scales)
library(ggrepel)

source(here::here("src", "R", "plotting_helpers.R"))

# *---------------------------------- paths -----------------------------------*
de_dir <- here::here("data", "GMP", "data_elements")
ind_dir <- here::here("data", "GMP", "indicators")
ocha_dir <- here::here("data", "shapefiles", "OCHA")
idb_prop_path <- here::here("src", "R", "outputs", "data_preparation", "not_used", "idb_under2", "idb_under2_proportion.csv")
out_dir <- here::here("src", "R", "outputs", "RQ2_RQ3_national_level_and_geographic_variation", "not_used", "gmp_regional_maps")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

NUT_VAR <- "NUT_Children <2 Years Weighted during GMP Session"
WBP_VAR <- "WBP-Total population"
IND_VAR <- "NUT_ % of Children < 2 years participated in GMP"
ZOOM_YEAR <- 2025L

# *------------------------- period → gregorian year --------------------------*
eth_to_greg_year <- function(period) {
  eth_year <- as.integer(substr(period, 1, 4))
  eth_month <- as.integer(substr(period, 5, 6))
  eth_year + if_else(eth_month <= 4L, 7L, 8L)
}

# *----------- OCHA name matching (from generate_ocha_gmp_shapes.R) -----------*
normalise <- function(x) {
  x |>
    str_trim() |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9 ]", " ") |>
    str_squish()
}

strip_suffixes <- function(x) {
  x |> str_remove_all("region|city administration|peoples") |> str_squish()
}

match_to_ocha <- function(dhis2_names, ocha_sf) {
  ocha_names <- ocha_sf$adm1_name
  ocha_pcodes <- ocha_sf$adm1_pcode

  gmp_norm <- normalise(dhis2_names)
  gmp_core <- strip_suffixes(gmp_norm)
  ocha_norm <- normalise(ocha_names)
  ocha_core <- strip_suffixes(ocha_norm)

  result <- tibble(
    org_unit_name = dhis2_names,
    adm1_name = NA_character_,
    adm1_pcode = NA_character_,
    match_step = NA_character_
  )

  # Step 1: exact normalised match
  exact <- match(gmp_norm, ocha_norm)
  hit <- !is.na(exact)
  result$adm1_name[hit] <- ocha_names[exact[hit]]
  result$adm1_pcode[hit] <- ocha_pcodes[exact[hit]]
  result$match_step[hit] <- "exact"

  # Step 2: suffix-stripped exact match
  todo <- which(is.na(result$adm1_name))
  stripped <- match(gmp_core[todo], ocha_core)
  hit2 <- !is.na(stripped)
  result$adm1_name[todo[hit2]] <- ocha_names[stripped[hit2]]
  result$adm1_pcode[todo[hit2]] <- ocha_pcodes[stripped[hit2]]
  result$match_step[todo[hit2]] <- "strip"

  # Step 3: fuzzy match on core names for anything still unmatched
  todo <- which(is.na(result$adm1_name))
  if (length(todo) > 0) {
    dist_mat <- stringdistmatrix(gmp_core[todo], ocha_core, method = "jw")
    best_idx <- apply(dist_mat, 1, which.min)
    best_dist <- apply(dist_mat, 1, min)
    fuzzy_hit <- best_dist < 0.2
    result$adm1_name[todo[fuzzy_hit]] <- ocha_names[best_idx[fuzzy_hit]]
    result$adm1_pcode[todo[fuzzy_hit]] <- ocha_pcodes[best_idx[fuzzy_hit]]
    result$match_step[todo[fuzzy_hit]] <- paste0(
      "fuzzy(",
      round(best_dist[fuzzy_hit], 3),
      ")"
    )
  }

  result
}

# *----------------------------------- load -----------------------------------*
idb_prop <- read_csv(idb_prop_path, show_col_types = FALSE) |>
  select(greg_year = year, idb_proportion = proportion)

ocha_adm1 <- st_read(file.path(ocha_dir, "eth_admin1.shp"), quiet = TRUE) |>
  st_make_valid() |>
  st_transform(4326)

# *---------- Build OCHA lookup from all distinct DHIS2 region names ----------*
dhis2_regions <- bind_rows(
  read_csv(
    file.path(ind_dir, "malnutrition_ind_level2_Regional.csv"),
    col_types = cols(.default = "c"),
    show_col_types = FALSE
  ) |>
    distinct(org_unit_name),
  read_csv(
    file.path(de_dir, "malnutrition_de_level2_Regional.csv"),
    col_types = cols(.default = "c"),
    show_col_types = FALSE
  ) |>
    distinct(org_unit_name)
) |>
  mutate(org_unit_name = str_trim(org_unit_name)) |>
  distinct()

ocha_lookup <- match_to_ocha(dhis2_regions$org_unit_name, ocha_adm1)

unmatched <- ocha_lookup |> filter(is.na(adm1_name))
if (nrow(unmatched) > 0) {
  cat("WARNING - unmatched DHIS2 region names:\n")
  print(unmatched)
}
cat("OCHA match summary:\n")
print(count(ocha_lookup, match_step))

# *------------ Scenario 1: indicator % (level 2) - no urban/rural ------------*
s1 <- read_csv(
  file.path(ind_dir, "malnutrition_ind_level2_Regional.csv"),
  col_types = cols(.default = "c"),
  show_col_types = FALSE
) |>
  filter(data_element_name == IND_VAR) |>
  mutate(
    value = as.numeric(value),
    greg_year = eth_to_greg_year(period),
    org_unit_name = str_trim(org_unit_name)
  ) |>
  left_join(
    ocha_lookup |> select(org_unit_name, adm1_name, adm1_pcode),
    by = "org_unit_name"
  ) |>
  filter(!is.na(adm1_name)) |>
  group_by(adm1_name, adm1_pcode, greg_year) |>
  summarise(scenario1_pct = mean(value, na.rm = TRUE), .groups = "drop")

# *-- Scenario 2: WBP coverage (level 2) - NUT from de, WBP from indicators ---*
nut_de <- read_csv(
  file.path(de_dir, "malnutrition_de_level2_Regional.csv"),
  col_types = cols(.default = "c"),
  show_col_types = FALSE
) |>
  filter(data_element_name == NUT_VAR) |>
  mutate(
    value = as.numeric(value),
    greg_year = eth_to_greg_year(period),
    org_unit_name = str_trim(org_unit_name)
  )

wbp_ind <- read_csv(
  file.path(ind_dir, "malnutrition_ind_level2_Regional.csv"),
  col_types = cols(.default = "c"),
  show_col_types = FALSE
) |>
  filter(data_element_name == WBP_VAR) |>
  mutate(
    value = as.numeric(value),
    greg_year = eth_to_greg_year(period),
    org_unit_name = str_trim(org_unit_name)
  )


nut <- nut_de |>
  select(
    data_element_name,
    period,
    greg_year,
    category_option_combo_name,
    org_unit_name,
    value
  )
nut <- nut |>
  group_by(data_element_name, period, greg_year, org_unit_name) |>
  summarise(value = sum(value, na.rm = TRUE))
s2 <- bind_rows(
  nut,
  wbp_ind |> select(data_element_name, period, greg_year, org_unit_name, value)
) |>
  mutate(
    value = as.numeric(value),
    greg_year = eth_to_greg_year(period),
    org_unit_name = str_trim(org_unit_name)
  ) |>
  left_join(
    ocha_lookup |> select(org_unit_name, adm1_name, adm1_pcode),
    by = "org_unit_name"
  ) |>
  filter(!is.na(adm1_name)) |>
  group_by(adm1_name, adm1_pcode, greg_year, data_element_name) |>
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = data_element_name, values_from = value) |>
  rename(nut = all_of(NUT_VAR), wbp = all_of(WBP_VAR)) |>
  left_join(idb_prop, by = "greg_year") |>
  mutate(
    denom_wbp = wbp * idb_proportion / 100,
    scenario2_pct = nut / denom_wbp * 100
  )

# *------------------------------- Combined CSV -------------------------------*
coverage_csv <- s2 |>
  select(
    adm1_name,
    adm1_pcode,
    greg_year,
    nut,
    wbp,
    idb_proportion,
    denom_wbp,
    scenario2_pct
  ) |>
  full_join(
    s1 |> select(adm1_pcode, greg_year, scenario1_pct),
    by = c("adm1_pcode", "greg_year")
  ) |>
  arrange(adm1_name, greg_year)

write_csv(coverage_csv, file.path(out_dir, "regional_coverage.csv"))
cat("Saved regional_coverage.csv (", nrow(coverage_csv), "rows)\n\n")

# *------------------------------- Map helpers --------------------------------*
all_years <- sort(unique(coverage_csv$greg_year[
  !is.na(coverage_csv$greg_year)
]))
all_years <- all_years[all_years >= 2018 & all_years <= 2025]

s1_years <- sort(unique(coverage_csv$greg_year[
  !is.na(coverage_csv$scenario1_pct)
]))
s1_years <- s1_years[s1_years >= 2018 & s1_years <= 2025]

s2_years <- sort(unique(coverage_csv$greg_year[
  !is.na(coverage_csv$scenario2_pct)
]))
s2_years <- s2_years[s2_years >= 2018 & s2_years <= 2025]

# Colour scale: red (0%) → orange → yellow (lower in range) → smooth transition
# into green starting at 75% → dark green (100%). No hard jump.
coverage_fill_scale <- scale_fill_gradientn(
  colours = c("#d73027", "#f46d43", "#fee08b", "#d9ef8b", "#91cf60", "#1a9641"),
  values = rescale(c(0, 28, 55, 75, 88, 100)),
  limits = c(0, 100),
  oob = squish,
  na.value = "grey88",
  labels = label_percent(scale = 1),
  name = "Coverage (%)"
)

OFFSET_REGIONS <- c("Addis Ababa", "Dire Dawa", "Harari", "Sidama")
CONTESTED_REGION <- "Tigray"

make_single_map <- function(
  df,
  fill_col,
  title,
  subtitle = NULL,
  annotate = FALSE
) {
  plot_sf <- ocha_adm1 |>
    left_join(df |> select(-any_of("adm1_name")), by = "adm1_pcode")

  p <- ggplot(plot_sf) +
    geom_sf(aes(fill = .data[[fill_col]]), colour = "white", linewidth = 0.3) +
    geom_sf(data = ocha_adm1, fill = NA, colour = "black", linewidth = 0.5) +
    coverage_fill_scale +
    labs(title = title, subtitle = subtitle) +
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

  if (annotate) {
    label_pts <- plot_sf |>
      st_centroid() |>
      mutate(
        .x = st_coordinates(geometry)[, 1],
        .y = st_coordinates(geometry)[, 2],
        .pct = if_else(
          !is.na(.data[[fill_col]]),
          paste0(round(.data[[fill_col]], 0), "%"),
          NA_character_
        ),
        .name_lbl = case_when(
          adm1_name == CONTESTED_REGION ~ paste0(adm1_name, " (contested)"),
          TRUE ~ adm1_name
        ),
        .lbl = if_else(is.na(.pct), .name_lbl, paste0(.name_lbl, "\n", .pct)),
        .offset = adm1_name %in% OFFSET_REGIONS
      ) |>
      st_drop_geometry()

    label_args <- list(
      size = 3.5,
      lineheight = 0.85,
      label.padding = unit(0.18, "lines"),
      label.r = unit(0.1, "lines"),
      fill = alpha("white", 0.78),
      colour = "black",
      segment.colour = "grey40",
      segment.size = 0.35,
      max.overlaps = Inf,
      seed = 42,
      hjust = 0.5,
      vjust = 0.5
    )

    p <- p +
      do.call(
        geom_label_repel,
        c(
          list(
            data = label_pts |> filter(!.offset),
            mapping = aes(x = .x, y = .y, label = .lbl),
            min.segment.length = Inf, # suppress connector lines - label stays near centroid
            box.padding = 0.15,
            point.padding = 0,
            force = 0.3,
            force_pull = 2
          ),
          label_args
        )
      ) +
      do.call(
        geom_label_repel,
        c(
          list(
            data = label_pts |> filter(.offset),
            mapping = aes(x = .x, y = .y, label = .lbl),
            min.segment.length = 0,
            box.padding = 0.5,
            point.padding = 0.2,
            nudge_x = 2.2,
            direction = "y",
            force = 0.5,
            force_pull = 1
          ),
          label_args
        )
      )
  }
  p
}

make_facet_map <- function(
  data_col,
  title_prefix,
  years,
  ncols = ceiling(sqrt(length(years)))
) {
  facet_df <- cross_join(ocha_adm1, tibble(greg_year = years)) |>
    left_join(
      coverage_csv |>
        select(-any_of("adm1_name")) |>
        select(adm1_pcode, greg_year, val = all_of(data_col)),
      by = c("adm1_pcode", "greg_year")
    )

  p <- ggplot(facet_df) +
    geom_sf(aes(fill = val), colour = "white", linewidth = 0.15) +
    geom_sf(data = ocha_adm1, fill = NA, colour = "black", linewidth = 0.35) +
    coverage_fill_scale +
    facet_wrap(~greg_year, ncol = ncols) +
    labs(title = paste(title_prefix, "- regional coverage over time")) +
    theme_void(base_size = 10) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 13,
        margin = margin(b = 8)
      ),
      plot.margin = margin(10, 10, 10, 10),
      strip.text = element_text(
        size = 9,
        face = "bold",
        margin = margin(b = 3)
      ),
      legend.position = "right"
    )

  panel_w <- 3
  list(
    plot = p,
    w = panel_w * ncols + 1.5,
    h = panel_w * ceiling(length(years) / ncols) + 1.5
  )
}

# *----------------------- Faceted maps over all years ------------------------*
s1_facet <- make_facet_map(
  "scenario1_pct",
  "Scenario 1: DHIS2 indicator",
  years = s1_years,
  ncols = 2
)
s2_facet <- make_facet_map(
  "scenario2_pct",
  "Scenario 2: WBP total",
  years = s2_years,
  ncols = 4
)

ggsave(
  file.path(out_dir, "scenario1_map_by_year.png"),
  s1_facet$plot,
  width = s1_facet$w,
  height = s1_facet$h,
  dpi = 180,
  limitsize = FALSE
)
cat("Saved scenario1_map_by_year.png\n")

save_used_figure(
  s2_facet$plot,
  out_dir,
  "scenario2_map_by_year.png",
  cols = c("adm1_name", "adm1_pcode", "greg_year", scenario2_pct = "val"),
  width = s2_facet$w,
  height = s2_facet$h,
  dpi = 180,
  limitsize = FALSE
)
cat("Saved scenario2_map_by_year.png\n")

# *----------------------- Single maps: Gregorian 2025 ------------------------*
data_2025 <- coverage_csv |> filter(greg_year == ZOOM_YEAR)

if (nrow(data_2025) == 0) {
  cat("No data for Gregorian", ZOOM_YEAR, "- skipping single-year maps\n")
} else {
  save_used_figure(
    make_single_map(
      data_2025,
      "scenario1_pct",
      title = "GMP coverage estimates for children <2 years",
      subtitle = paste0("Scenario 1 - DHIS2 indicator (", ZOOM_YEAR, ")"),
      annotate = TRUE
    ),
    out_dir,
    "scenario1_map_2025.png",
    cols = c("adm1_name", "adm1_pcode", "scenario1_pct"),
    width = 11,
    height = 9,
    dpi = 180
  )
  cat("Saved scenario1_map_2025.png\n")

  save_used_figure(
    make_single_map(
      data_2025,
      "scenario2_pct",
      title = "GMP coverage estimates for Children <2 years",
      subtitle = paste0(
        "Scenario 2: WBP total",
        " (",
        ZOOM_YEAR,
        ")"
      ),
      annotate = TRUE
    ),
    out_dir,
    "scenario2_map_2025.png",
    cols = c("adm1_name", "adm1_pcode", "scenario2_pct"),
    width = 11,
    height = 9,
    dpi = 180
  )
  cat("Saved scenario2_map_2025.png\n")
}

cat("\nDone. Outputs in", out_dir, "\n")

# *============================================================================*
