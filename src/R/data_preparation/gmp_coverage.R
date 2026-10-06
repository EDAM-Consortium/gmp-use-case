# *=============================== GMP coverage ===============================*
# Estimates GMP coverage = NUT_Children <2 Years Weighted / estimated under-2.
#
# Two denominator approaches:
#   (A) DHIS2: WBP-Total population-Aggregated × IDB proportion — urban/rural split kept
#   (B) IDB:   idb_under2_population.csv — national (adm0) and regional (adm1)
#
# Outputs in src/R/outputs/data_preparation/not_used/gmp_coverage/:
#   gmp_nut_coverage_idb.csv — (A) NUT rows with coverage added, urban/rural split
#   idb_gmp_nut.csv          — (B) NUT coverage using IDB under-2 denominator
# *============================================================================*

# *--------------------------------- imports ----------------------------------*
library(dplyr)
library(readr)
library(tidyr)

source(here::here("src", "R", "plotting_helpers.R"))

# *---------------------------------- paths -----------------------------------*
data_dir <- here::here("data", "GMP", "data_elements")
idb_prop <- here::here("src", "R", "outputs", "data_preparation", "not_used", "idb_under2", "idb_under2_proportion.csv")
idb_pop <- here::here("src", "R", "outputs", "data_preparation", "not_used", "idb_under2", "idb_under2_population.csv")
spatial_csv <- here::here(
  "src",
  "R",
  "outputs",
  "data_preparation",
  "not_used",
  "generate_dhis2_gmp_shapes",
  "gmp_spatial.csv"
)
out_dir <- here::here("src", "R", "outputs", "data_preparation", "not_used", "gmp_coverage")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

NUT_VAR <- "NUT_Children <2 Years Weighted during GMP Session"
WBP_VAR <- "WBP-Total population-Aggregated"
WBP_NONAGG_VAR <- "WBP-Total population-Not aggregated"
WBP_CF_VAR <- "WBP-CF Under‑2 years of age population Proportion"

# *----------------------------------- load -----------------------------------*
gmp_raw <- read_csv(
  list.files(
    data_dir,
    pattern = "malnutrition_de_level.*\\.csv",
    full.names = TRUE
  ),
  show_col_types = FALSE
) |>
  distinct() |>
  filter(
    data_element_name %in% c(NUT_VAR, WBP_VAR, WBP_NONAGG_VAR, WBP_CF_VAR)
  ) |>
  mutate(
    year = as.integer(substr(period, 1, 4)),
    gregorian_year = as.integer(format(as.Date(gregorian_date), "%Y")),
    value = as.numeric(value)
  )

proportion <- read_csv(idb_prop, show_col_types = FALSE) |>
  select(year, proportion, national_under2_est = under2_pop)

idb_under2 <- read_csv(idb_pop, show_col_types = FALSE)

# Manual lookup: idb_under2_population area names → OCHA adm1_name
# Bridges OCHA adm1_name (from gmp_spatial) ↔ IDB code (from idb_under2_population)
region_lookup <- tibble(
  adm1_name = c(
    "Addis Ababa",
    "Afar",
    "Amhara",
    "Benishangul-Gumuz",
    "Central Ethiopia",
    "Dire Dawa",
    "Gambela",
    "Harari",
    "Oromia",
    "Sidama",
    "Somali",
    "Tigray",
    "South Ethiopia",
    "South West Ethiopia"
  ),
  idb_code = c(
    "ET001",
    "ET005",
    "ET006",
    "ET007",
    "ET004",
    "ET003",
    "ET008",
    "ET002",
    "ET009",
    "ET013",
    "ET011",
    "ET012",
    "ET010",
    "ET014"
  )
)

# *---------------- (A) DHIS2 denominator — urban/rural split -----------------*
# Sum NUT across age combos per (org_unit, year, urban_rural).
# WBP has no age combos so is one row per (org_unit, year, urban_rural).
# Two sub-versions differ only in which proportion is used:
#   (A1) IDB national proportion (idb_under2_proportion.csv)
#   (A2) WBP-CF Under‑2 proportion from the GMP data itself

join_keys <- c(
  "org_unit_id",
  "org_unit_name",
  "org_unit_level",
  "org_unit_level_name",
  "period",
  "year",
  "gregorian_year",
  "urban_rural"
)

base_wide <- gmp_raw |>
  group_by(across(all_of(c(join_keys, "data_element_name")))) |>
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = data_element_name, values_from = value)

wbp_conflicts <- base_wide |>
  filter(!is.na(.data[[WBP_VAR]]), !is.na(.data[[WBP_NONAGG_VAR]])) |>
  select(
    all_of(join_keys),
    wbp_agg = all_of(WBP_VAR),
    wbp_nonagg = all_of(WBP_NONAGG_VAR)
  )

if (nrow(wbp_conflicts) > 0) {
  cat(
    "WARNING:",
    nrow(wbp_conflicts),
    "rows have both Aggregated and Non-Aggregated WBP values (Aggregated takes priority):\n"
  )
  print(wbp_conflicts)
}

base <- base_wide |>
  mutate(wbp = coalesce(.data[[WBP_VAR]], .data[[WBP_NONAGG_VAR]])) |>
  select(-all_of(c(WBP_VAR, WBP_NONAGG_VAR))) |>
  rename(
    nut = all_of(NUT_VAR),
    wbp_cf_proportion = all_of(WBP_CF_VAR)
  ) |>
  mutate(wbp_cf_proportion = wbp_cf_proportion * 0.0001)

# NUT %
gmp_nut_coverage_idb <- base |>
  left_join(proportion |> select(-national_under2_est), by = "year") |>
  mutate(
    idb_prop_under2_est = wbp * proportion / 100,
    idb_prop_nut_pct = nut / idb_prop_under2_est,
    wbp_prop_under2_est = wbp * wbp_cf_proportion,
    wbp_prop_nut_pct = nut / wbp_prop_under2_est
  ) |>
  arrange(org_unit_level, org_unit_name, year, urban_rural)

write_csv(gmp_nut_coverage_idb, file.path(out_dir, "gmp_nut_coverage_idb.csv"))
cat("Saved gmp_nut_coverage_idb (", nrow(gmp_nut_coverage_idb), "rows)\n")

# *---------------- (B) IDB denominator — national & regional -----------------*
# Both levels sourced from gmp_spatial.csv so spatial join metadata is consistent.
# National (level 1): sum across all org units per period; denominator = IDB adm0.
# Regional (level 2): sum per adm1_name per period; denominator = IDB adm1.

spatial <- read_csv(spatial_csv, show_col_types = FALSE) |>
  filter(data_element_name == NUT_VAR) |>
  mutate(value = as.numeric(value), year = as.integer(year)) |>
  select(org_unit_level, adm1_name, adm1_pcode, date, year, value)

# National denominator: use proportion file (2005–2026) — wider year coverage
# than idb_under2_population.csv which is limited to xlsx range (2015–2026).
idb_national <- proportion |>
  select(year, under2_est = national_under2_est)

idb_regional <- idb_under2 |>
  filter(adm_level == 1) |>
  select(idb_code = code, year, under2_est)

unmatched <- region_lookup |>
  anti_join(idb_regional, by = "idb_code") |>
  distinct(adm1_name, idb_code)
if (nrow(unmatched) > 0) {
  cat("region_lookup entries with no matching IDB code:\n")
  print(unmatched)
}

idb_national_coverage <- spatial |>
  filter(org_unit_level == 1) |>
  group_by(date, year) |>
  summarise(nut = sum(value, na.rm = TRUE), .groups = "drop") |>
  left_join(idb_national, by = "year") |>
  mutate(
    adm_level = 0L,
    idb_code = "ET",
    area = "Ethiopia",
    nut_pct = round(100 * nut / under2_est, 2)
  ) |>
  select(adm_level, idb_code, area, date, year, nut, under2_est, nut_pct)

idb_regional_coverage <- spatial |>
  filter(org_unit_level == 2, !is.na(adm1_name), adm1_name != "NA") |>
  group_by(adm1_name, date, year) |>
  summarise(nut = sum(value, na.rm = TRUE), .groups = "drop") |>
  left_join(region_lookup, by = "adm1_name") |>
  left_join(idb_regional, by = c("idb_code", "year")) |>
  mutate(
    adm_level = 1L,
    area = adm1_name,
    nut_pct = round(100 * nut / under2_est, 2)
  ) |>
  select(adm_level, idb_code, area, date, year, nut, under2_est, nut_pct)

idb_gmp_nut <- bind_rows(idb_national_coverage, idb_regional_coverage) |>
  arrange(adm_level, area, date)

write_csv(idb_gmp_nut, file.path(out_dir, "idb_gmp_nut.csv"))
cat("Saved idb_gmp_nut.csv (", nrow(idb_gmp_nut), "rows)\n")

cat("\nDone. Outputs in", out_dir, "\n")

# *============================================================================*
