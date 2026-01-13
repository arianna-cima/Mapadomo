# ==========================================================
#  LOAD & PREPARE DWELLING DATA
# ==========================================================

# --- Load dwelling data ---
stock <- fread(
  file.path(source_stock_path, "SE_building_13_24.csv"),
  encoding = "UTF-8"
)

# --- Map building types to house/flat ---
type_map <- c(
  "one- or two-dwelling buildings" = "house",
  "multi-dwelling buildings"       = "flat",
  "other buildings"                = "other",
  "special housing"                = "special"
)
stock$type.of.building <- type_map[stock$type.of.building]


# ----------------------------------------------------------
# Sum values by construction period and region
# ----------------------------------------------------------

# Step 1: Sum over type.of.building
stock_all <- stock[
  , .(value = sum(value)),
  by = .(year, observations, period.of.construction, region)
]

# Step 2: Mark aggregated rows as "all"
stock_all[, type.of.building := "all"]

# Step 3: Combine with original stock
stock <- rbind(stock, stock_all)


# ==========================================================
#  LOAD & MERGE LAU CODES
# ==========================================================

lau <- fread(
  file.path(source_lau_path, "SE_lau.csv"),
  encoding = "UTF-8"
)
lau$`LAU NAME NATIONAL` <- iconv(
  lau$`LAU NAME NATIONAL`, from = "Latin1", to = "UTF-8"
)

# --- Normalize region names ---
normalize_name <- function(x)
  stri_trans_general(x, "Latin-ASCII") %>% tolower()

stock$region_norm  <- normalize_name(stock$region)
lau$region_norm    <- normalize_name(lau$`LAU NAME NATIONAL`)

# --- Merge with LAU codes ---
stock_final <- stock %>%
  left_join(lau, by = "region_norm")

# Rename NUTS column
stock_final <- dplyr::rename(stock_final, nuts3 = `NUTS 3 CODE`)

setDT(stock_final)


# ==========================================================
#  RECODE CONSTRUCTION PERIOD GROUPS
# ==========================================================

stock_final[, period_group := fcase(
  period.of.construction %in% c("-1930", "1931-1940", "1941-1950"), "Before-1950",
  period.of.construction %in% c("1951-1960", "1961-1970", "1971-1980"), "1951-1980",
  period.of.construction %in% c("1981-1990", "1991-2000"), "1981-2000",
  period.of.construction %in% c("2001-2010", "2011-2020"), "2001-2020",
  period.of.construction == "2021-",                     "After-2020",
  period.of.construction == "data missing",              "Missing",
  default = "Other"
)]


# ==========================================================
#  CREATE NUTS LEVEL VARIABLES
# ==========================================================

stock_final <- stock_final %>%
  mutate(
    nuts3 = as.character(nuts3),
    nuts2 = str_sub(nuts3, 1, 4),
    nuts1 = str_sub(nuts3, 1, 3),
    nuts0 = str_sub(nuts3, 1, 2),
    year  = as.character(year),
    type.of.building = as.character(type.of.building),
    value = as.numeric(value)
  )

dt <- as.data.table(stock_final)
setnames(dt, "LAU CODE", "laucode")


# ==========================================================
#  AGGREGATION AT ALL REGIONAL LEVELS
# ==========================================================

# --- NUTS3 ---
agg_nuts3_total <- dt[
  , .(value = sum(value, na.rm = TRUE)),
  by = .(nuts3, year, type.of.building, period_group)
][, level := "nuts3"]
setnames(agg_nuts3_total, "nuts3", "nuts")

# --- NUTS2 ---
agg_nuts2_total <- dt[
  , .(value = sum(value, na.rm = TRUE)),
  by = .(nuts2, year, type.of.building, period_group)
][, level := "nuts2"]
setnames(agg_nuts2_total, "nuts2", "nuts")

# --- NUTS1 ---
agg_nuts1_total <- dt[
  , .(value = sum(value, na.rm = TRUE)),
  by = .(nuts1, year, type.of.building, period_group)
][, level := "nuts1"]
setnames(agg_nuts1_total, "nuts1", "nuts")

# --- NUTS0 ---
agg_nuts0_total <- dt[
  , .(value = sum(value, na.rm = TRUE)),
  by = .(nuts0, year, type.of.building, period_group)
][, level := "nuts0"]
setnames(agg_nuts0_total, "nuts0", "nuts")

# --- LAU (municipality) ---
agg_lau_total <- dt[
  , .(value = sum(value, na.rm = TRUE)),
  by = .(laucode, year, type.of.building, period_group)
][, level := "laucode"]
setnames(agg_lau_total, "laucode", "nuts")


# ----------------------------------------------------------
# Combine all aggregated levels
# ----------------------------------------------------------
agg_all_levels <- rbindlist(
  list(
    agg_nuts3_total,
    agg_nuts2_total,
    agg_nuts1_total,
    agg_nuts0_total,
    agg_lau_total
  ),
  fill = TRUE
)


# ==========================================================
#  CREATE MEASURE VARIABLE
# ==========================================================

agg_all_levels$measure <- paste0("dwell_", agg_all_levels$period_group)
agg_all_levels$measure <- gsub("-", "_", agg_all_levels$measure)

agg_all_levels[, period_group := NULL]

agg_all_levels[
  measure == "dwell_Missing",
  measure := "dwell_age_missing"
]


# ----------------------------------------------------------
# Save output
# ----------------------------------------------------------
write.csv(
  agg_all_levels,
  file = file.path(output_path, "SE_dwell_age.csv"),
  row.names = FALSE
)



# ==========================================================
#  TIME SERIES PLOTTING — DWELLING AGE MEASURES
# ==========================================================

setDT(agg_all_levels)

output_dir <- file.path(base_path, "SE/outputdata/checks_graphs/")
cat("Saving dwelling-age plots to:", output_dir, "\n")


# ----------------------------------------------------------
# Helper function: plot 12 regions per page
# ----------------------------------------------------------
plot_group <- function(dt, region_list, group_id, output_dir, prefix, measure_name) {
  
  data_sub <- dt[nuts %in% region_list]
  
  if (nrow(data_sub) == 0) {
    message("Skipping group ", group_id, " (NO DATA)")
    return()
  }
  
  p <- ggplot(
    data_sub,
    aes(x = as.integer(year), y = value, color = nuts)
  ) +
    geom_line(size = 0.9) +
    geom_point(size = 1.5) +
    facet_wrap(~ nuts, ncol = 3, scales = "free_y") +
    labs(
      title = paste(prefix, "–", measure_name, "– Group", group_id),
      x = "Year",
      y = "Total dwellings"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none")
  
  outfile <- file.path(
    output_dir,
    paste0(prefix, "_", measure_name, "_group_", group_id, ".png")
  )
  
  ggsave(outfile, p, width = 12, height = 10, dpi = 150)
  message("Saved plot:", outfile)
}


# ----------------------------------------------------------
# Wrapper: iterate over levels + measures
# ----------------------------------------------------------
process_level <- function(level_name, prefix, measure_name) {
  
  dt_sub <- agg_all_levels[
    level == level_name &
      measure == measure_name &
      type.of.building == "all"
  ]
  
  if (nrow(dt_sub) == 0) {
    message("❗ No data found for ", prefix, " — ", measure_name)
    return()
  }
  
  dt_sub[, year := as.integer(year)]
  
  region_list   <- unique(dt_sub$nuts)
  region_groups <- split(region_list, ceiling(seq_along(region_list) / 12))
  
  message(
    "Processing ", prefix, " — Measure: ", measure_name,
    " (Regions: ", length(region_list), ")"
  )
  
  for (i in seq_along(region_groups)) {
    plot_group(dt_sub, region_groups[[i]], i, output_dir, prefix, measure_name)
  }
}


# ----------------------------------------------------------
# Run plots for ALL dwelling-age measures
# ----------------------------------------------------------
all_measures <- unique(agg_all_levels$measure)

for (m in all_measures) {
  
  # LAU municipalities
  process_level("laucode", "lau_dwellage", m)
  
  # NUTS3 regions
  process_level("nuts3", "nuts3_dwellage", m)
}

cat("\n🎉 Finished generating DWELLING AGE time-series for ALL LAU & NUTS3 regions!\n")
