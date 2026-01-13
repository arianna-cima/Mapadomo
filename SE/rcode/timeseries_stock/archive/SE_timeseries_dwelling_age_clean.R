# --- Load dwelling data ---
stock <- fread(file.path(source_stock_path, "SE_building_13_24.csv"), encoding = "UTF-8")

# --- Map building types to house/flat ---
type_map <- c(
  "one- or two-dwelling buildings" = "house",
  "multi-dwelling buildings"       = "flat",
  "other buildings"                = "other",
  "special housing"                = "special"
)
stock$type.of.building <- type_map[stock$type.of.building]

# Assuming stock is already a data.table
# Step 1: Sum over type.of.building
stock_all <- stock[, .(value = sum(value)), 
                   by = .(year, observations, period.of.construction, region)]

# Step 2: Add type.of.building=="all"
stock_all[, type.of.building := "all"]

# Step 3: Combine with original stock
stock <- rbind(stock, stock_all)


# --- Load LAU codes ---
lau <- fread(file.path(source_lau_path, "SE_lau.csv"), encoding = "UTF-8")
lau$`LAU NAME NATIONAL` <- iconv(lau$`LAU NAME NATIONAL`, from = "Latin1", to = "UTF-8")

# --- Normalize region names ---
normalize_name <- function(x) stri_trans_general(x, "Latin-ASCII") %>% tolower()
stock$region_norm <- normalize_name(stock$region)
lau$region_norm   <- normalize_name(lau$`LAU NAME NATIONAL`)

# --- Merge with LAU codes ---
stock_final <- stock %>%
  left_join(lau, by = "region_norm")

# --- Rename NUTS column safely ---
stock_final <- dplyr::rename(stock_final, nuts3 = `NUTS 3 CODE`)

# Assuming stock_final is already a data.table
setDT(stock_final)

# Create aggregated construction period variable
stock_final[, period_group := fcase(
  period.of.construction %in% c("-1930", "1931-1940", "1941-1950"), "Before-1950",
  period.of.construction %in% c("1951-1960", "1961-1970", "1971-1980"), "1951-1980",
  period.of.construction %in% c("1981-1990", "1991-2000"), "1981-2000",
  period.of.construction %in% c("2001-2010", "2011-2020"), "2001-2020",
  period.of.construction == "2021-", "After-2020",
  period.of.construction == "data missing", "Missing",
  default = "Other"
)]

# --- Create other NUTS codes ---
stock_final <- stock_final %>%
  mutate(
    nuts3 = as.character(nuts3),
    nuts2 = str_sub(nuts3, 1, 4),
    nuts1 = str_sub(nuts3, 1, 3),
    nuts0 = str_sub(nuts3, 1, 2),
    year = as.character(year),
    type.of.building = as.character(type.of.building),
    value = as.numeric(value)
  )


# Convert to data.table
dt <- as.data.table(stock_final)
setnames(dt, "LAU CODE", "laucode")

# --- Aggregate at different levels ---
agg_nuts3_total <- dt[, .(value = sum(value, na.rm = TRUE)), by = .(nuts3, year, type.of.building, period_group)]
agg_nuts3_total[, level := "nuts3"]
setnames(agg_nuts3_total, "nuts3", "nuts")

agg_nuts2_total <- dt[, .(value = sum(value, na.rm = TRUE)), by = .(nuts2, year, type.of.building, period_group)]
agg_nuts2_total[, level := "nuts2"]
setnames(agg_nuts2_total, "nuts2", "nuts")

agg_nuts1_total <- dt[, .(value = sum(value, na.rm = TRUE)), by = .(nuts1, year, type.of.building, period_group)]
agg_nuts1_total[, level := "nuts1"]
setnames(agg_nuts1_total, "nuts1", "nuts")

agg_nuts0_total <- dt[, .(value = sum(value, na.rm = TRUE)), by = .(nuts0, year, type.of.building, period_group)]
agg_nuts0_total[, level := "nuts0"]
setnames(agg_nuts0_total, "nuts0", "nuts")

agg_lau_total <- dt[, .(value = sum(value, na.rm = TRUE)), by = .(laucode, year, type.of.building, period_group)]
agg_lau_total[, level := "laucode"]
setnames(agg_lau_total, "laucode", "nuts")

# --- Combine all levels ---
agg_all_levels <- rbindlist(list(agg_nuts3_total, agg_nuts2_total, agg_nuts1_total, agg_nuts0_total, agg_lau_total), fill = TRUE)

# --- Create concatenated measure variable and remove useful.floor.space ---
agg_all_levels$measure <- paste0("dwell_", agg_all_levels$period_group)
#agg_all_levels[, period.of.construction := NULL]


agg_all_levels$measure <- gsub("-", "_", agg_all_levels$measure)

agg_all_levels[, period_group := NULL]
agg_all_levels[measure == "dwell_Missing", measure := "dwell_age_missing"]

# Save to CSV in the usual output folder
# Using fwrite (fast, from data.table)
write.csv(
  agg_all_levels,
  file = file.path(output_path, "SE_dwell_age.csv"),
  row.names = FALSE
)








# ==========================================================
#  TIME SERIES PLOTTING — DWELLING AGE MEASURES
# ==========================================================

setDT(agg_all_levels)

# Output folder
output_dir <- file.path(base_path, "SE/outputdata/checks_graphs/")
cat("Saving dwelling-age plots to:", output_dir, "\n")


# ----------------------------------------------------------
# Helper: Plot 12 regions per figure
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
  
  outfile <- file.path(output_dir,
                       paste0(prefix, "_", measure_name, "_group_", group_id, ".png"))
  
  ggsave(outfile, p, width = 12, height = 10, dpi = 150)
  
  message("Saved plot:", outfile)
}


# ----------------------------------------------------------
# Wrapper for LAU + NUTS3
# ----------------------------------------------------------
process_level <- function(level_name, prefix, measure_name) {
  
  dt_sub <- agg_all_levels[
    level == level_name &
      measure == measure_name &
      type.of.building == "all"
  ]
  
  if (nrow(dt_sub) == 0) {
    message("❗ No data found for: ", prefix, " + ", measure_name)
    return()
  }
  
  dt_sub[, year := as.integer(year)]
  
  region_list <- unique(dt_sub$nuts)
  region_groups <- split(region_list, ceiling(seq_along(region_list) / 12))
  
  message("Processing ", prefix, " – measure:", measure_name,
          " (regions: ", length(region_list), ")")
  
  for (i in seq_along(region_groups)) {
    plot_group(dt_sub, region_groups[[i]], i, output_dir, prefix, measure_name)
  }
}


# ----------------------------------------------------------
# Run for ALL dwelling-age measures (automatic)
# ----------------------------------------------------------
all_measures <- unique(agg_all_levels$measure)

for (m in all_measures) {
  
  # LAU (municipality)
  process_level("laucode", "lau_dwellage", m)
  
  # NUTS3
  process_level("nuts3", "nuts3_dwellage", m)
}

cat("\n🎉 Finished generating DWELLING AGE time-series for ALL measures at LAU & NUTS3!\n")

