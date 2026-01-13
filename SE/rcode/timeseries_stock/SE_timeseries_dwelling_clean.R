# ==========================================================
#  LOAD NUMBER OF DWELLINGS BY TYPE AND CLEAN THE FILE
#  ADD LAU CODE AND NUTS CODES
# ==========================================================

# --- Load dwelling data ---
stock <- fread(file.path(source_stock_path, "SE_dwelling_13_24.csv"), encoding = "UTF-8")

# --- Map building types to house/flat ---
type_map <- c(
  "one- or two-dwelling buildings" = "house",
  "multi-dwelling buildings"       = "flat",
  "other buildings"                = "other",
  "special housing"                = "special"
)
stock$type.of.building <- type_map[stock$type.of.building]

# --- Reshape data (melt) ---
stock_long <- melt(
  stock,
  id.vars      = c("year", "observations", "type.of.building", "region"),
  measure.vars = "value",
  variable.name = "measure",
  value.name    = "value"
)

# --- Load LAU codes ---
lau <- fread(file.path(source_lau_path, "SE_lau.csv"), encoding = "UTF-8")
lau$`LAU NAME NATIONAL` <- iconv(lau$`LAU NAME NATIONAL`, from = "Latin1", to = "UTF-8")

# --- Normalize region names for merging ---
normalize_name <- function(x) stri_trans_general(x, "Latin-ASCII") %>% tolower()
stock_long$region_norm <- normalize_name(stock_long$region)
lau$region_norm         <- normalize_name(lau$`LAU NAME NATIONAL`)

# --- Merge dwelling data with LAU codes ---
stock_final <- stock_long %>%
  left_join(lau, by = "region_norm") %>%
  dplyr::select(
    year, type.of.building, region, value,
    `NUTS 3 CODE`, `LAU CODE`, `LAU NAME NATIONAL`,
    POPULATION, `TOTAL AREA (m2)`
  )

# --- Rename NUTS3 column ---
setnames(stock_final, "NUTS 3 CODE", "nuts3")

# --- Create additional NUTS level codes ---
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

# --- Convert to data.table + rename LAU column ---
dt <- as.data.table(stock_final)
setnames(dt, "LAU CODE", "laucode")


# ==========================================================
#  AGGREGATIONS: NUTS3 / NUTS2 / NUTS1 / NUTS0 / LAU
# ==========================================================

# --- Aggregate at all NUTS levels ---
agg_nuts3_total <- dt[, .(value = sum(value, na.rm = TRUE)), by = .(nuts3, year, type.of.building)]
agg_nuts3_total[, level := "nuts3"]
setnames(agg_nuts3_total, "nuts3", "nuts")

agg_nuts2_total <- dt[, .(value = sum(value, na.rm = TRUE)), by = .(nuts2, year, type.of.building)]
agg_nuts2_total[, level := "nuts2"]
setnames(agg_nuts2_total, "nuts2", "nuts")

agg_nuts1_total <- dt[, .(value = sum(value, na.rm = TRUE)), by = .(nuts1, year, type.of.building)]
agg_nuts1_total[, level := "nuts1"]
setnames(agg_nuts1_total, "nuts1", "nuts")

agg_nuts0_total <- dt[, .(value = sum(value, na.rm = TRUE)), by = .(nuts0, year, type.of.building)]
agg_nuts0_total[, level := "nuts0"]
setnames(agg_nuts0_total, "nuts0", "nuts")

# --- Aggregate at LAU level ---
agg_lau_total <- dt[, .(value = sum(value, na.rm = TRUE)), by = .(laucode, year, type.of.building)]
agg_lau_total[, level := "laucode"]
setnames(agg_lau_total, "laucode", "nuts")

# --- Combine all ---
agg_all_levels <- rbindlist(
  list(
    agg_nuts3_total, agg_nuts2_total, agg_nuts1_total,
    agg_nuts0_total, agg_lau_total
  ),
  fill = TRUE
)
agg_all_levels$measure <- "dwelling"
setnames(agg_all_levels, "measure", "indicator")
l_agg_all_levels <- agg_all_levels


# ==========================================================
#  CREATE TYPE "ALL" (SUM OVER HOUSE/FLAT/OTHER)
# ==========================================================

dt_all <- dt[, .(value = sum(value, na.rm = TRUE)),
             by = .(nuts3, nuts2, nuts1, nuts0, laucode, year)]
dt_all[, type.of.building := "all"]

dt_combined <- rbindlist(list(dt, dt_all), fill = TRUE)

# --- Helper aggregation function ---
aggregate_level_total <- function(dt, level_col) {
  agg <- dt[, .(value = sum(value, na.rm = TRUE)), by = c(level_col, "year")]
  agg[, level := level_col]
  setnames(agg, level_col, "nuts")
  agg[, type.of.building := "all"]
  agg
}

# Apply function
agg_nuts3 <- aggregate_level_total(dt, "nuts3")
agg_nuts2 <- aggregate_level_total(dt, "nuts2")
agg_nuts1 <- aggregate_level_total(dt, "nuts1")
agg_nuts0 <- aggregate_level_total(dt, "nuts0")
agg_lau   <- aggregate_level_total(dt, "laucode")

# Combine
agg_all_levels <- rbindlist(
  list(agg_nuts3, agg_nuts2, agg_nuts1, agg_nuts0, agg_lau),
  fill = TRUE
)
agg_all_levels[, indicator := "dwelling"]

# Add detailed + all-types version
agg_all_levels <- rbind(agg_all_levels, l_agg_all_levels)

# Save final output
write.csv(
  agg_all_levels,
  file = file.path(output_path, "SE_dwell_clean.csv"),
  row.names = FALSE
)


# ==========================================================
#  TIME-SERIES PLOT GENERATION (LAU + NUTS3)
# ==========================================================

output_dir <- file.path(base_path, "SE/outputdata/checks_graphs/")
cat("Saving to:", output_dir, "\n")

# --- Helper: plot 12 regions at a time ---
plot_group <- function(dt, region_list, group_id, output_dir, prefix) {
  data_sub <- dt[nuts %in% region_list]
  if (nrow(data_sub) == 0) return()
  
  p <- ggplot(data_sub, aes(x = year, y = value, group = nuts, color = nuts)) +
    geom_line(size = 0.8) +
    geom_point(size = 1.3) +
    facet_wrap(~ nuts, ncol = 3, scales = "free_y") +
    labs(
      title = paste(prefix, "Time Series – Group", group_id),
      x = "Year", y = "Dwelling stock"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none")
  
  outfile <- file.path(output_dir, paste0(prefix, "_group_", group_id, ".png"))
  ggsave(outfile, p, width = 12, height = 10, dpi = 150)
}

# --- Wrapper for processing levels & types ---
process_level_type <- function(level_name, dwelling_type, prefix) {
  dt_sub <- agg_all_levels[level == level_name & type.of.building == dwelling_type]
  dt_sub[, year := as.integer(year)]
  
  region_list <- unique(dt_sub$nuts)
  region_groups <- split(region_list, ceiling(seq_along(region_list) / 12))
  
  for (i in seq_along(region_groups)) {
    plot_group(dt_sub, region_groups[[i]], i, output_dir, prefix)
  }
}

# --- LAU level plots ---
process_level_type("laucode", "house", "lau_house")
process_level_type("laucode", "flat",  "lau_flat")
process_level_type("laucode", "all",   "lau_all")

# --- NUTS3 level plots ---
process_level_type("nuts3", "house", "nuts3_house")
process_level_type("nuts3", "flat",  "nuts3_flat")
process_level_type("nuts3", "all",   "nuts3_all")

cat("\n🎉 All LAU + NUTS3 time series generated!\n")



# Filtra solo nuts = "SE" e type.of.building = "flat"
# agg_se_flat <- agg_all_levels %>%
#   filter(nuts == "SE110", type.of.building == "special")

# Line chart: value nel tempo per ogni level
# ggplot(agg_se_flat, aes(x = year, y = value, color = level, group = level)) +
#   geom_line(size = 1) +
#   geom_point() +
#   labs(
#     title = "Evoluzione del valore per nuts = SE (flat)",
#     x = "Year",
#     y = "Value",
#     color = "Aggregation Level"
#   ) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
#     axis.title = element_text(size = 12)
#   )


