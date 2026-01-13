# ==========================================================
#  LOAD & PREPARE DWELLING DATA
# ==========================================================

# --- Load dwelling Stock Data ---
stock <- fread(
  file.path(source_stock_path, "SE_dwelling_90_24.csv"),
  encoding = "UTF-8"
)

# ----------------------------------------------------------
# Map building types → house/flat/other/special
# ----------------------------------------------------------
type_map <- c(
  "one- or two-dwelling buildings" = "house",
  "multi-dwelling buildings"       = "flat",
  "other buildings"                = "other",
  "special housing"                = "special"
)
stock$type.of.building <- type_map[stock$type.of.building]

# ----------------------------------------------------------
# Map tenure categories → simplified codes
# ----------------------------------------------------------
type_map <- c(
  "tenant-owned dwellings"  = "tenant_owned",
  "rented dwellings"        = "rented",
  "owner-occupied dwellings"= "owner_occupied",
  "data missing"            = "missing"
)
stock$tenure <- type_map[stock$tenure]


# ==========================================================
#  ADD AGGREGATED “ALL BUILDING TYPES”
# ==========================================================

# Convert to data.table
stock2 <- as.data.table(stock)

# Sum over building type, then assign type.of.building = "all"
stock2_all <- stock2[
  ,
  .(value = sum(value, na.rm = TRUE)),
  by = .(year, observations, tenure, region)
]
stock2_all[, type.of.building := "all"]

# Combine full dataset + aggregated rows
stock2_combined <- rbindlist(list(stock, stock2_all), fill = TRUE)

# Reorder columns
setcolorder(
  stock2_combined,
  c("year", "observations", "tenure", "type.of.building", "region", "value")
)
stock <- stock2_combined


# ==========================================================
#  LOAD & MERGE LAU CODES
# ==========================================================

# --- Load LAU mapping ---
lau <- fread(file.path(source_lau_path, "SE_lau.csv"), encoding = "UTF-8")
lau$`LAU NAME NATIONAL` <- iconv(lau$`LAU NAME NATIONAL`, from = "Latin1", to = "UTF-8")

# --- Normalize region names (ASCII, lowercase) ---
normalize_name <- function(x) stri_trans_general(x, "Latin-ASCII") %>% tolower()
stock$region_norm <- normalize_name(stock$region)
lau$region_norm   <- normalize_name(lau$`LAU NAME NATIONAL`)

# --- Merge ---
stock_final <- stock %>%
  left_join(lau, by = "region_norm")

# --- Rename NUTS3 Code ---
stock_final <- dplyr::rename(stock_final, nuts3 = `NUTS 3 CODE`)

# --- Create NUTS 0/1/2 codes ---
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


# ==========================================================
#  AGGREGATION AT ALL LEVELS
# ==========================================================

dt <- as.data.table(stock_final)
setnames(dt, "LAU CODE", "laucode")

# --- NUTS3 ---
agg_nuts3_total <- dt[
  ,
  .(value = sum(value, na.rm = TRUE)),
  by = .(nuts3, year, type.of.building, tenure)
]
agg_nuts3_total[, level := "nuts3"]
setnames(agg_nuts3_total, "nuts3", "nuts")

# --- NUTS2 ---
agg_nuts2_total <- dt[
  ,
  .(value = sum(value, na.rm = TRUE)),
  by = .(nuts2, year, type.of.building, tenure)
]
agg_nuts2_total[, level := "nuts2"]
setnames(agg_nuts2_total, "nuts2", "nuts")

# --- NUTS1 ---
agg_nuts1_total <- dt[
  ,
  .(value = sum(value, na.rm = TRUE)),
  by = .(nuts1, year, type.of.building, tenure)
]
agg_nuts1_total[, level := "nuts1"]
setnames(agg_nuts1_total, "nuts1", "nuts")

# --- NUTS0 ---
agg_nuts0_total <- dt[
  ,
  .(value = sum(value, na.rm = TRUE)),
  by = .(nuts0, year, type.of.building, tenure)
]
agg_nuts0_total[, level := "nuts0"]
setnames(agg_nuts0_total, "nuts0", "nuts")

# --- LAU ---
agg_lau_total <- dt[
  ,
  .(value = sum(value, na.rm = TRUE)),
  by = .(laucode, year, type.of.building, tenure)
]
agg_lau_total[, level := "laucode"]
setnames(agg_lau_total, "laucode", "nuts")

# --- Combine all levels ---
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
#  CREATE FINAL MEASURE VARIABLE
# ==========================================================

agg_all_levels$measure <- paste0("dwell_", agg_all_levels$tenure)
agg_all_levels[, tenure := NULL]

# Harmonize missing category
agg_all_levels$measure[agg_all_levels$measure == "dwell_missing"] <- "dwell_tenure_missing"

# --- Save Final Output ---
write.csv(
  agg_all_levels,
  file = file.path(output_path, "SE_dwell_tenure.csv"),
  row.names = FALSE
)



# ==========================================================
#  TIME SERIES PLOTTING — DWELLING TENURE MEASURES
# ==========================================================

setDT(agg_all_levels)
output_dir <- file.path(base_path, "SE/outputdata/checks_graphs/")
cat("Saving dwelling-tenure plots to:", output_dir, "\n")


# ----------------------------------------------------------
# Helper function — plot 12 regions per page
# ----------------------------------------------------------
plot_group <- function(dt, region_list, group_id, output_dir, prefix, measure_name) {
  
  data_sub <- dt[nuts %in% region_list]
  if (nrow(data_sub) == 0) {
    message("Skipping group ", group_id, " (NO DATA)")
    return()
  }
  
  p <- ggplot(data_sub, aes(x = as.integer(year), y = value, color = nuts)) +
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
# Wrapper: iterate over regions + measures
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
  
  message("Processing ", prefix, " — Measure: ", measure_name,
          " (Regions: ", length(region_list), ")")
  
  for (i in seq_along(region_groups)) {
    plot_group(dt_sub, region_groups[[i]], i, output_dir, prefix, measure_name)
  }
}



# ==========================================================
#  GENERATE ALL PLOTS — LAU + NUTS3
# ==========================================================

all_measures <- unique(agg_all_levels$measure)

for (m in all_measures) {
  process_level("laucode", "lau_dwelltenure", m)
  process_level("nuts3",  "nuts3_dwelltenure", m)
}

cat("\n🎉 Finished generating DWELLING TENURE time-series for ALL LAU & NUTS3 regions!\n")
