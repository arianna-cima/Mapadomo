# --- Load dwelling data ---
stock <- fread(file.path(source_stock_path, "SE_dwelling_area_13_24.csv"), encoding = "UTF-8")

# --- Map building types to house/flat ---
type_map <- c(
  "one- or two-dwelling buildings" = "house",
  "multi-dwelling buildings"       = "flat",
  "other buildings"                = "other",
  "special housing"                = "special"
)
stock$type.of.building <- type_map[stock$type.of.building]

# --- Reshape data ---
stock_long <- melt(stock,
                   id.vars = c("year", "observations", "type.of.building", "useful.floor.space", "region"),
                   measure.vars = "value",
                   variable.name = "measure",
                   value.name = "value")


# --- Load LAU codes ---
lau <- fread(file.path(source_lau_path, "SE_lau.csv"), encoding = "UTF-8")
lau$`LAU NAME NATIONAL` <- iconv(lau$`LAU NAME NATIONAL`, from = "Latin1", to = "UTF-8")

# --- Normalize region names ---
normalize_name <- function(x) stri_trans_general(x, "Latin-ASCII") %>% tolower()
stock_long$region_norm <- normalize_name(stock_long$region)
lau$region_norm   <- normalize_name(lau$`LAU NAME NATIONAL`)

stock_final <- stock_long %>%
  left_join(lau, by = "region_norm") %>%
  dplyr::select(year, type.of.building, useful.floor.space, `LAU CODE`,`NUTS 3 CODE`, value)

# Make sure dt2 is a data.table
dt2 <- as.data.table(stock_final)
# Sum over type.of.building in dt2 and replace type.of.building with "all"
dt2_all <- dt2[, .(value = sum(value, na.rm = TRUE)),
               by = .(year, useful.floor.space, `LAU CODE`, `NUTS 3 CODE`)]
dt2_all[, type.of.building := "all"]

# Rename columns to match your main dt if needed
#setnames(dt2_all, old = c("LAU CODE", "NUTS 3 CODE"), new = c("laucode", "nuts3"))

# Combine with original dt
stock_final_combined <- rbindlist(list(stock_final, dt2_all), fill = TRUE)

# Optional: reorder columns
#setcolorder(stock_final_combined, c("year", "type.of.building", "useful.floor.space", "laucode", "nuts3", "value"))

write.csv(
  stock_final_combined,
  file = file.path(output_path, "SE_m2_lau.csv"),
  row.names = FALSE
)


# --- Rename NUTS column safely ---
stock_final <- dplyr::rename(stock_final, nuts3 = `NUTS 3 CODE`)

# --- Recode useful.floor.space into clean labels ---
stock_final <- stock_final %>%
  mutate(useful.floor.space = case_when(
    useful.floor.space %in% c("< 31 sq.m.", "31-40 sq.m.", "41-50 sq.m.", "51-60 sq.m.") ~ "<60sqm",
    useful.floor.space %in% c("61-70 sq.m.", "71-80 sq.m.", "81-90 sq.m.", "91-100 sq.m.") ~ "61_100sqm",
    useful.floor.space %in% c("101-110 sq.m.", "111-120 sq.m.", "121-130 sq.m.", "131-140 sq.m.",
                              "141-150 sq.m.") ~ "101_150sqm",
    useful.floor.space %in% c("151-160 sq.m.", "161-170 sq.m.", "171-180 sq.m.", "181-190 sq.m.",
                              "191-200 sq.m.", "> 200 sq.m.") ~ ">150sqm",
    useful.floor.space == "data missing" ~ "data_missing",
    TRUE ~ str_replace_all(useful.floor.space, " |\\-", "_")
  ))

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



############creating all type################
# Function to aggregate at a given level, keeping useful.floor.space and adding type.of.building == "all"
aggregate_level_floor <- function(dt, level_col) {
  # Original aggregation by type.of.building
  agg <- dt[, .(value = sum(value, na.rm = TRUE)), 
            by = c(level_col, "year", "type.of.building", "useful.floor.space")]
  
  agg[, level := level_col]                   # mark the level
  setnames(agg, level_col, "nuts")            # rename spatial column to "nuts"
  
  # Create type.of.building == "all" by summing over types
  agg_all <- dt[, .(value = sum(value, na.rm = TRUE)), 
                by = c(level_col, "year", "useful.floor.space")]
  agg_all[, level := level_col]
  setnames(agg_all, level_col, "nuts")
  agg_all[, type.of.building := "all"]
  
  # Combine original + type.all
  agg_combined <- rbindlist(list(agg, agg_all), fill = TRUE)
  
  return(agg_combined)
}

# Aggregate at all levels
agg_nuts3_total <- aggregate_level_floor(dt, "nuts3")
agg_nuts2_total <- aggregate_level_floor(dt, "nuts2")
agg_nuts1_total <- aggregate_level_floor(dt, "nuts1")
agg_nuts0_total <- aggregate_level_floor(dt, "nuts0")
agg_lau_total   <- aggregate_level_floor(dt, "laucode")

# Combine all levels
agg_all_levels <- rbindlist(list(agg_nuts3_total, agg_nuts2_total, agg_nuts1_total, agg_nuts0_total, agg_lau_total), fill = TRUE)

# Create concatenated measure variable and remove useful.floor.space
agg_all_levels[, measure := paste0("dwell", useful.floor.space)]
agg_all_levels[, useful.floor.space := NULL]



setDT(agg_all_levels)

# Output folder
output_dir <- file.path(base_path, "SE/outputdata/checks_graphs/")
cat("Saving dwelling-floor-space plots to:", output_dir, "\n")


# ----------------------------------------------------------
# Helper function — plot 12 regions per page
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
# Wrapper function — generates plots for each measure
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



# ==========================================================
#  GENERATE ALL PLOTS — LAU + NUTS3
# ==========================================================

all_measures <- unique(agg_all_levels$measure)

for (m in all_measures) {
  
  # Municipalities (LAU)
  process_level("laucode", "lau_dwell", m)
  
  # NUTS3 regions
  process_level("nuts3",  "nuts3_dwell", m)
}

cat("\n🎉 Finished generating DWELLING FLOOR-SPACE time-series for ALL LAU & NUTS3 regions!\n")