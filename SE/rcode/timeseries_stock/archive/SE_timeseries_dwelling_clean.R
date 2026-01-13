# load number of dwelling by type and clean the file adding lau code and nuts codes
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

# --- Reshape data: melt useful.floor.space columns if needed ---
# If "useful.floor.space" is a column, we can just keep it as variable
stock_long <- melt(stock,
                   id.vars = c("year", "observations", "type.of.building", "region"),
                   measure.vars = "value",
                   variable.name = "measure",
                   value.name = "value")

# --- Load LAU codes ---
lau <- fread(file.path(source_lau_path, "SE_lau.csv"), encoding = "UTF-8")

# Re-encode LAU names
lau$`LAU NAME NATIONAL` <- iconv(lau$`LAU NAME NATIONAL`, from = "Latin1", to = "UTF-8")

# --- Normalize region names for safe join ---
normalize_name <- function(x) stri_trans_general(x, "Latin-ASCII") %>% tolower()
stock_long$region_norm <- normalize_name(stock_long$region)
lau$region_norm   <- normalize_name(lau$`LAU NAME NATIONAL`)

# --- Merge with LAU codes ---
stock_final <- stock_long %>%
  left_join(lau, by = "region_norm")



stock_final <- stock_final %>%
  dplyr::select(`year`, `type.of.building`, `region`, `value`,
                `NUTS 3 CODE`, `LAU CODE`, `LAU NAME NATIONAL`, `POPULATION`, `TOTAL AREA (m2)`)

# --- Rename NUTS column safely ---
#stock_final <- dplyr::rename(stock_final, nuts3 = `NUTS 3 CODE`)
setnames(stock_final, "NUTS 3 CODE", "nuts3")

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
# Rename column "LAU CODE" to "laucode"
setnames(dt, "LAU CODE", "laucode")


# 1. Aggregate at nuts3 level
agg_nuts3_total <- dt[, .(value = sum(value, na.rm = TRUE)), by = .(nuts3, year, type.of.building)]
agg_nuts3_total[, level := "nuts3"]
setnames(agg_nuts3_total, "nuts3", "nuts")  # rename column

# 2. Aggregate at nuts2 level
agg_nuts2_total <- dt[, .(value = sum(value, na.rm = TRUE)), by = .(nuts2, year, type.of.building)]
agg_nuts2_total[, level := "nuts2"]
setnames(agg_nuts2_total, "nuts2", "nuts")

# 3. Aggregate at nuts1 level
agg_nuts1_total <- dt[, .(value = sum(value, na.rm = TRUE)), by = .(nuts1, year, type.of.building)]
agg_nuts1_total[, level := "nuts1"]
setnames(agg_nuts1_total, "nuts1", "nuts")

# 4. Aggregate at nuts0 level
agg_nuts0_total <- dt[, .(value = sum(value, na.rm = TRUE)), by = .(nuts0, year, type.of.building)]
agg_nuts0_total[, level := "nuts0"]
setnames(agg_nuts0_total, "nuts0", "nuts")


# Aggregate at LAU code level
agg_lau_total <- dt[, .(value = sum(value, na.rm = TRUE)), by = .(laucode, year, type.of.building)]
agg_lau_total[, level := "laucode"]
setnames(agg_lau_total, "laucode", "nuts")  # rename to match other levels


# 5. Combine all into one table
agg_all_levels <- rbindlist(list(agg_nuts3_total, agg_nuts2_total, agg_nuts1_total, agg_nuts0_total,agg_lau_total), fill = TRUE)
agg_all_levels$measure<-"dwelling"
setnames(agg_all_levels, "measure", "indicator")
l_agg_all_levels<- agg_all_levels


# --- Create type "all" by summing over type.of.building ---
dt_all <- dt[, .(value = sum(value, na.rm = TRUE)), by = .(nuts3, nuts2, nuts1, nuts0, laucode, year)]
dt_all[, type.of.building := "all"]

# --- Combine original dt with the "all" type ---
dt_combined <- rbindlist(list(dt, dt_all), fill = TRUE)

# Function to aggregate at a given level and add type.of.building = "all"
aggregate_level_total <- function(dt, level_col) {
  agg <- dt[, .(value = sum(value, na.rm = TRUE)), by = c(level_col, "year")]
  agg[, level := level_col]               # keep track of aggregation level
  setnames(agg, level_col, "nuts")        # rename column to "nuts"
  agg[, type.of.building := "all"]        # add type = all
  return(agg)
}

# Aggregate at each level
agg_nuts3 <- aggregate_level_total(dt, "nuts3")
agg_nuts2 <- aggregate_level_total(dt, "nuts2")
agg_nuts1 <- aggregate_level_total(dt, "nuts1")
agg_nuts0 <- aggregate_level_total(dt, "nuts0")
agg_lau   <- aggregate_level_total(dt, "laucode")

# Combine all levels
agg_all_levels <- rbindlist(list(agg_nuts3, agg_nuts2, agg_nuts1, agg_nuts0, agg_lau), fill = TRUE)

# Add indicator column
agg_all_levels[, indicator := "dwelling"]


agg_all_levels<-rbind(agg_all_levels, l_agg_all_levels)
# save
write.csv(
  agg_all_levels,
  file = file.path(output_path, "SE_dwell_clean.csv"),
  row.names = FALSE
)




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


## ----------------------------------------------------------
## Base output folder
## ----------------------------------------------------------
output_dir <- file.path(base_path, "SE/outputdata/checks_graphs/")
#dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

cat("Saving to:", output_dir, "\n")


## ----------------------------------------------------------
## Helper function to plot groups of 12 regions (4×3 grid)
## ----------------------------------------------------------
plot_group <- function(dt, region_list, group_id, output_dir, prefix) {
  
  data_sub <- dt[nuts %in% region_list]
  
  if (nrow(data_sub) == 0) {
    message("Skipping group ", group_id, " (NO DATA)")
    return()
  }
  
  p <- ggplot(
    data_sub,
    aes(x = year, y = value, group = nuts, color = nuts)
  ) +
    geom_line(size = 0.8) +
    geom_point(size = 1.3) +
    facet_wrap(~ nuts, ncol = 3, scales = "free_y") +
    labs(
      title = paste(prefix, "Time Series – Group", group_id),
      x = "Year",
      y = "Dwelling stock"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none")
  
  outfile <- file.path(output_dir, paste0(prefix, "_group_", group_id, ".png"))
  
  message("Saving: ", outfile)
  
  ggsave(
    filename = outfile,
    plot = p,
    width = 12,
    height = 10,
    dpi = 150
  )
  
  message("Saved: ", outfile)
}


## ----------------------------------------------------------
## Function to process any level (laucode, nuts3) and type
## ----------------------------------------------------------
process_level_type <- function(level_name, dwelling_type, prefix) {
  
  dt_sub <- agg_all_levels[
    level == level_name &
      type.of.building == dwelling_type
  ]
  
  dt_sub[, year := as.integer(year)]
  
  region_list <- unique(dt_sub$nuts)
  
  message("Found ", length(region_list), " regions for ", prefix)
  
  region_groups <- split(region_list, ceiling(seq_along(region_list) / 12))
  
  for (i in seq_along(region_groups)) {
    plot_group(dt_sub, region_groups[[i]], i, output_dir, prefix)
  }
  
  message("✔ Finished ", prefix, "\n")
}


## ----------------------------------------------------------
## 1. LAU-level: house, flat, all
## ----------------------------------------------------------
process_level_type("laucode", "house", "lau_house")
process_level_type("laucode", "flat",  "lau_flat")
process_level_type("laucode", "all",   "lau_all")


## ----------------------------------------------------------
## 2. NUTS3-level: house, flat, all
## ----------------------------------------------------------
process_level_type("nuts3", "house", "nuts3_house")
process_level_type("nuts3", "flat",  "nuts3_flat")
process_level_type("nuts3", "all",   "nuts3_all")

cat("\n🎉 All LAU + NUTS3 time series generated!\n")





