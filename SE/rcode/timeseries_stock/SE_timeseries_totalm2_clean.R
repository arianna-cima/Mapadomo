# ==========================================================
#  LOAD DWELLING DATA (TOTAL m² SERIES)
# ==========================================================

# --- Load dwelling data ---
stock <- fread(file.path(source_stock_path, "SE_dwelling_area_13_24.csv"), encoding = "UTF-8")

# --- Map building types to categories ---
type_map <- c(
  "one- or two-dwelling buildings" = "house",
  "multi-dwelling buildings"       = "flat",
  "other buildings"                = "other",
  "special housing"                = "special"
)
stock$type.of.building <- type_map[stock$type.of.building]

dt <- stock


# ==========================================================
#  RECODE FLOOR SIZE CATEGORIES INTO MIDPOINTS
# ==========================================================

# --- Small size ranges ---
block1 <- fifelse(dt$`useful.floor.space` == "< 31 sq.m.",   15.5,
                  fifelse(dt$`useful.floor.space` == "31-40 sq.m.",  35.5,
                          fifelse(dt$`useful.floor.space` == "41-50 sq.m.",  45.5,
                                  fifelse(dt$`useful.floor.space` == "51-60 sq.m.",  55.5,
                                          fifelse(dt$`useful.floor.space` == "61-70 sq.m.",  65.5, NA_real_)))))

# --- Medium size ranges ---
block2 <- fifelse(dt$`useful.floor.space` == "71-80 sq.m.",   75.5,
                  fifelse(dt$`useful.floor.space` == "81-90 sq.m.",   85.5,
                          fifelse(dt$`useful.floor.space` == "91-100 sq.m.",  95.5,
                                  fifelse(dt$`useful.floor.space` == "101-110 sq.m.", 105.5,
                                          fifelse(dt$`useful.floor.space` == "111-120 sq.m.", 115.5,
                                                  fifelse(dt$`useful.floor.space` == "121-130 sq.m.", 125.5, NA_real_))))))

# --- Large size ranges ---  check the last category if we want a bit more than 210
block3 <- fifelse(dt$`useful.floor.space` == "131-140 sq.m.", 135.5,
                  fifelse(dt$`useful.floor.space` == "141-150 sq.m.", 145.5,
                          fifelse(dt$`useful.floor.space` == "151-160 sq.m.", 155.5,
                                  fifelse(dt$`useful.floor.space` == "161-170 sq.m.", 165.5,
                                          fifelse(dt$`useful.floor.space` == "171-180 sq.m.", 175.5,
                                                  fifelse(dt$`useful.floor.space` == "181-190 sq.m.", 185.5,
                                                          fifelse(dt$`useful.floor.space` == "191-200 sq.m.", 195.5,
                                                                  fifelse(dt$`useful.floor.space` == "> 200 sq.m.", 210, 0))))))))

# --- Combine midpoint assignments ---
dt[, floor_m2 := fifelse(!is.na(block1), block1,
                         fifelse(!is.na(block2), block2,
                                 fifelse(!is.na(block3), block3, NA_real_)))]


# ==========================================================
#  COMPUTE TOTAL m² (COUNT * MIDPOINT)
# ==========================================================

dt$value <- dt$value * dt$floor_m2
dt[, floor_m2 := NULL]

# Aggregate total m² per region/year/type
dt <- dt[, .(value = sum(value)), by = .(year, type.of.building, region)]
dt[, value := round(value, 0)]


# ==========================================================
#  MERGE WITH LAU CODES
# ==========================================================

# --- Load LAU codes ---
lau <- fread(file.path(source_lau_path, "SE_lau.csv"), encoding = "UTF-8")
lau$`LAU NAME NATIONAL` <- iconv(lau$`LAU NAME NATIONAL`, from = "Latin1", to = "UTF-8")

# --- Normalize for safe joining ---
normalize_name <- function(x) stri_trans_general(x, "Latin-ASCII") %>% tolower()
dt$region_norm  <- normalize_name(dt$region)
lau$region_norm <- normalize_name(lau$`LAU NAME NATIONAL`)

# --- Merge ---
stock_final <- dt %>%
  left_join(lau, by = "region_norm") %>%
  dplyr::select(
    year, type.of.building, region, value,
    `NUTS 3 CODE`, `LAU CODE`, `LAU NAME NATIONAL`,
    POPULATION, `TOTAL AREA (m2)`
  )

# Rename NUTS3 column
stock_final <- dplyr::rename(stock_final, nuts3 = `NUTS 3 CODE`)


# ==========================================================
#  ADD NUTS CODES (1,2,0)
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
#  AGGREGATIONS: NUTS0 / NUTS1 / NUTS2 / NUTS3 / LAU
# ==========================================================

agg_nuts3_total <- dt[, .(value = sum(value, na.rm = TRUE)),
                      by = .(nuts3, year, type.of.building)]
agg_nuts3_total[, level := "nuts3"]
setnames(agg_nuts3_total, "nuts3", "nuts")

agg_nuts2_total <- dt[, .(value = sum(value, na.rm = TRUE)),
                      by = .(nuts2, year, type.of.building)]
agg_nuts2_total[, level := "nuts2"]
setnames(agg_nuts2_total, "nuts2", "nuts")

agg_nuts1_total <- dt[, .(value = sum(value, na.rm = TRUE)),
                      by = .(nuts1, year, type.of.building)]
agg_nuts1_total[, level := "nuts1"]
setnames(agg_nuts1_total, "nuts1", "nuts")

agg_nuts0_total <- dt[, .(value = sum(value, na.rm = TRUE)),
                      by = .(nuts0, year, type.of.building)]
agg_nuts0_total[, level := "nuts0"]
setnames(agg_nuts0_total, "nuts0", "nuts")

agg_lau_total <- dt[, .(value = sum(value, na.rm = TRUE)),
                    by = .(laucode, year, type.of.building)]
agg_lau_total[, level := "laucode"]
setnames(agg_lau_total, "laucode", "nuts")

# Combine all
agg_all_levels <- rbindlist(
  list(agg_nuts3_total, agg_nuts2_total, agg_nuts1_total, agg_nuts0_total, agg_lau_total),
  fill = TRUE
)
agg_all_levels$measure <- "totalm2"


# ==========================================================
#  CREATE TYPE "ALL" (SUM ACROSS BUILDING TYPES)
# ==========================================================

all <- agg_all_levels[
  , .(value = sum(value)),
  by = .(nuts, year, measure, level)
]
all$type.of.building <- "all"

setDT(all)
setDT(agg_all_levels)

agg_all_levels <- rbind(agg_all_levels, all, fill = TRUE)


# ==========================================================
#  SAVE CLEANED DATA
# ==========================================================

write.csv(
  agg_all_levels,
  file = file.path(output_path, "SE_m2_clean.csv"),
  row.names = FALSE
)


# ==========================================================
#  TIME SERIES PLOTTING
# ==========================================================

setDT(agg_all_levels)

# Output folder
output_dir <- file.path(base_path, "SE/outputdata/checks_graphs/")
cat("Saving plots to:", output_dir, "\n")


# --- Helper: Plot 12 regions per figure ---
plot_group <- function(dt, region_list, group_id, output_dir, prefix) {
  
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
      title = paste(prefix, "– Total m² – Group", group_id),
      x = "Year",
      y = "Total m²"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none")
  
  outfile <- file.path(output_dir, paste0(prefix, "_group_", group_id, ".png"))
  
  ggsave(outfile, p, width = 12, height = 10, dpi = 150)
}


# --- Wrapper for LAU/NUTS3 ---
process_level <- function(level_name, prefix) {
  
  dt_sub <- agg_all_levels[
    level == level_name &
      measure == "totalm2" &
      type.of.building == "all"
  ]
  
  dt_sub[, year := as.integer(year)]
  
  region_list <- unique(dt_sub$nuts)
  region_groups <- split(region_list, ceiling(seq_along(region_list) / 12))
  
  for (i in seq_along(region_groups)) {
    plot_group(dt_sub, region_groups[[i]], i, output_dir, prefix)
  }
}

# --- Lau plots ---
process_level("laucode", "lau_totalm2")

# --- NUTS3 plots ---
process_level("nuts3", "nuts3_totalm2")

cat("\n🎉 Finished generating total m² time-series for LAU & NUTS3!\n")
