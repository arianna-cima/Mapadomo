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
agg_all_levels[, measure := paste0("floor", useful.floor.space)]
agg_all_levels[, useful.floor.space := NULL]

#Filtra solo nuts = "SE" e type.of.building = "flat"
#highest increase SE312
# agg_se_flat <- agg_all_levels %>%
#   filter(nuts == "SE312", type.of.building == "house", measure=="floor>150sqm")

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
# 
# 
# 



# Read CSV into data.table
dt <- fread(file.path(output_path, "SE_m2_lau.csv"))

# Remove rows where useful.floor.space is "data missing"
dt <- dt[`useful.floor.space` != "data missing"]

# First block: small sizes
block1 <- fifelse(dt$`useful.floor.space` == "< 31 sq.m.", 15.5,
                  fifelse(dt$`useful.floor.space` == "31-40 sq.m.", 35.5,
                          fifelse(dt$`useful.floor.space` == "41-50 sq.m.", 45.5,
                                  fifelse(dt$`useful.floor.space` == "51-60 sq.m.", 55.5,
                                          fifelse(dt$`useful.floor.space` == "61-70 sq.m.", 65.5, NA_real_)))))

# Second block: medium sizes
block2 <- fifelse(dt$`useful.floor.space` == "71-80 sq.m.", 75.5,
                  fifelse(dt$`useful.floor.space` == "81-90 sq.m.", 85.5,
                          fifelse(dt$`useful.floor.space` == "91-100 sq.m.", 95.5,
                                  fifelse(dt$`useful.floor.space` == "101-110 sq.m.", 105.5,
                                          fifelse(dt$`useful.floor.space` == "111-120 sq.m.", 115.5,
                                                  fifelse(dt$`useful.floor.space` == "121-130 sq.m.", 125.5, NA_real_))))))

# Third block: large sizes
block3 <- fifelse(dt$`useful.floor.space` == "131-140 sq.m.", 135.5,
                  fifelse(dt$`useful.floor.space` == "141-150 sq.m.", 145.5,
                          fifelse(dt$`useful.floor.space` == "151-160 sq.m.", 155.5,
                                  fifelse(dt$`useful.floor.space` == "161-170 sq.m.", 165.5,
                                          fifelse(dt$`useful.floor.space` == "171-180 sq.m.", 175.5,
                                                  fifelse(dt$`useful.floor.space` == "181-190 sq.m.", 185.5,
                                                          fifelse(dt$`useful.floor.space` == "191-200 sq.m.", 195.5,
                                                                  fifelse(dt$`useful.floor.space` == "> 200 sq.m.", 210,
                                                      0 ))))))))

# Combine blocks
dt[, floor_m2 := fifelse(!is.na(block1), block1,
                         fifelse(!is.na(block2), block2,
                                 fifelse(!is.na(block3), block3, NA_real_)))]


# Make sure LAU CODE and year are correct type
dt[, `LAU CODE` := as.character(`LAU CODE`)]
dt[, year := as.integer(year)]  # or as.character if needed

dt <- dt %>%
  dplyr::rename(LAU_CODE = "LAU CODE")

dt<-data.table(dt)

# Make sure dt is a data.table
setDT(dt)

# Compute weighted average floor_m2 by year, LAU_CODE, and type.of.building
tot_floor <- dt[!is.na(floor_m2),   # ignore missing floor_m2
                .(avg_floor_m2 = round(sum(floor_m2 * value, na.rm = TRUE), 0)),
                by = .(year, LAU_CODE, type.of.building)]

# Compute weighted average floor_m2 by year, LAU_CODE, and type.of.building
avg_floor <- dt[!is.na(floor_m2),   # ignore missing floor_m2
                .(avg_floor_m2 = round(weighted.mean(floor_m2, w = value, na.rm = TRUE), 0)),
                by = .(year, LAU_CODE, type.of.building)]

# Check result
head(avg_floor)



# Save to CSV in the usual output folder
# Using fwrite (fast, from data.table)
write.csv(
  avg_floor,
  file = file.path(output_path, "SE_m2_mean.csv"),
  row.names = FALSE
)


# Save to CSV in the usual output folder
# Using fwrite (fast, from data.table)
write.csv(
  tot_floor,
  file = file.path(output_path, "SE_m2_total.csv"),
  row.names = FALSE
)




