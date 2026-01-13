# ==========================================================
# 1. LOAD DATA
# ==========================================================

dt <- fread(file.path(output_path, "SE_m2_lau.csv"))

# Remove uninformative category
dt <- dt[`useful.floor.space` != "data missing"]


# ==========================================================
# 2. MAP USEFUL FLOOR SPACE RANGES TO MIDPOINTS (m²)
# ==========================================================

# Dictionary for midpoint assignment
floor_map <- c(
  "< 31 sq.m."  = 15.5,
  "31-40 sq.m." = 35.5,
  "41-50 sq.m." = 45.5,
  "51-60 sq.m." = 55.5,
  "61-70 sq.m." = 65.5,
  "71-80 sq.m." = 75.5,
  "81-90 sq.m." = 85.5,
  "91-100 sq.m." = 95.5,
  "101-110 sq.m." = 105.5,
  "111-120 sq.m." = 115.5,
  "121-130 sq.m." = 125.5,
  "131-140 sq.m." = 135.5,
  "141-150 sq.m." = 145.5,
  "151-160 sq.m." = 155.5,
  "161-170 sq.m." = 165.5,
  "171-180 sq.m." = 175.5,
  "181-190 sq.m." = 185.5,
  "191-200 sq.m." = 195.5,
  "> 200 sq.m."  = 210
)

# Assign midpoint size
dt[, floor_m2 := floor_map[`useful.floor.space`]]


# ==========================================================
# 3. CLEAN FIELD TYPES
# ==========================================================

dt[, LAU_CODE := as.character(`LAU CODE`)]
dt[, year := as.integer(year)]

setDT(dt)  # ensure data.table


# ==========================================================
# 4. WEIGHTED TOTAL FLOOR AREA (SUM of m² × dwellings)
# ==========================================================

tot_floor <- dt[!is.na(floor_m2),
                .(total_floor_area = sum(floor_m2 * value, na.rm = TRUE)),
                by = .(year, LAU_CODE, type.of.building)]


# ==========================================================
# 5. WEIGHTED AVERAGE FLOOR SIZE (true mean m²)
# ==========================================================

avg_floor <- dt[!is.na(floor_m2),
                .(avg_floor_m2 = round(weighted.mean(
                  x = floor_m2,
                  w = value,
                  na.rm = TRUE
                ), 1)),
                by = .(year, LAU_CODE, type.of.building)]


# ==========================================================
# 6. SAVE OUTPUTS
# ==========================================================

fwrite(avg_floor, file.path(output_path, "SE_m2_mean.csv"))
#fwrite(tot_floor, file.path(output_path, "SE_m2_total.csv"))






# ==========================================================
# 0. LOAD LAU TABLE
# ==========================================================

lau <- fread(file.path(source_lau_path, "SE_lau.csv"), encoding = "UTF-8")

lau$`LAU NAME NATIONAL` <- iconv(
  lau$`LAU NAME NATIONAL`,
  from = "Latin1",
  to = "UTF-8"
)

setnames(lau, "LAU CODE", "LAU_CODE")
lau[, LAU_CODE := as.character(LAU_CODE)]


# ==========================================================
# 1. MERGE TOTAL FLOOR AREA (tot_floor) WITH NUTS3
# ==========================================================

setDT(tot_floor)
tot_floor[, LAU_CODE := as.character(LAU_CODE)]

# Merge NUTS3 code into total floor data
stock_final <- merge(
  tot_floor,
  lau[, .(LAU_CODE, nuts3 = `NUTS 3 CODE`)],
  by = "LAU_CODE",
  all.x = TRUE
)

stock_final <- stock_final[
  ,
  .(year, LAU_CODE, type.of.building, total_floor_area, nuts3)
]


# ==========================================================
# 2. COMPUTE NUTS3 TOTAL FLOOR AREA + LAU SHARE
# ==========================================================

nuts3_totals <- stock_final[
  ,
  .(nuts3_total_area = sum(total_floor_area, na.rm = TRUE)),
  by = .(nuts3, year, type.of.building)
]

merged_final <- merge(
  stock_final,
  nuts3_totals,
  by = c("nuts3", "year", "type.of.building"),
  all.x = TRUE
)

merged_final[, lau_share := total_floor_area / nuts3_total_area]


# ==========================================================
# 3. MERGE LAU AVERAGES (avg_floor_m2)
# ==========================================================

merged_data <- merge(
  avg_floor,
  merged_final[, .(nuts3, year, LAU_CODE, type.of.building, lau_share)],
  by = c("year", "LAU_CODE", "type.of.building"),
  all.x = TRUE
)


# ==========================================================
# 4. ADD NUTS2, NUTS1, NUTS0
# ==========================================================

merged_data[, nuts2 := substr(nuts3, 1, 3)]
merged_data[, nuts1 := substr(nuts3, 1, 2)]
merged_data[, nuts0 := substr(nuts3, 1, 2)]


# ==========================================================
# 5. FUNCTION FOR WEIGHTED AVERAGE
# ==========================================================

weighted_avg <- function(dt, region_col) {
  dt[
    ,
    .(avg_floor_m2 =
        sum(avg_floor_m2 * lau_share, na.rm = TRUE) /
        sum(lau_share, na.rm = TRUE)),
    by = c(region_col, "year", "type.of.building")
  ][, level := region_col ]
}


# ==========================================================
# 6. COMPUTE ALL LEVELS AND COMBINE INTO ONE TABLE
# ==========================================================

nuts3_avg <- weighted_avg(merged_data, "nuts3")
nuts2_avg <- weighted_avg(merged_data, "nuts2")
nuts1_avg <- weighted_avg(merged_data, "nuts1")
nuts0_avg <- weighted_avg(merged_data, "nuts0")

# Combine all levels
all_levels <- rbindlist(
  list(nuts3_avg, nuts2_avg, nuts1_avg, nuts0_avg),
  fill = TRUE
)

all_levels <- all_levels[
  ,
  .(nuts3, year, type.of.building, avg_floor_m2, level)
]


# Rename in all_levels
setnames(all_levels, "nuts3", "nuts")
setnames(all_levels, "avg_floor_m2", "value")

# Rename in avg_floor
setnames(avg_floor, "LAU_CODE", "nuts")
setnames(avg_floor, "avg_floor_m2", "value")
avg_floor$level<-"laucode"

dd<-rbind(all_levels, avg_floor)
# ==========================================================
# 7. SAVE EVERYTHING TO ONE FILE
# ==========================================================

fwrite(
  dd,
  file.path(output_path, "SE_m2_avg_all_levels.csv")
)



