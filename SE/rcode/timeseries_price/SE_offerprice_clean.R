#############################################
# 1. LOAD & CLEAN WEBSCRAPED DATA
#############################################

# Load data
dt <- fread(file.path(output_webscrape_path, "SE_webscraped_clean.csv"))

# Remove SE_ prefix from LAU_code
dt[, LAU_code := sub("^SE_", "", LAU_code)]
setDT(dt)


#############################################
# 2. COUNT ADS BY TYPE AND LAU
#############################################

dt_summary2 <- dt[, .(N_ads = .N), by = .(type, LAU_code)]


#############################################
# 3. MERGE COUNTS BACK INTO ORIGINAL DATA
#############################################

ss <- merge(dt, dt_summary2, 
            by = c("type", "LAU_code"), 
            all.x = TRUE)


#############################################
# 4. COLLAPSE TO TYPE == "all" USING WEIGHTED MEAN
#############################################

# Keep only rows with price_m2
dt_price <- ss[!is.na(price_m2)]

# Weighted average price_m2 for ALL types together
dt_summary_all <- dt_price[
  , .(avg_ad_price = weighted.mean(price_m2, w = N_ads, na.rm = TRUE)),
  by = LAU_code
]

# Add type category
dt_summary_all[, type := "all"]


#############################################
# 5. SUMMARY BY TYPE & LAU (UNWEIGHTED MEAN)
#############################################

dt_summary <- dt[
  , .(avg_ad_price = mean(price_m2, na.rm = TRUE)),
  by = .(type, LAU_code)
]

# Stack with the "all" weighted summary
dt_summary <- rbind(dt_summary, dt_summary_all)


#############################################
# 6. CLEAN LAU CODE FORMATTING
#############################################

dt_summary[, LAU_code := sub("^0", "", LAU_code)]

dt_summary$avg_ad_price<- dt_summary$avg_ad_price*0.0975
#############################################
# 7. LOAD LAU REFERENCE DATA
#############################################

lau <- fread(file.path(source_lau_path, "SE_lau.csv"), encoding = "UTF-8")

# Clean column names (remove trailing spaces)
setnames(lau, old = names(lau), new = trimws(names(lau)))

# Convert LAU CODE to character
lau[, `LAU CODE` := as.character(`LAU CODE`)]

# Convert names to UTF-8
lau$`LAU NAME NATIONAL` <- iconv(lau$`LAU NAME NATIONAL`, 
                                 from = "Latin1", to = "UTF-8")


#############################################
# 8. MERGE SUMMARY WITH LAU INFORMATION
#############################################

stock_final <- merge(
  dt_summary,
  lau,
  by.x = "LAU_code",
  by.y = "LAU CODE",
  all.x = TRUE
)

# Keep only the first 4 columns
stock_final <- stock_final[, 1:4, with = FALSE]

# Rename columns
setnames(stock_final, "LAU_code", "laucode")
setnames(stock_final, "type", "type.of.building")
setnames(stock_final, "NUTS 3 CODE", "nuts3")

# Add fixed year
stock_final[, year := 2018]



# Load the OECD HPI CSV
df <- read.csv(
  file.path(source_prices_path, "SE_hpi_region.csv"),
  stringsAsFactors = FALSE
)


setnames(df, old = c("value"), new = c("index"))  # clearer name
all_years <- df[, unique(year)]

# unique municipality metadata
muni_meta <- unique(
  stock_final[, .(laucode,  nuts3, type.of.building)]
)

# expand: one row per municipality per year
muni_expanded <- muni_meta[
  , .(year = all_years), 
  by = .(laucode,  nuts3, type.of.building)
]


# merge benchmark values into full year grid
muni_full <- merge(
  muni_expanded, 
  stock_final[, .(laucode, year, nuts3, type.of.building, avg_ad_price)],
  by = c("laucode", "year","type.of.building","nuts3"),
  all.x = TRUE
)


full_data <- merge(muni_full, df, by = c("nuts3", "year"), all.x = TRUE)

setDT(full_data)

# 1. Get the 2018 base price for each laucode × type
full_data[
  year == 2018,
  base_price_2018 := avg_ad_price,
  by = .(laucode, type.of.building)
]

# 2. Carry the base price to all years within laucode × type
full_data[
  ,
  base_price_2018 := base_price_2018[!is.na(base_price_2018)][1],
  by = .(laucode, type.of.building)
]

# 3. Impute missing avg_ad_price using the index
full_data[
  is.na(avg_ad_price),
  avg_ad_price := base_price_2018 * (index / 100)
]

# 4. (Optional) drop helper column
full_data[, base_price_2018 := NULL]
full_data[, observations := NULL]
full_data[, county := NULL]
full_data[, index:= NULL]

#############################################
# 9. LOAD FLOOR AREA DATA AND MERGE
#############################################

totalm2 <- fread(file.path(output_path, "SE_m2_clean.csv"))

floor <- totalm2[level == "laucode"]
setnames(floor, "nuts", "laucode")

# Ensure matching types
stock_final[, laucode := as.integer(laucode)]
floor[, laucode := as.integer(laucode)]

# Merge on LAU
merged_data <- merge(
  stock_final, floor,
  by = c("laucode", "type.of.building", "year"),
  all.x = TRUE
)

#############################################
# 11. COMPUTE NUTS-LEVEL WEIGHTED AVERAGES
#############################################

# NUTS 3
nuts3_data <- merged_data[
  , .(
    price_m2 = weighted.mean(avg_ad_price, w = value, na.rm = TRUE),
    value = sum(value, na.rm = TRUE)
  ),
  by = .(year, nuts3, type.of.building)
]

# NUTS 2
nuts2_data <- merged_data[
  , .(
    price_m2 = round(weighted.mean(avg_ad_price, w = value, na.rm = TRUE), 0),
    value = sum(value, na.rm = TRUE)
  ),
  by = .(year, nuts2 = substr(nuts3, 1, 4), type.of.building)
]

# NUTS 1
nuts1_data <- merged_data[
  , .(
    price_m2 = round(weighted.mean(avg_ad_price, w = value, na.rm = TRUE), 0),
    value = sum(value, na.rm = TRUE)
  ),
  by = .(year, nuts1 = substr(nuts3, 1, 3), type.of.building)
]

# NUTS 0
nuts0_data <- merged_data[
  , .(
    price_m2 = round(weighted.mean(avg_ad_price, w = value, na.rm = TRUE), 0),
    value = sum(value, na.rm = TRUE)
  ),
  by = .(year, nuts0 = substr(nuts3, 1, 2), type.of.building)
]

# Rename avg_ad_price for compatibility
setnames(merged_data, "avg_ad_price", "price_m2")


#############################################
# 12. STACK ALL LEVELS INTO ONE AGGREGATION TABLE
#############################################

agg_all_levels <- rbindlist(list(
  merged_data[, .(year, level = "laucode", code = laucode, type.of.building, price_m2, value)],
  nuts3_data[, .(year, level = "nuts3", code = nuts3, type.of.building, price_m2, value)],
  nuts2_data[, .(year, level = "nuts2", code = nuts2, type.of.building, price_m2, value)],
  nuts1_data[, .(year, level = "nuts1", code = nuts1, type.of.building, price_m2, value)],
  nuts0_data[, .(year, level = "nuts0", code = nuts0, type.of.building, price_m2, value)]
), use.names = TRUE)

# Add measure variable
agg_all_levels[, measure := "offer_pricem2"]

# Remove value (sum of weights)
agg_all_levels[, value := NULL]

# Rename price_m2 → value
setnames(agg_all_levels, "price_m2", "value")

# Round values
agg_all_levels[, value := round(value, 0)]


#############################################
# 13. EXPORT FINAL CLEANED DATASET
#############################################

write.csv(
  agg_all_levels,
  file = file.path(output_path, "SE_offer_price_clean.csv"),
  row.names = FALSE
)
