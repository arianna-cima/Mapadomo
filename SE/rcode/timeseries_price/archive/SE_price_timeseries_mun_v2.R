# ================================
# LOAD AND PREPARE HPI DATA
# ================================

# Load the OECD HPI CSV
df <- read.csv(
  file.path(source_prices_path, "OECD_HPI_2025-10-01.csv"),
  stringsAsFactors = FALSE
)

# Work with data.table
setDT(df)

# --------------------------------
# Split city rows (with 'm' in code), clean codes, and create NUTS
# --------------------------------

# Rows with 'm' (case-insensitive) → these are cities
cities <- df[grepl("m", code, ignore.case = TRUE)]

# Remove 'm' (case-insensitive) from code and create NUTS for cities
cities[, code := gsub("m", "", code, ignore.case = TRUE)]
cities[, nuts := toupper(code)]

# Remove these rows from original df and create NUTS for remaining rows
df <- df[!grepl("m", code, ignore.case = TRUE)]
df[, nuts := toupper(code)]

# --------------------------------
# Manual mappings for specific city codes to NUTS3
# --------------------------------
cities[nuts == "SE001", nuts := "SE110"]
cities[nuts == "SE002", nuts := "SE231"]
cities[nuts == "SE003", nuts := "SE224"]

# Ensure df is data.table (already setDT above, but this is harmless)
setDT(df)

# --------------------------------
# Define NUTS2 → NUTS3 mapping
# --------------------------------

nuts2_to_nuts3 <- list(
  SE22 = c("SE221"),
  SE11 = c("SE124", "SE125"),
  SE31 = c("SE311", "SE312", "SE313"),
  SE32 = c("SE321", "SE322"),
  SE33 = c("SE331", "SE332"),
  SE12 = c("SE121", "SE122", "SE123"),
  SE21 = c("SE211", "SE212", "SE213", "SE214")
)

# --------------------------------
# Expand HPI from NUTS2 to NUTS3 using mapping
# (You mentioned all NUTS are in the mapping list.)
# --------------------------------

df_expanded <- df[
  ,
  .(
    nuts = nuts2_to_nuts3[[nuts]]
  ),
  by = .(
    time,
    code,
    name,
    level,
    country,
    country_name,
    target,
    frequency,
    evolution,
    dwelling,
    vintage,
    index,
    index_country
  )
]

# Append city rows (already at NUTS3 level)
df_expanded <- rbind(df_expanded, cities, use.names = TRUE, fill = TRUE)

# --------------------------------
# Convert 'time' to year and keep relevant columns
# --------------------------------

# 'time' is assumed like "01/01/1986"; dmy() parses it, year() extracts year
df_expanded[, year := year(dmy(time))]

# Keep only NUTS and index (HPI)
hpi_df <- df_expanded[, .(nuts, year, index)]

# --------------------------------
# Compute index with base 2024 = 100
# --------------------------------

# Index for 2024 by NUTS
hpi_2024 <- hpi_df[year == 2024, .(nuts, index_2024 = index)]

# Merge 2024 index into full panel
hpi_df <- merge(hpi_df, hpi_2024, by = "nuts", all.x = TRUE)

# Base-2024 index (index_2024 may be NA for some nuts if missing 2024 data)
hpi_df[, index_base2024 := index / index_2024 * 100]

# Keep only needed columns
hpi_df <- hpi_df[, .(nuts, year, index_base2024)]

# Create 'nuts3' column: exactly 5-character codes
hpi_df[, nuts3 := ifelse(nchar(nuts) == 5, nuts, NA_character_)]

# Remove original 'nuts' (we only retain nuts3 further down)
hpi_df[, nuts := NULL]

# Quick check
head(hpi_df)



# ================================
# LOAD AND PREPARE MUNICIPALITY DATA
# ================================

# Load municipality Excel file
muni_df <- read_excel(
  file.path(source_prices_path, "average_price_2024_municipality.xlsx")
)

muni_df <- as.data.table(muni_df)

# --------------------------------
# Clean municipality codes and drop invalid rows
# --------------------------------

# Copy original column with spaces into a clean one
muni_df[, Municipality.code := as.character(`Municipality code`)]

# Remove leading zeros
muni_df[, Municipality.code := sub("^0+", "", Municipality.code)]

# Keep only rows with non-missing municipality codes
muni_df <- muni_df[
  !is.na(Municipality.code) & Municipality.code != ""
]



# ================================
# LOAD AND MERGE LAU CODES
# ================================

# --------------------------------
# Load LAU codes and clean encoding
# --------------------------------

lau <- fread(
  file.path(source_lau_path, "SE_lau.csv"),
  encoding = "UTF-8"
)

lau[, `LAU NAME NATIONAL` := iconv(
  `LAU NAME NATIONAL`,
  from = "Latin1",
  to   = "UTF-8"
)]

# Ensure LAU codes are character
lau[, `LAU CODE` := as.character(`LAU CODE`)]

# --------------------------------
# Merge municipality data with LAU codes
# --------------------------------

merged_muni_lau <- merge(
  muni_df,
  lau,
  by.x = "Municipality.code",
  by.y = "LAU CODE",
  all.x = TRUE
)

# Keep only first 6 columns (as in original code)
merged_muni_lau <- merged_muni_lau[, 1:6, with = FALSE]

# Add default building type
merged_muni_lau[, type.of.building := "house"]

# Rename 'NUTS 3 CODE' to 'nuts3' if present
if ("NUTS 3 CODE" %in% colnames(merged_muni_lau)) {
  setnames(merged_muni_lau, "NUTS 3 CODE", "nuts3")
}

# --------------------------------
# Convert average municipality prices from SEK to EUR
# --------------------------------

exchange_rate <- 0.08745  # 1 SEK ≈ 0.08745 EUR in 2024
merged_muni_lau[, Average.price.EUR := `Average price in 1000 SEK` * 1000 * exchange_rate]

# Create 'nuts2' from first 4 characters of nuts3
merged_muni_lau[, nuts2 := substring(nuts3, 1, 4)]

# Fallback region (if nuts3 is NA) using first 4 digits of municipality code
merged_muni_lau[, nuts_region := fifelse(
  !is.na(nuts3),
  nuts3,
  substr(Municipality.code, 1, 4)
)]

# Quick check
head(merged_muni_lau)



# ================================
# PREPARE HPI INDEX FOR MERGING
# ================================

hpi_index <- copy(hpi_df)



# ================================
# MERGE MUNICIPALITY DATA WITH HPI
# ================================

# --------------------------------
# Merge on nuts3 and year (cartesian allowed)
# --------------------------------

merged_prices <- merge(
  merged_muni_lau,
  hpi_index[, .(nuts3, year, index_base2024)],
  by = "nuts3",
  all.x = TRUE,
  allow.cartesian = TRUE
)

# Keep only relevant columns
merged_prices <- merged_prices[
  ,
  .(
    Municipality.code,
    nuts3,
    `Number of purchases`,
    Average.price.EUR,
    year,
    index_base2024
  )
]



# ================================
# CALCULATE HISTORICAL HOUSE PRICE
# ================================

# Back-cast price from 2024 base:
# price(year) = price_2024 * index(year) / 100
merged_prices[, price := Average.price.EUR * index_base2024 / 100]

# Remove the 2024 price in EUR, keep back-casted price only
merged_prices[, Average.price.EUR := NULL]

# Quick check
head(merged_prices)



# ================================
# AGGREGATION BY NUTS3 AND TYPE
# ================================
# Use webscraped micro data to compute average prices per m2
# by lau (municipality), NUTS3 and type (flat/house)
# ================================

# --------------------------------
# Load webscraped micro data (only once) and clean
# --------------------------------

price <- read.csv(
  file.path(output_webscrape_path, "SE_webscraped_clean.csv"),
  stringsAsFactors = FALSE
) %>%
  as.data.frame() %>%
  mutate(
    price_m2 = Winsorize(price_m2, probs = c(0.01, 0.99), na.rm = TRUE),
    lau      = gsub(".*_", "", LAU_code),
    lau      = sub("^0+(?=[1-9])", "", lau, perl = TRUE),
    indicator = "price_offer"
    # price_m2 = price_m2 / 10.2805  # commented out as in original
  ) %>%
  dplyr::select(price_m2, floor_m2, lau, NUTS_code, type)

# Convert to data.table
dt <- as.data.table(price)

# Keep only valid entries for NUTS_code and price_m2
dt_clean <- dt[!is.na(NUTS_code) & !is.na(price_m2)]

# --------------------------------
# Summarize by LAU, NUTS3 and type
# --------------------------------

summary_dt <- dt_clean[
  ,
  .(
    total_m2     = sum(floor_m2, na.rm = TRUE),
    avg_price_m2 = mean(price_m2, na.rm = TRUE),
    n_ads        = .N
  ),
  by = .(lau, NUTS_code, type)
]

# --------------------------------
# Cast wide by type (flat/house) for:
# total m2, average price per m2, number of ads
# --------------------------------

total_price_wide <- dcast(
  summary_dt,
  NUTS_code + lau ~ type,
  value.var      = "total_m2",
  fun.aggregate  = sum,
  na.rm          = TRUE
)
setnames(total_price_wide, c("flat", "house"), c("total_flat", "total_house"))

avg_price_wide <- dcast(
  summary_dt,
  NUTS_code + lau ~ type,
  value.var      = "avg_price_m2",
  fun.aggregate  = mean,
  na.rm          = TRUE
)
setnames(avg_price_wide, c("flat", "house"), c("avg_flat", "avg_house"))

n_ads_wide <- dcast(
  summary_dt,
  NUTS_code + lau ~ type,
  value.var      = "n_ads",
  fun.aggregate  = sum,
  na.rm          = TRUE
)
setnames(n_ads_wide, c("flat", "house"), c("n_flat", "n_house"))

# Merge all wide tables
summary_wide <- Reduce(
  function(x, y) merge(x, y, by = c("NUTS_code", "lau")),
  list(total_price_wide, avg_price_wide, n_ads_wide)
)

setDT(summary_wide)

# --------------------------------
# Replace NaN avg_flat with NA, then impute avg_flat within NUTS3
# using weighted mean (weights = n_flat)
# --------------------------------

summary_wide[is.nan(avg_flat), avg_flat := NA_real_]

summary_wide[
  ,
  avg_flat := ifelse(
    is.na(avg_flat),
    sum(avg_flat * n_flat, na.rm = TRUE) / sum(n_flat[!is.na(avg_flat)]),
    avg_flat
  ),
  by = NUTS_code
]

# House–flat price ratio
summary_wide[, share_house := avg_house / avg_flat]

# Quick check
head(summary_wide)



# ================================
# STEP 1: PREPARE DATA FOR HOUSE → FLAT IMPUTATION
# ================================
# merged_prices: house prices at municipality (Municipality.code),
# nuts3, year.
# summary_wide: share_house = avg_house_price_m2 / avg_flat_price_m2
# We will use this ratio to construct flat prices from house prices.
# ================================

setDT(merged_prices)
setDT(summary_wide)

# Keep only NUTS3, ratio and counts from summary_wide
shares <- summary_wide[
  ,
  .(
    nuts3  = NUTS_code,
    share_house,
    n_flat,
    n_house,
    lau
  )
]



# ================================
# STEP 2: MERGE SHARES INTO HOUSE DATA
# ================================

# Normalize Municipality.code: pad 3-digit codes with leading zero
merged_prices[
  nchar(Municipality.code) == 3,
  Municipality.code := paste0("0", Municipality.code)
]

# Merge micro shares (per LAU) into the municipality-level prices
merged_prices <- merge(
  merged_prices,
  shares,
  by.x = "Municipality.code",
  by.y = "lau",
  all.x = TRUE
)

# All rows currently are houses
merged_prices[, type.of.building := "house"]



# ================================
# STEP 3: GENERATE FLAT ROWS
# ================================
# Duplicate house rows as flats and scale price:
# flat_price = house_price / share_house
# ================================

flats <- copy(merged_prices)[type.of.building == "house"]
flats[, type.of.building := "flat"]
flats[, price := price / share_house]



# ================================
# STEP 4: APPEND HOUSE + FLAT DATA
# ================================

merged_prices_all <- rbind(merged_prices, flats, use.names = TRUE)

# Keep only relevant columns (note: nuts3 is kept, not nuts3.x)
merged_prices_all <- merged_prices_all[
  ,
  .(
    Municipality.code,
    nuts3.x,
    `Number of purchases`,
    year,
    price,
    n_flat,
    n_house,
    type.of.building
  )
]

# ================================
# STEP 5: BUILD "ALL" TYPE (HOUSE+FLAT) AGGREGATES
# ================================

dt <- copy(merged_prices_all)

# Weights: use n_house for houses, n_flat for flats
dt[type.of.building == "house", w := n_house]
dt[type.of.building == "flat",  w := n_flat]

# Weighted average price over all types
dt_all <- dt[
  ,
  .(
    price_all = weighted.mean(price, w = w, na.rm = TRUE)
  ),
  by = .(Municipality.code, year)
]

dt_all[, type.of.building := "all"]
setnames(dt_all, "price_all", "value")
dt_all[, variable := "price"]

# Total number of transactions over all types
dt_transactions <- dt[
  ,
  .(
    transaction_all = sum(`Number of purchases`, na.rm = TRUE)
  ),
  by = .(Municipality.code, year)
]

dt_transactions[, type.of.building := "all"]
dt_transactions[, variable := "Number_purchases"]
setnames(dt_transactions, "transaction_all", "value")

# Stack all-type price and transactions
dd <- rbind(dt_transactions, dt_all, use.names = TRUE)

# Remove n_flat and n_house to avoid duplication
merged_prices_all[, c("n_flat", "n_house") := NULL]

# ================================
# STEP 6: LONG FORMAT FOR OUTPUT
# ================================

setnames(merged_prices_all, old = "nuts3.x", new = "nuts3")

# Melt houses/flats
merged_prices_all <- melt(
  merged_prices_all,
  id.vars = c("Municipality.code", "nuts3", "year", "type.of.building")
)

setDT(merged_prices_all)
merged_prices_all[, value := as.numeric(value)]
merged_prices_all[, value := round(value, 0)]

# We don't need nuts3 in the final output (as in original)
merged_prices_all[, nuts3 := NULL]

# Add all-type aggregates
merged_prices_all <- rbind(merged_prices_all, dd, use.names = TRUE, fill = TRUE)

# Write to CSV
fwrite(merged_prices_all, file = file.path(output_path, "SE_prices.csv"))



# ================================
# STEP 7: MERGE AVERAGE FLOOR AREA (m2)
# ================================
# Convert price to price per m2 using avg_floor_m2
# ================================

avg_floor <- read.csv(
  file.path(output_path, "SE_m2_mean.csv"),
  stringsAsFactors = FALSE
)

avg_floor <- data.table(avg_floor)

# Rename to match Municipality.code
setnames(avg_floor, old = "LAU_CODE", new = "Municipality.code")

# Ensure join columns are character
merged_prices_all[, Municipality.code := as.character(Municipality.code)]
avg_floor[, Municipality.code := as.character(Municipality.code)]

# Pad Municipality.code to 4 digits when needed
avg_floor[
  nchar(Municipality.code) == 3,
  Municipality.code := paste0("0", Municipality.code)
]

# Merge by year, Municipality.code and type.of.building
merged_data <- merge(
  merged_prices_all,
  avg_floor,
  by = c("year", "Municipality.code", "type.of.building"),
  all.x = TRUE
)

# Remove rows without avg_floor_m2
merged_data <- merged_data[!is.na(avg_floor_m2)]
merged_data[, avg_floor_m2 := as.numeric(avg_floor_m2)]

# Convert price variable to €/m2 (others unchanged)
merged_data[variable == "price", value := value / avg_floor_m2]

# Save €/m2 output
write.csv(
  merged_data,
  file = file.path(output_path, "SE_price_m2.csv"),
  row.names = FALSE
)



# ================================
# STEP 8: MERGE TOTAL m2 BY LAU
# ================================
# Attach information on total m2 (by year, municipality, type)
# ================================

totalm2 <- fread(file.path(output_path, "SE_m2_clean.csv"))

# Normalize municipality codes in 'nuts' to 4-digit strings (strip "SE")
totalm2[, nuts := as.character(nuts)]
totalm2[, nuts := gsub("^SE", "", nuts)]
totalm2[nchar(nuts) == 3, nuts := paste0("0", nuts)]

# Keep only LAU-level rows
totalm2 <- totalm2[level == "laucode", ]

# Merge totals into merged_data (by year, Municipality.code, type.of.building)
merged_data <- merge(
  merged_data,
  totalm2,
  by.x = c("year", "Municipality.code", "type.of.building"),
  by.y = c("year", "nuts", "type.of.building"),
  all.x = TRUE
)




## ================================
## BUILD NUTS HIERARCHY
## ================================
lau[, `LAU CODE` := ifelse(nchar(`LAU CODE`) == 3,
                           paste0("0", `LAU CODE`),
                           `LAU CODE`)]

tt <- merge(
  merged_data,
  lau,
  by.x = "Municipality.code",
  by.y = "LAU CODE",
  all.x = TRUE
)

tt <- tt[, c("Municipality.code","year","type.of.building","variable","value.x","value.y","level","measure","NUTS 3 CODE")]

setnames(tt, old = "NUTS 3 CODE", new = "nuts3")
setnames(tt, old = "value.x", new = "price_m2")
setnames(tt, old = "value.y", new = "value")

merged_data<-tt
merged_data[, nuts2 := substr(nuts3, 1, 4)]
merged_data[, nuts1 := substr(nuts3, 1, 3)]
merged_data[, nuts0 := substr(nuts3, 1, 2)]


## ================================
## WEIGHTED AGGREGATION FUNCTION
## ================================


agg_fun <- function(dt, nuts_var, level_name) {
  
  nuts_col <- dt[[nuts_var]]   # safely extract the column inside the function
  
  dt[!is.na(nuts_col),
     .(
       price_m2 = weighted.mean(price_m2, value, na.rm = TRUE),
       weight   = sum(value, na.rm = TRUE)
     ),
     by = .(year, type.of.building, nuts = nuts_col)
  ][, level := level_name]
}

## ================================
## NUTS 3/2/1/0 – ALL TYPES
## ================================
nuts3_agg <- agg_fun(merged_data, "nuts3", "nuts3")
nuts2_agg <- agg_fun(merged_data, "nuts2", "nuts2")
nuts1_agg <- agg_fun(merged_data, "nuts1", "nuts1")
nuts0_agg <- agg_fun(merged_data, "nuts0", "nuts0")

price_m2_agg <- rbindlist(
  list(nuts3_agg, nuts2_agg, nuts1_agg, nuts0_agg),
  use.names = TRUE, fill = TRUE
)


# Remove column "weight"
price_m2_agg <- price_m2_agg[, -c("weight")]

# Rename price_m2 → value
setnames(price_m2_agg, old = "price_m2", new = "value")

# Add variable column
price_m2_agg[, variable := "price_m2"]


## ================================
## LAU LEVEL
## ================================

# Select columns properly using data.table syntax
lau_all <- merged_data[, .(Municipality.code, year, type.of.building, variable, price_m2, level)]

# Filter rows where variable == "price"
lau_all <- lau_all[variable == "price"]

# Rename Municipality.code → nuts

# Rename price_m2 → value
setnames(lau_all, old = "price_m2", new = "value")
setnames(lau_all, old = "Municipality.code", new = "nuts")

## ================================
## FINAL STACKING
## ================================
final <- rbindlist(
  list(price_m2_agg, lau_all),
  use.names = TRUE, fill = TRUE
)

final[, value := round(value, 0)]

setnames(final, old = "type.of.building", new = "type")
setnames(final, old = "variable", new = "measure")

## ================================
## EXPORT
## ================================
write.csv(
  final,
  file = file.path(output_path, "SE_price_final.csv"),
  row.names = FALSE
)


## ================================
## QUICK CHECK
## ================================
test <- final %>%
  filter(level == "laucode",
         year == "2021",
         type == "all",
         !is.na(value))


## ================================
## DIAGNOSTIC PLOT
## ================================
plot(
  density(merged_data$price_m2, na.rm = TRUE),
  main = "Density of Price per m²",
  xlab = "Price per m² (EUR)",
  ylab = "Density",
  col  = "blue",
  lwd  = 2
)