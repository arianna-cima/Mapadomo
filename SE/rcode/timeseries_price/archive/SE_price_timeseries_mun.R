# ================================
# LOAD AND PREPARE HPI DATA
# ================================

# Load the OECD HPI CSV

df <- read.csv(file.path(source_prices_path, "OECD_HPI_2025-10-01.csv"), stringsAsFactors = FALSE)
# Create 'nuts' column: uppercase and remove 'M' (both lower and uppercase)
# Create cities table (rows with "M")
setDT(df)
# Filter rows where code contains lowercase "m"
cities <- df[grepl("m", df[["code"]], ignore.case = TRUE)]
# Step 2: Remove the "m" from code
cities[, code := gsub("m", "", code, ignore.case = TRUE)]
cities$nuts <-toupper(cities$code)

# Remove these rows from the original df
df <- df[!grepl("m", df[["code"]], ignore.case = TRUE)]
df$nuts <-toupper(df$code)

# Filter rows where code contains "M" (case-insensitive)
#cities <- df[grepl("M", code, ignore.case = TRUE)]


# Apply specific mappings for certain codes
cities$nuts[cities$nuts == "SE001"] <- "SE110"
cities$nuts[cities$nuts == "SE002"] <- "SE231"
cities$nuts[cities$nuts == "SE003"] <- "SE224"

# Ensure df is a data.table
df <- as.data.table(df)


setDT(df)

# Define the mapping manually as a named list
nuts2_to_nuts3 <- list(
  SE22 = c("SE221"),
  SE11 = c( "SE124", "SE125"),
  SE31 = c("SE311", "SE312", "SE313"),
  SE32 = c("SE321", "SE322"),
  SE33 = c("SE331", "SE332"),
  SE12 = c("SE121", "SE122", "SE123"),
  SE21 = c("SE211", "SE212", "SE213", "SE214")
)

# Expand df for each nuts3
df_expanded <- df[, .(nuts = nuts2_to_nuts3[[nuts]]), by = .(time, code, name, level, country, country_name, target, frequency, evolution, dwelling, vintage, index, index_country)]

df_expanded<-rbind(df_expanded,cities)

# Convert 'time' to Date, then extract year
df_expanded[, year := year(dmy(time))]  # dmy() converts "01/01/1986" to Date, year() extracts the year

# Keep only relevant columns
hpi_df <- df_expanded[, .(nuts, year, index)]

# Extract the 2024 index for each NUTS region (to use as base)
hpi_2024 <- hpi_df[year == 2024, .(nuts, index_2024 = index)]

# Merge 2024 index with full HPI to compute base-2024 index
hpi_df <- merge(hpi_df, hpi_2024, by = "nuts", all.x = TRUE)

# Compute HPI with base 2024 = 100
hpi_df[, index_base2024 := index / index_2024 * 100]

# Keep only necessary columns
hpi_df <- hpi_df[, .(nuts, year, index_base2024)]

# Create 'nuts3' column: if nuts has 5 characters → nuts3 = nuts, else NA
hpi_df[, nuts3 := ifelse(nchar(nuts) == 5, nuts, NA_character_)]

# Remove the nuts3 column
hpi_df[, nuts := NULL]# Rename 'nuts' to 'nuts2' for consistency

# Quick check
head(hpi_df)



# ================================
# LOAD AND PREPARE MUNICIPALITY DATA
# ================================

# Load municipality Excel file
muni_df <- read_excel(file.path(source_prices_path, "average_price_2024_municipality.xlsx"))
muni_df <- as.data.table(muni_df)

# Remove leading zeros from Municipality code
muni_df$Municipality.code <- as.character(muni_df$`Municipality code`)
muni_df$Municipality.code <- sub("^0+", "", muni_df$Municipality.code)

# Remove rows with missing Municipality code
muni_df <- muni_df[!is.na(Municipality.code) & Municipality.code != ""]


# ================================
# LOAD AND MERGE LAU CODES
# ================================

# --- Load LAU codes ---
lau <- fread(file.path(source_lau_path, "SE_lau.csv"), encoding = "UTF-8")
lau$`LAU NAME NATIONAL` <- iconv(lau$`LAU NAME NATIONAL`, from = "Latin1", to = "UTF-8")

# Ensure LAU codes are character for merging
lau$`LAU CODE` <- as.character(lau$`LAU CODE`)

# Merge municipality data with LAU codes using cleaned codes
merged_muni_lau <- merge(muni_df, lau, by.x = "Municipality.code", by.y = "LAU CODE", all.x = TRUE)

# Keep only the first 6 columns for simplicity
merged_muni_lau <- merged_muni_lau[, 1:6, with = FALSE]

# Add a new column 'type.of.building' with default value "house"
merged_muni_lau[, type.of.building := "house"]

# Rename 'NUTS 3 CODE' to 'nuts3' if it exists
if("NUTS 3 CODE" %in% colnames(merged_muni_lau)) {
  setnames(merged_muni_lau, "NUTS 3 CODE", "nuts3")
}

# Convert 'Average price in 1000 SEK' to EUR
exchange_rate <- 0.08745  # 1 SEK ≈ 0.08745 EUR in 2024
merged_muni_lau[, Average.price.EUR := `Average price in 1000 SEK` * 1000 * exchange_rate]

# Create 'nuts2' by taking first 4 characters of 'nuts3'
merged_muni_lau$nuts2 <- substring(merged_muni_lau$nuts3, 1, 4)

# If nuts3 is NA, fallback to nuts2 using Municipality code prefix
merged_muni_lau[, nuts_region := fifelse(!is.na(nuts3), nuts3, substr(Municipality.code, 1, 4))]

# Quick check
head(merged_muni_lau)



# ================================
# PREPARE HPI INDEX FOR MERGING
# ================================

hpi_index <- copy(hpi_df)


# ================================
# MERGE MUNICIPALITY DATA WITH HPI
# ================================

# Step 1: Merge on nuts3 (preferred, more granular level)
merged_prices <- merge(
  merged_muni_lau,
  hpi_index[, .(nuts3, year, index_base2024)],
  by = "nuts3",
  all.x = TRUE,
  allow.cartesian = TRUE
)

merged_prices<- merged_prices[,c("Municipality.code","nuts3","Number of purchases","Average.price.EUR","year","index_base2024")]
# ================================
# CALCULATE HISTORICAL PRICE
# ================================

# Compute back-calculated price based on index_base2024
merged_prices[, price := Average.price.EUR * index_base2024 / 100]

# Remove columns
merged_prices <- merged_prices[, "Average.price.EUR" := NULL]

# Quick check of final merged data
head(merged_prices)

# # 
# # # Filter for municipality 114
# muni_114 <- merged_prices[Municipality.code == "2580" ]
# 
# # Plot price over time
# ggplot(muni_114, aes(x = year, y = price)) +
#   geom_line(color = "blue", size = 1) +
#   geom_point(color = "red") +
#   labs(
#     title = "Backcasted Average Price over Time for Municipality 114",
#     x = "Year",
#     y = "Average Price (EUR)"
#   ) +
#   theme_minimal()



# ================================
# AGGREGATION BY NUTS3 AND TYPE
# ================================
# Compute total, average, and count of listings per m2
# separately for houses and flats within each NUTS3 region.
# ================================

dmicro <- read.csv(file.path(output_webscrape_path, "SE_webscraped_clean.csv"), stringsAsFactors = FALSE)

# Clean and transform
price <- read.csv(file.path(output_webscrape_path, "SE_webscraped_clean.csv"), stringsAsFactors = FALSE) %>%
  as.data.frame() %>%
  mutate(
    price_m2 = Winsorize(price_m2, probs = c(0.01, 0.99), na.rm = TRUE),
    lau = gsub(".*_", "", LAU_code),
    lau = sub("^0+(?=[1-9])", "", lau, perl = TRUE),
    indicator = "price_offer",
    #price_m2 = price_m2 / 10.2805   # convert to €/ft2 if that’s what you need
  ) %>%
  dplyr::select(price_m2, floor_m2,lau, NUTS_code, type)

# Convert to data.table
dt <- as.data.table(price)

# Keep valid entries
dt_clean <- dt[!is.na(NUTS_code) & !is.na(price_m2)]

# Summarize by NUTS3 and type
summary_dt <- dt_clean[, .(
 total_m2 = sum(floor_m2, na.rm = TRUE),
  avg_price_m2   = mean(price_m2, na.rm = TRUE),
  n_ads          = .N
), by = .(lau, NUTS_code, type)]

# Cast each variable
total_price_wide <- dcast(summary_dt, NUTS_code + lau ~ type,
                          value.var = "total_m2",
                          fun.aggregate = sum, na.rm = TRUE)
setnames(total_price_wide, c("flat", "house"), c("total_flat", "total_house"))

avg_price_wide <- dcast(summary_dt, NUTS_code + lau ~ type,
                        value.var = "avg_price_m2",
                        fun.aggregate = mean, na.rm = TRUE)
setnames(avg_price_wide, c("flat", "house"), c("avg_flat", "avg_house"))

n_ads_wide <- dcast(summary_dt, NUTS_code + lau ~ type,
                    value.var = "n_ads",
                    fun.aggregate = sum, na.rm = TRUE)
setnames(n_ads_wide, c("flat", "house"), c("n_flat", "n_house"))

# Merge all three
summary_wide <- Reduce(function(x, y) merge(x, y, by = c("NUTS_code", "lau")),
                       list(total_price_wide, avg_price_wide, n_ads_wide))

#if summary_wide$avg_flat is NaN then you replace avg_flat with the average of the average flat of the corresponding nuts3
# Replace NaN with NA (because mean() handles NA more naturally)
setDT(summary_wide)
summary_wide[is.nan(avg_flat), avg_flat := NA]


# Compute the weighted mean of avg_flat for each NUTS3, using n_flats as weights
summary_wide[, avg_flat := ifelse(
  is.na(avg_flat),
  sum(avg_flat * n_flat, na.rm = TRUE) / sum(n_flat[!is.na(avg_flat)]),
  avg_flat
), by = NUTS_code]

# Compute the mean avg_flat for each NUTS3, ignoring missing values
#summary_wide[, avg_flat := ifelse(
#  is.na(avg_flat),
 # mean(avg_flat, na.rm = TRUE),
 # avg_flat
#), by = NUTS_code]

summary_wide$share_house <- summary_wide$avg_house / summary_wide$avg_flat

# View results
head(summary_wide)

# ================================
# STEP 1: PREPARE DATA
# ================================
# merged_prices contains house prices by NUTS3, Municipality, and year.
# summary_wide contains the ratio share_house = avg_house_price_m2 / avg_flat_price_m2
# We will use this ratio to "create" flat prices from house prices.

setDT(merged_prices)
setDT(summary_wide)

# Keep only nuts3 and share_house info
shares <- summary_wide[, .(nuts3 = NUTS_code, share_house, n_flat, n_house,lau)]

# ================================
# STEP 2: MERGE SHARES INTO HOUSE DATA
# ================================

## FIX from here not matching nuts
#shares--> 0180

merged_prices$Municipality.code <- ifelse(
  nchar(merged_prices$Municipality.code) == 3,
  paste0("0", merged_prices$Municipality.code),
  merged_prices$Municipality.code
)


# Add the house/flat ratio to each row of merged_prices
merged_prices <- merge(merged_prices, shares, by.x = "Municipality.code", by.y = "lau", all.x = TRUE)
merged_prices$type.of.building="house"
# ================================
# STEP 3: GENERATE FLAT ROWS
# ================================
# Copy house rows, switch type to "flat",
# and scale price using share_house ratio:
#   flat_price = house_price / share_house
# This ensures consistency with the ratio definition.
# ================================
flats <- copy(merged_prices)[type.of.building == "house"]
flats[, type.of.building := "flat"]
flats[, price := price / share_house]

# ================================
# STEP 4: APPEND HOUSE + FLAT
# ================================
merged_prices_all <- rbind(merged_prices, flats, use.names = TRUE)
# Rename nuts3.x to nuts3
merged_prices_all <- merged_prices_all[, c(
  "Municipality.code", 
  "nuts3.x", 
  "Number of purchases", 
  "year", 
  "price", 
  "n_flat", 
  "n_house", 
  "type.of.building"
)]

# Make a copy of your data
dt <- copy(merged_prices_all)
dt[type.of.building == "house", w := n_house]
dt[type.of.building == "flat",  w := n_flat]

dt_all <- dt[, .(
  price_all = weighted.mean(price, w = w, na.rm = TRUE)
), by = .(Municipality.code, year)]
dt_all[, type.of.building := "all"]
setnames(dt_all, "price_all", "value")
dt_all$variable<-"price"

dt_transactions <- dt[, .(
  transaction_all = sum(`Number of purchases`, na.rm = TRUE)
), by = .(Municipality.code, year)]

dt_transactions[, type.of.building := "all"]
dt_transactions$variable<-"Number_purchases"

setnames(dt_transactions, "transaction_all", "value")


dd<-rbind(dt_transactions,dt_all)

# Remove n_flat and n_house
merged_prices_all[, c("n_flat", "n_house") := NULL]

# Melt the dataset
merged_prices_all <- melt(
  merged_prices_all,
  id.vars = c("Municipality.code", "nuts3.x", "year","type.of.building")
)

setDT(merged_prices_all)
merged_prices_all[, value := as.numeric(value)]
merged_prices_all[, value := round(value, 0)]

names(merged_prices_all)[names(merged_prices_all) == "nuts3.x"] <- "nuts3"
merged_prices_all[, c("nuts3") := NULL]

merged_prices_all<-rbind(merged_prices_all,dd)

# Write to CSV
fwrite(merged_prices_all, file = paste0(output_path, "/SE_prices.csv"))

# ================================
# STEP 4: download average price by squaremeter
# ================================
avg_floor <- read.csv(file.path(output_path, "SE_m2_mean.csv"), stringsAsFactors = FALSE)

# Check column names
colnames(merged_prices_all)
colnames(avg_floor)
avg_floor<-data.table(avg_floor)
# If avg_floor is a data.table

# Rename column in avg_floor to match merged_prices_all
setnames(avg_floor, old = "LAU_CODE", new = "Municipality.code")

# Make sure the join columns are the same type
merged_prices_all[, Municipality.code := as.character(Municipality.code)]
avg_floor[, Municipality.code := as.character(Municipality.code)]


avg_floor[, Municipality.code := ifelse(
  nchar(Municipality.code) == 3,
  paste0("0", Municipality.code),
  as.character(Municipality.code)
)]
# Merge by year and Municipality.code
merged_data <- merge(
  merged_prices_all,
  avg_floor,
  by = c("year", "Municipality.code", "type.of.building"),
  all.x = TRUE  # left join
)


# Remove rows where avg_floor_m2 is NA
merged_data <- merged_data[!is.na(avg_floor_m2)]

# Generate price per m2
merged_data[, price_m2 := price / avg_floor_m2]



# Save to CSV in the usual output folder
# Using fwrite (fast, from data.table)
write.csv(
  merged_data,
  file = file.path(output_path, "SE_price_m2.csv"),
  row.names = FALSE
)

totalm2 <- data.table::fread(file.path(output_path, "SE_m2_clean.csv"))
# Normalize municipality codes in 'nuts' to 4-digit strings (strip any 'SE' prefix)
totalm2[, nuts := as.character(nuts)]
totalm2[, nuts := gsub("^SE", "", nuts)]
totalm2[nchar(nuts) == 3, nuts := paste0("0", nuts)]
totalm2 <- totalm2[level == "laucode", ]


# Merge totals into the dataset (by year and Municipality.code)

merged_data <- merge(
  merged_data,
  totalm2,
  by.x = c("year", "Municipality.code", "type.of.building"),
  by.y = c("year", "nuts", "type.of.building"),
  all.x = TRUE
)


## ================================
## AGGREGATE price_m2 TO NUTS LEVELS (weighted by value)
## ================================
## Weighted average of price_m2 using 'value' as weight at NUTS3/2/1/0.

# Build hierarchical codes from nuts3
merged_data[, nuts2 := substr(nuts3, 1, 4)]
merged_data[, nuts1 := substr(nuts3, 1, 3)]
merged_data[, nuts0 := substr(nuts3, 1, 2)]

# NUTS3 aggregation (weighted average)
nuts3_agg <- merged_data[!is.na(nuts3),
  .(price_m2 = weighted.mean(price_m2, value, na.rm = TRUE),
    weight   = sum(value, na.rm = TRUE)),
  by = .(year, type.of.building, nuts = nuts3)
][, geo_level := "nuts3"]

# NUTS2 aggregation (weighted average)
nuts2_agg <- merged_data[!is.na(nuts2),
  .(price_m2 = weighted.mean(price_m2, value, na.rm = TRUE),
    weight   = sum(value, na.rm = TRUE)),
  by = .(year, type.of.building, nuts = nuts2)
][, geo_level := "nuts2"]

# NUTS1 aggregation (weighted average)
nuts1_agg <- merged_data[!is.na(nuts1),
  .(price_m2 = weighted.mean(price_m2, value, na.rm = TRUE),
    weight   = sum(value, na.rm = TRUE)),
  by = .(year, type.of.building, nuts = nuts1)
][, geo_level := "nuts1"]

# NUTS0 aggregation (weighted average)
nuts0_agg <- merged_data[!is.na(nuts0),
  .(price_m2 = weighted.mean(price_m2, value, na.rm = TRUE),
    weight   = sum(value, na.rm = TRUE)),
  by = .(year, type.of.building, nuts = nuts0)
][, geo_level := "nuts0"]

# Stack and save
price_m2_agg <- data.table::rbindlist(list(nuts3_agg, nuts2_agg, nuts1_agg, nuts0_agg), use.names = TRUE, fill = TRUE)
# Also create 'all' (house+flat) weighted averages at each NUTS level
nuts3_all <- merged_data[!is.na(nuts3),
  .(price_m2 = weighted.mean(price_m2, value, na.rm = TRUE)),
  by = .(year, nuts = nuts3)
][, `:=`(type.of.building = "all", geo_level = "nuts3")]

nuts2_all <- merged_data[!is.na(nuts2),
  .(price_m2 = weighted.mean(price_m2, value, na.rm = TRUE)),
  by = .(year, nuts = nuts2)
][, `:=`(type.of.building = "all", geo_level = "nuts2")]

nuts1_all <- merged_data[!is.na(nuts1),
  .(price_m2 = weighted.mean(price_m2, value, na.rm = TRUE)),
  by = .(year, nuts = nuts1)
][, `:=`(type.of.building = "all", geo_level = "nuts1")]

nuts0_all <- merged_data[!is.na(nuts0),
  .(price_m2 = weighted.mean(price_m2, value, na.rm = TRUE)),
  by = .(year, nuts = nuts0)
][, `:=`(type.of.building = "all", geo_level = "nuts0")]

price_m2_agg <- data.table::rbindlist(
  list(price_m2_agg, nuts3_all, nuts2_all, nuts1_all, nuts0_all),
  use.names = TRUE, fill = TRUE
)
price_m2_agg <- price_m2_agg[,c("price_m2","year","type.of.building","nuts","geo_level")]
colnames(price_m2_agg)<-c("price_m2","year","type.of.building","nuts","level")

lau_data<-merged_data[,.(year,type.of.building,price_m2,Municipality.code)]
lau_data$level<-"laucode"
colnames(lau_data)<-c("year","type.of.building","price_m2","nuts","level")

# Add LAU "all" weighted across house+flat
lau_all <- merged_data[, .(price_m2 = weighted.mean(price_m2, value, na.rm = TRUE)),
                       by = .(year, nuts = Municipality.code)]
lau_all[, `:=`(type.of.building = "all", level = "laucode")]
setcolorder(lau_all, c("year","type.of.building","price_m2","nuts","level"))


final<-rbind(price_m2_agg,lau_data,lau_all, use.names = TRUE, fill = TRUE)
final$price_m2<-round(final$price_m2,0)
colnames(final)<-c("value","year","type.of.building","nuts","level")




test <- final %>%
  filter(level == "laucode", 
         year=="2021",type.of.building=="all",
         !is.na(value))



# Save to CSV in the usual output folder
# Using fwrite (fast, from data.table)
write.csv(
 final,
  file = file.path(output_path, "SE_price_final.csv"),
  row.names = FALSE
)





# 
# 
# #
#  # Filter for municipality 114
# muni_114 <- merged_data[Municipality.code == "1080" & type.of.building=="flat"]
# 
# # Plot price over time
# ggplot(muni_114, aes(x = year, y = price_m2)) +
#   geom_line(color = "blue", size = 1) +
#   geom_point(color = "red") +
#   labs(
#     title = "Backcasted Average Price over Time for Municipality 114",
#     x = "Year",
#     y = "Average Price (EUR)"
#   ) +
#   theme_minimal()
# 

# Basic density plot
plot(density(merged_data$price_m2, na.rm = TRUE),
     main = "Density of Price per m²",
     xlab = "Price per m² (EUR)",
     ylab = "Density",
     col = "blue",
     lwd = 2)
