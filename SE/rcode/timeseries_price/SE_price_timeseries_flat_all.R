
dd <- fread(
  file.path(output_path, "SE_price_m2_house.csv"),
  encoding = "UTF-8"
)

#dd<-dd[dd$laucode=="1080"]
# ================================
# AGGREGATION BY NUTS3 AND TYPE
# ================================
# Use webscraped micro data to compute average prices per m2
# by lau (municipality), NUTS3 and type (flat/house)
# ================================

price <- read.csv(
  file.path(output_webscrape_path, "SE_webscraped_clean.csv"),
  stringsAsFactors = FALSE
) 

setDT(price)

# Step 1 — convert to numeric
price$price_m2 <- as.numeric(price$price_m2)

# Step 2 — winsorize
price$price_m2 <- Winsorize(price$price_m2, probs = c(0.01, 0.99), na.rm = TRUE)

# Step 3 — convert winsorized output to numeric
price$price_m2 <- as.numeric(price$price_m2)

# Step 4 — multiply by EUR conversion factor
price$price_m2 <- price$price_m2 * 0.092

# Step 5 — extract LAU code
price$lau <- sub(".*_", "", price$LAU_code)
price$lau <- sub("^0+(?=[1-9])", "", price$lau, perl = TRUE)

# Step 6 — add indicator
price$indicator <- "price_offer"

# Step 7 — keep the needed columns
price <- price[, .(price_m2, floor_m2, lau, NUTS_code, type)]
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

setDT(dd)
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

dd$laucode<-as.character(dd$laucode)


######imputation missing prices######
missing_lau <- dd[is.na(value), unique(laucode)]
nuts3_means <- dd[
  !is.na(value),
  .(nuts3_mean_value = mean(value)), 
  by = .(year, nuts3, type.of.building)
]

dd2 <- merge(
  dd,
  nuts3_means,
  by = c("year", "nuts3", "type.of.building"),
  all.x = TRUE
)

dd<- dd2[is.na(value), value := nuts3_mean_value]


# Merge micro shares (per LAU) into the municipality-level prices
merged_prices <- merge(
  dd,
  shares,
  by.x = "laucode",
  by.y = "lau",
  all.x = TRUE
)

uniqueN(dd$laucode)


# ensure data.table
setDT(merged_prices)

# 1. Compute NUTS3-level mean share_house (excluding NAs)
merged_prices[
  , nuts3_mean_share_house := mean(share_house, na.rm = TRUE),
  by = nuts3.x
]

# 2. Replace missing share_house with NUTS3 mean
merged_prices[
  is.na(share_house),
  share_house := nuts3_mean_share_house
]

# 3. (Optional) drop helper column
merged_prices[, nuts3_mean_share_house := NULL]




setDT(merged_prices)

# 1. Compute NUTS3-level means (excluding NAs)
merged_prices[
  , `:=`(
    nuts3_mean_share_house = mean(share_house, na.rm = TRUE),
    nuts3_mean_n_flat      = mean(n_flat, na.rm = TRUE),
    nuts3_mean_n_house     = mean(n_house, na.rm = TRUE)
  ),
  by = nuts3.x
]

# 2. Replace missing values with NUTS3 means
merged_prices[
  is.na(share_house),
  share_house := nuts3_mean_share_house
]

merged_prices[
  is.na(n_flat),
  n_flat := nuts3_mean_n_flat
]

merged_prices[
  is.na(n_house),
  n_house := nuts3_mean_n_house
]

# 3. (Optional) drop helper columns
merged_prices[
  , c(
    "nuts3_mean_share_house",
    "nuts3_mean_n_flat",
    "nuts3_mean_n_house"
  ) := NULL
]


#dd[is.na(value), unique(laucode)]
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
flats[,value := value / share_house]



# ================================
# STEP 4: APPEND HOUSE + FLAT DATA
# ================================

merged_prices_all <- rbind(merged_prices, flats, use.names = TRUE)

# Keep only relevant columns (note: nuts3 is kept, not nuts3.x)
merged_prices_all <- merged_prices_all[
  ,
  .(
    laucode,
    nuts3.x,
    year,
    value,
    n_flat,
    n_house,
    type.of.building
  )
]





# plot_data <- merged_prices_all[laucode == 125, .(
#   year,
#   type.of.building,
#   value
# )]
# 
# ggplot(plot_data, aes(x = year, y = value, color = type.of.building)) +
#   geom_line(linewidth = 1.2) +
#   theme_minimal(base_size = 14) +
#   labs(
#     title = "Value Time Series (€/m²)",
#     subtitle = paste(
#       "LAU:", 125, 
#       "| Municipality:", unique(merged_data[laucode == 125]$Municipality_name)
#     ),
#     x = "Year",
#     y = "Value (€/m²)",
#     color = "Building type"
#   )





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
    price_all = weighted.mean(value, w = w, na.rm = TRUE)
  ),
  by = .(laucode, year)
]



dt_all[, type.of.building := "all"]
setnames(dt_all, "price_all", "value")


merged_prices_all<-merged_prices_all[,c("laucode","year","type.of.building","value")]


all<-rbind(dt_all,merged_prices_all)

all[, value := round(value, 0)]
# 
# all[type.of.building == "flat",
#                   .(n_lau = uniqueN(laucode)),
#                   by = year]

write.csv(
  all,
  file = file.path(output_path, "SE_price_m2.csv"),
  row.names = FALSE
)
