# Load the CSV files
price <- read.csv(file.path(output_path, "SE_price_m2.csv"), stringsAsFactors = FALSE)
floor <- read.csv(file.path(output_path, "SE_m2_clean.csv"), stringsAsFactors = FALSE)

floor<-data.table(floor)
price<-data.table(price)


# Keep only rows where level == "laucode"
floor <- floor[level == "laucode"]

# Rename column in price to match 'laucode' for merging
setnames(floor, old = "nuts", new = "laucode")

# Make sure the types match
price[, laucode := as.integer(laucode)]
floor[, laucode := as.integer(laucode)]

# Merge price and floor by 'laucode' and 'type.of.building'
merged_data <- merge(price, floor, by = c("laucode", "type.of.building","year"), all.x = TRUE)

merged_data <- merged_data[year >= 2013]
setnames(merged_data , old = "value.x", new = "price_m2")
setnames(merged_data , old = "value.y", new = "value")


merged_data_1080 <- merged_data[laucode == 1080]



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
merged_data$laucode<-as.character(merged_data$laucode)
merged_data <- merge(
  merged_data,
  lau,
  by.x = "laucode",
  by.y = "LAU CODE",
  all.x = TRUE
)

merged_data<-merged_data[,1:8]

setnames(merged_data , old = "NUTS 3 CODE", new = "nuts3")
merged_data$value<-as.numeric(merged_data$value)
# Compute NUTS3 weighted average from LAU, and sum the weights (value)
nuts3_data <- merged_data[, .(
  price_m2 = weighted.mean(price_m2, w = value, na.rm = TRUE),
  value = sum(value, na.rm = TRUE)   # keep total value for higher-level weighting
), by = .(year, nuts3, type.of.building)]


nuts2_data <- nuts3_data[, .(
  price_m2 = round(weighted.mean(price_m2, w = value, na.rm = TRUE), 0),
  value = sum(value, na.rm = TRUE)
), by = .(year, nuts2 = substr(nuts3, 1, 4), type.of.building)]

nuts1_data <- nuts3_data[, .(
  price_m2 = round(weighted.mean(price_m2, w = value, na.rm = TRUE), 0),
  value = sum(value, na.rm = TRUE)
), by = .(year, nuts1 = substr(nuts3, 1, 3), type.of.building)]

nuts0_data <- nuts3_data[, .(
  price_m2 = round(weighted.mean(price_m2, w = value, na.rm = TRUE), 0),
  value = sum(value, na.rm = TRUE)
), by = .(year, nuts0 = substr(nuts3, 1, 2), type.of.building)]


# Combine all levels into one table
agg_all_levels <- rbindlist(list(
  merged_data[, .(year, level = "laucode", code = laucode, type.of.building, price_m2, value)],
  nuts3_data[, .(year, level = "nuts3", code = nuts3, type.of.building, price_m2, value)],
  nuts2_data[, .(year, level = "nuts2", code = nuts2, type.of.building, price_m2, value)],
  nuts1_data[, .(year, level = "nuts1", code = nuts1, type.of.building, price_m2, value)],
  nuts0_data[, .(year, level = "nuts0", code = nuts0, type.of.building, price_m2, value)]
), use.names = TRUE)

agg_all_levels$measure<-"price_m2"


setnames(agg_all_levels, old = "code", new = "nuts")
# Copy column
agg_all_levels$value <- agg_all_levels$price_m2
# Remove original column
agg_all_levels$price_m2 <- NULL




# Save to CSV in the usual output folder
# Using fwrite (fast, from data.table)
write.csv(
  agg_all_levels,
  file = file.path(output_path, "SE_prices_clean.csv"),
  row.names = FALSE
)
