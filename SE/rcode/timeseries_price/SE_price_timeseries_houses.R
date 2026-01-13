
#https://www.scb.se/en/finding-statistics/statistics-by-subject-area/housing-construction-and-building/real-estate/real-estate-prices-and-registrations-of-title/pong/tables-and-graphs/statistics-for-municipalities/average-prices-for-one--and-two-dwelling-buildings-2024-by-municipality/
# ================================
# LOAD AND PREPARE MUNICIPALITY DATA
# ================================

# Load municipality Excel file
muni_df <- read_excel(
  file.path(source_prices_path, "average_price_municipality.xlsx")
)

muni_df <- as.data.table(muni_df)
muni_df<-muni_df[!is.na(muni_df$Code),]

muni_df[, 3:ncol(muni_df)] <- muni_df[, 3:ncol(muni_df)] |>
  lapply(\(x) round(as.numeric(x), 0))

cols_keep <- c(
  "Code",
  "Municipality Name",
  "Average price in SEK thousands",
  "Average price 2023",
  "Average price 2019",
  "Average price 2014",
  "Average price 2004"
)
muni_df <- muni_df[, ..cols_keep]

setnames(muni_df,
         old = c("Code",
                 "Municipality Name",
                 "Average price in SEK thousands",
                 "Average price 2023",
                 "Average price 2019",
                 "Average price 2014",
                 "Average price 2004"),
         new = c("laucode",
                 "Municipality_name",
                 "2024",
                 "2023",
                 "2019",
                 "2014",
                 "2004"))
# --------------------------------
# Clean municipality codes and drop invalid rows
# --------------------------------

# Copy original column with spaces into a clean one
muni_df[, laucode := as.character(`laucode`)]

# Remove leading zeros
muni_df[, laucode := sub("^0+", "", laucode)]

# ================================
# LOAD AND MERGE LAU CODES
# ================================


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
  by.x = "laucode",
  by.y = "LAU CODE",
  all.x = TRUE
)

# Keep only first 6 columns (as in original code)
merged_muni_lau <- merged_muni_lau[, 1:8, with = FALSE]

# Add default building type
merged_muni_lau[, type.of.building := "house"]


# Rename 'NUTS 3 CODE' to 'nuts3' if present
if ("NUTS 3 CODE" %in% colnames(merged_muni_lau)) {
  setnames(merged_muni_lau, "NUTS 3 CODE", "nuts3")
}

# --------------------------------
# Convert average municipality prices from SEK to EUR
# --------------------------------

exchange_rate_24 <- 0.08745
exchange_rate_23 <- 0.08710
exchange_rate_19 <- 0.09450
exchange_rate_14 <- 0.10990
exchange_rate_04 <- 0.11

exchange_rates <- c(
  exchange_rate_24,
  exchange_rate_23,
  exchange_rate_19,
  exchange_rate_14,
  exchange_rate_04
)

merged_muni_lau[, (3:7) := Map(function(col, rate) col * 1000 * rate,
                               .SD, exchange_rates),
                .SDcols = 3:7]

merged_muni_lau[, 3:7] <- merged_muni_lau[, 3:7] |>
  lapply(\(x) round(as.numeric(x), 0))


# Load the OECD HPI CSV
df <- read.csv(
  file.path(source_prices_path, "SE_hpi_region.csv"),
  stringsAsFactors = FALSE
)

muni_long <- melt(
  merged_muni_lau,
  id.vars = c("laucode", "Municipality_name", "nuts3", "type.of.building"),
  measure.vars = c("2024", "2023", "2019", "2014", "2004"),
  variable.name = "year",
  value.name = "price"
)
# 
# setDT(muni_long)
# muni_long[type.of.building == "house",
#                 .(n_lau = uniqueN(.SD$laucode)),
#                 by = .SD$year]


setDT(muni_long)
muni_long[, year := as.integer(as.character(year))]



setDT(df)
setnames(df, old = c("value"), new = c("index"))  # clearer name
all_years <- df[, unique(year)]

# unique municipality metadata
muni_meta <- unique(
  merged_muni_lau[, .(laucode, Municipality_name, nuts3, type.of.building)]
)

# expand: one row per municipality per year
muni_expanded <- muni_meta[
  , .(year = all_years), 
  by = .(laucode, Municipality_name, nuts3, type.of.building)
]


# merge benchmark values into full year grid
muni_full <- merge(
  muni_expanded, 
  muni_long[, .(laucode, year, price)],
  by = c("laucode", "year"),
  all.x = TRUE
)

setDT(muni_full)
muni_full[type.of.building == "house",
          .(n_lau = uniqueN(.SD$laucode)),
          by = .SD$year]



full_data <- merge(muni_full, df, by = c("nuts3", "year"), all.x = TRUE)

# 
# setDT(full_data)
# full_data[type.of.building == "house",
#                 .(n_lau = uniqueN(.SD$laucode)),
#                 by = .SD$year]


hybrid_interpolation <- function(year, price, index) {
  
  n <- length(price)
  known_idx <- which(!is.na(price))
  n_known <- length(known_idx)
  
  # Case 0: No price at all
  if (n_known == 0) return(rep(NA_real_, n))
  
  # -----------------------
  # 1. LINEAR INTERPOLATION
  # -----------------------
  if (n_known == 1) {
    # Only one observed value → constant line
    price_linear <- rep(price[known_idx], n)
  } else {
    price_linear <- approx(
      x = year[known_idx],
      y = price[known_idx],
      xout = year,
      rule = 2
    )$y
  }
  
  # -------------------------
  # 2. INDEX-BASED INTERPOLATION
  # -------------------------
  price_index <- numeric(n)
  
  # For each segment between known prices
  for (j in 1:n_known) {
    
    k <- known_idx[j]  # base year index
    base_p <- price[k]
    base_i <- index[k]
    
    # Start of segment
    start <- k
    # End of segment
    end <- if (j < n_known) known_idx[j+1] else n
    
    # Fill this segment
    price_index[start:end] <- base_p * (index[start:end] / base_i)
  }
  
  # For years before the first known observation
  if (known_idx[1] > 1) {
    k <- known_idx[1]
    base_p <- price[k]
    base_i <- index[k]
    price_index[1:(k-1)] <- base_p * (index[1:(k-1)] / base_i)
  }
  
  # Observed prices must remain exact
  price_index[known_idx] <- price[known_idx]
  
  # -------------------------
  # 3. HYBRID AVERAGE
  # -------------------------
  hybrid <- 0.7 * price_linear + 0.3 * price_index
  
  # Observed prices remain exact
  hybrid[known_idx] <- price[known_idx]
  
  return(hybrid)
}

full_data<-full_data[, price_hybrid := hybrid_interpolation(year, price, index), by = laucode]



plot_data<-full_data[laucode == 125, .(year, index, price, price_hybrid)]


ggplot(plot_data, aes(x = year)) +
  
  # Interpolated price line
  geom_line(aes(y = price_hybrid), color = "blue", size = 1.2) +
  
  # Observed price points
  geom_point(aes(y = price), color = "red", size = 3, na.rm = TRUE) +
  
  # Labels for observed points
  geom_text(
    aes(y = price, label = round(price, 0)),
    color = "red", vjust = -1, size = 3, na.rm = TRUE
  ) +
  
  labs(
    title = "Municipality Price Trend (Observed + Interpolated)",
    subtitle = paste("LAU:", 114, "| Municipality:", unique(full_data[laucode == 114]$Municipality_name)),
    x = "Year",
    y = "Price",
    caption = "Blue = Interpolated | Red = Observed"
  ) +
  theme_minimal(base_size = 14)


full_data<- full_data[,c("nuts3", "year", "laucode", "type.of.building", "price_hybrid")]

setnames(full_data, "price_hybrid", "value")
full_data[, value := round(value, 0)]





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

# Ensure data.table
setDT(avg_floor)

# All years you want
years_missing <- 1990:2012

# For each LAU_CODE, get the 2013 value
base_values <- avg_floor[year == 2013, .(
  LAU_CODE,
  type.of.building,
  avg_2013 = avg_floor_m2
)]

# Build repeated rows for each LAU_CODE
missing_rows <- base_values[
  , .(year = years_missing, avg_floor_m2 = avg_2013),
  by = .(LAU_CODE, type.of.building)
]


avg_floor <- rbind(avg_floor, missing_rows)


# Rename to match Municipality.code
setnames(avg_floor, old = "LAU_CODE", new = "laucode")

# Ensure join columns are character
full_data[, laucode := as.character(laucode)]
avg_floor[, laucode := as.character(laucode)]


# Merge by year, Municipality.code and type.of.building
merged_data <- merge(
  full_data,
  avg_floor,
  by = c("year", "laucode", "type.of.building"),
  all.x = TRUE
)
merged_data[, avg_floor_m2 := as.numeric(avg_floor_m2)]

# Convert price variable to €/m2 (others unchanged)
merged_data[, value := value / avg_floor_m2]

merged_data[, value := round(value, 0)]

merged_data<-merged_data[!is.na(laucode),]
# Save €/m2 output
write.csv(
  merged_data,
  file = file.path(output_path, "SE_price_m2_house.csv"),
  row.names = FALSE
)




result <- merged_data[
  , .(
    n_unique_laucode = uniqueN(laucode),
    n_value_NA = sum(is.na(value))
  ),
  by = .(year, type.of.building)
]

result

# 
# plot_data <- merged_data[laucode == 125, .(
#   year, 
#   value
# )]
# 
# ggplot(plot_data, aes(x = year, y = value)) +
#   geom_line(color = "blue", linewidth = 1.2) +
#   theme_minimal(base_size = 14) +
#   labs(
#     title = "Value Time Series (€/m²)",
#     subtitle = paste("LAU:", 125, "| Municipality:", 
#                      unique(merged_data[laucode == 125]$Municipality_name)),
#     x = "Year",
#     y = "Value (€/m²)"
#   )





