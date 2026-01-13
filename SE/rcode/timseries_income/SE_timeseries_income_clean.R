# Load the data
dt <- read.csv(file.path(source_income_path, "SE_income_13_24.csv"))

# Define variable dictionary
type_map <- list(
  average_salary = "Average monthly salary in the municipalities",
  number_empl   = "Number of employees in the municipalities"
)

# Reverse the dictionary to map CSV column names to your variable names
reverse_map <- setNames(names(type_map), unlist(type_map))

# Replace the column names in 'observations' with the mapped names
dt$observations <- reverse_map[dt$observations]

# --- Load LAU codes ---
lau <- fread(file.path(source_lau_path, "SE_lau.csv"), encoding = "UTF-8")
lau$`LAU NAME NATIONAL` <- iconv(lau$`LAU NAME NATIONAL`, from = "Latin1", to = "UTF-8")

stock_long<-dt
# --- Normalize region names ---
normalize_name <- function(x) stri_trans_general(x, "Latin-ASCII") %>% tolower()
stock_long$region_norm <- normalize_name(stock_long$region)
lau$region_norm   <- normalize_name(lau$`LAU NAME NATIONAL`)

# Convert to data.table if not already
setDT(stock_long)
setDT(lau)

# Merge with LAU codes
stock_final <- merge(stock_long, lau, by = "region_norm", all.x = TRUE)

# Rename columns
setnames(stock_final, 
         old = c("LAU CODE", "observations"), 
         new = c("nuts", "measure"))

# Keep only selected variables
stock_final <- stock_final[, .(nuts, `NUTS 3 CODE`, measure, sex, value, year)]
 
# Delete rows with missing NUTS 3 codes
stock_final <- stock_final[!is.na(`NUTS 3 CODE`)]
lau_income<-stock_final

# Remove the NUTS3 column if it exists
lau_income[, nuts3 := NULL]  # remove column named 'nuts3'

# Add the level column
lau_income[, level := "laucode"]

# --- Rename NUTS column safely ---
stock_final <- dplyr::rename(stock_final, nuts3 = `NUTS 3 CODE`)

# --- Create higher-level NUTS codes ---
# Example: take the first n characters of nuts3
stock_final <- stock_final %>%
  mutate(
    nuts2 = substr(nuts3, 1, 4),  # adjust length based on NUTS coding
    nuts1 = substr(nuts3, 1, 3),
    nuts0 = substr(nuts3, 1, 2)
  )

# Reshape long to wide with an aggregate function
stock_wide <- dcast(stock_final, nuts + nuts3 + nuts2+ nuts1 + nuts0 +sex + year ~ measure, 
                    value.var = "value", 
                    fun.aggregate = function(x) sum(x, na.rm = TRUE))  # sum duplicates

# Compute weighted average salary at higher NUTS levels
aggregate_nuts <- function(dt, nuts_level, digits = 0) {
  dt[, .(
    average_salary = round(sum(average_salary * number_empl, na.rm = TRUE) / 
                             sum(number_empl, na.rm = TRUE), digits),
    number_empl    = sum(number_empl, na.rm = TRUE)
  ), by = .(nuts = get(nuts_level), sex, year)]
}

setDT(stock_wide) 


# Aggregate at higher levels
nuts3_agg <- aggregate_nuts(stock_wide, "nuts3")
nuts2_agg <- aggregate_nuts(stock_wide, "nuts2")
nuts1_agg <- aggregate_nuts(stock_wide, "nuts1")
nuts0_agg <- aggregate_nuts(stock_wide, "nuts0")
# Melt


# Function to aggregate and melt
aggregate_and_melt <- function(dt, nuts_level, digits = 0) {
  agg <- aggregate_nuts(dt, nuts_level, digits)
  melt(agg,
       id.vars = c("nuts", "sex", "year"),
       variable.name = "measure",
       value.name = "value")
}


# Aggregate and melt for all NUTS levels
nuts3_melt <- aggregate_and_melt(stock_wide, "nuts3")
nuts3_melt$level<-"nuts3"
nuts2_melt <- aggregate_and_melt(stock_wide, "nuts2")
nuts2_melt$level<-"nuts2"
nuts1_melt <- aggregate_and_melt(stock_wide, "nuts1")
nuts1_melt$level<-"nuts1"
nuts0_melt <- aggregate_and_melt(stock_wide, "nuts0")
nuts0_melt$level<-"nuts0"


# Remove the column named "NUTS 3 CODE"
lau_income[, `NUTS 3 CODE` := NULL]
# Combine all melted levels
agg_all_levels  <- rbind(nuts0_melt, nuts1_melt, nuts2_melt, nuts3_melt, lau_income)

# Combine all levels
# agg_all_levels <- rbind(nuts0_agg, nuts1_agg, nuts2_agg, nuts3_agg,lau_income)

# Save to CSV


# Save to CSV in the usual output folder
# Using fwrite (fast, from data.table)
write.csv(
  agg_all_levels,
  file = file.path(output_path, "SE_income_clean.csv"),
  row.names = FALSE
)

