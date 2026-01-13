# ================================
# Read and merge Estonian house price Excel files
#https://maaruum.ee/en/land-registry-and-land-valuation/real-estate-transactions/real-estate-transaction-statistics
#https://www.maaamet.ee/kinnisvara/htraru/FilterUI.aspx
# ================================

library(readxl)
library(dplyr)
library(stringr)
library(purrr)
library(readr)

# Folder containing the Excel files
data_path <- file.path(base_path, "EE/sourcedata/transaction_prices")

# List all relevant Excel files
files <- list.files(path = data_path, pattern = "Real_property_price_statistics_house_.*\\.xlsx$", full.names = TRUE)


# Function to read one file safely
read_one_file <- function(f) {
  
  # ---- 1. Municipality from A2 ----
  municipality_raw <- read_excel(
    f,
    range = "A2",
    col_names = FALSE
  ) %>% 
    pull(1)
  
  municipality <- str_remove(
    municipality_raw,
    "\\s+All municipalities$"
  )
  
  # ---- 2. Read table: FORCE ALL COLUMNS TO TEXT ----
  df <- read_excel(f, skip = 5, col_types = "text")
  
  # ---- 3. Add municipality ----
  df <- df %>% mutate(municipality = municipality)
  
  return(df)
}

# ---- 4. Read and bind all files ----
dd <- map_dfr(files, read_one_file)

# ---- 5. Convert numeric-looking columns (optional but recommended) ----
dd <- dd %>%
  mutate(
    across(
      where(~ all(is.na(.) | grepl("^[0-9.,]+$", ., perl = TRUE))),
      ~ parse_number(.)
    )
  )


# ---- 2. Replace "***" with NA (CHARACTER columns only) ----
dd <- dd %>%
  mutate(
    across(
      where(is.character),
      ~ na_if(.x, "***")
    )
  )

dd<- dd[,c(1:14)]

# ---- 1. Rename columns explicitly ----
colnames(dd)[1:14] <- c("year","vald","area_bin_m2","number","avg_area_m2","total_price_eur","min_price_eur","max_price_eur","min_price_m2","max_price_m2","median_price_m2","average_price_m2","standard_deviation","municipality")


# ---- 2. Replace "***" with NA (character columns only) ----
dd <- dd %>% mutate(across(where(is.character), ~ na_if(.x, "***")))

# ---- 3. Fill DOWN year and vald ----
dd <- dd %>% fill(year, vald, .direction = "down")

# ---- 4. Clean vald names ----
dd <- dd %>% mutate(vald = str_squish(vald))

# ---- 5. Convert numeric columns ----
numeric_vars <- c("number","avg_area_m2","total_price_eur","min_price_eur","max_price_eur","min_price_m2","max_price_m2","median_price_m2","average_price_m2","standard_deviation")


dd <- dd %>% mutate(across(all_of(numeric_vars), ~ parse_number(as.character(.))))


#dd$type<- "house"
setDT(dd)
vald_count_by_year <- dd[, .(n_unique_vald = uniqueN(vald)), by = year]


neighbors <- c("Jõhvi vald", "Alutaguse vald", "Lüganuse vald")
target_vald <- "Toila vald"

vars_to_impute <- c("number", "avg_area_m2", "total_price_eur",
                    "min_price_eur", "max_price_eur", "average_price_m2","median_price_m2")

# -------------------------------
# 1. Create missing Toila rows
# -------------------------------

toila_template <- unique(
  dd[vald %in% neighbors, .(year, area_bin_m2)]
)
toila_template[, vald := target_vald]

# Key both tables
setkey(dd, year, area_bin_m2, vald)
setkey(toila_template, year, area_bin_m2, vald)

# Add only missing rows
dd <- rbindlist(
  list(dd, toila_template[!dd]),
  use.names = TRUE, fill = TRUE
)

# -------------------------------
# 2. Impute using neighbour means
# -------------------------------

for (v in vars_to_impute) {
  dd[
    vald == target_vald & is.na(get(v)),
    (v) := dd[
      vald %in% neighbors & year == .BY$year & area_bin_m2 == .BY$area_bin_m2,
      mean(get(v), na.rm = TRUE)
    ],
    by = .(year, area_bin_m2)
  ]
}



dd <- dd[area_bin_m2 != "other"]
dd <- dd[area_bin_m2 == "Dwelling"]
dd <-dd[, .(vald, year, number, average_price_m2, median_price_m2)]


lau <- fread(file.path(source_lau_path, "EE_LAU2.csv"), encoding = "Latin-1")


clean_name <- function(x) {
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- tolower(x)
  x <- gsub("[^a-z ]", "", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

dd[, vald_clean := clean_name(vald)]

# adjust column name if needed (e.g. LAU_NAME, NAME, municipality)
lau[, lau_clean := clean_name(`LAU NAME LATIN`)]

#==================================================
# 5. Merge with LAU2
#==================================================

final_dt <- merge(dd, lau, by.x = "vald_clean", by.y = "lau_clean", all.x = TRUE)

final_dt<-final_dt[,c(1:10)]
final_dt$type<- "house"


vald_count_by_year <- final_dt[, .(n_unique_vald = uniqueN(vald)), by = year]
final_dt <- final_dt[year >= 2003 & year <= 2025]




# Save to CSV in the usual output folder
# Using fwrite (fast, from data.table)
write.csv(
  final_dt,
  file = file.path(source_prices_path, "EE_price_house_2003_2026_v1.csv"),
  row.names = FALSE
)

