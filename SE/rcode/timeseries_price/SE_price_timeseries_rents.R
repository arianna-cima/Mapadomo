# ==========================================================
#  LOAD NUMBER OF secondary building
#  ADD LAU CODE AND NUTS CODES
# ==========================================================
# --- Load dwelling data ---
rent <- fread(
  file.path(source_prices_path, "SE_rent.csv"),
  encoding = "UTF-8"
)

# 1. Keep only relevant observation types
rent <- rent[
  observations %in% c(
    "Median rent in rented dwellings",
    "Average rent in rented dwelling"
  )
]

# 2. Keep only relevant rental data types
rent <- rent[
  rental.data %in% c(
    "Annual rent per square metre",
    "New rent per square metre"
  )
]

# 3. Create clean indicator variable
rent[
  observations == "Median rent in rented dwellings",
  rent_type := "med_rent"
]

rent[
  observations == "Average rent in rented dwelling",
  rent_type := "avg_rent"
]



# --- Load LAU codes ---
lau <- fread(file.path(source_lau_path, "SE_lau.csv"), encoding = "UTF-8")
lau$`LAU NAME NATIONAL` <- iconv(lau$`LAU NAME NATIONAL`, from = "Latin1", to = "UTF-8")

# --- Normalize region names for merging ---
normalize_name <- function(x) stri_trans_general(x, "Latin-ASCII") %>% tolower()
rent$region_norm <- normalize_name(rent$region)
lau$region_norm         <- normalize_name(lau$`LAU NAME NATIONAL`)

# --- Merge dwelling data with LAU codes ---
rent <- rent %>%
  left_join(lau, by = "region_norm") %>%
  dplyr::select(
    year, region, value,
    `NUTS 3 CODE`, `LAU CODE`
  )



# --- Rename NUTS3 column ---
setnames(rent, "NUTS 3 CODE", "nuts3")
setnames(rent, "LAU CODE", "laucode")

rent$level<-"laucode"
rent[, region := NULL]
rent[, nuts3 := NULL]

write.csv(
  rent,
  file = file.path(output_path, "SE_rent_clean.csv"),
  row.names = FALSE
)


# ensure data.table
setDT(rent)

# summary table
result_rent <- rent[
  , .(
    n_unique_laucode = uniqueN(laucode),
    n_laucode_value_NA = uniqueN(laucode[is.na(value)])
  ),
  by = year
]

# view result
result_rent