# ==================================================
# #RLV215: CONVENTIONAL DWELLINGS by Location, Type of building, Year and Occupancy
# ==================================================

# ----------------------------
# 1. API URL
# ----------------------------
url <- "https://andmed.stat.ee/api/v1/en/stat/RLV215"

# ----------------------------
# 2. RAW JSON body (exactly as API requires)
# ----------------------------
json_body <- '{
  "query": [],
  "response": {
    "format": "json-stat"
  }
}'

# ----------------------------
# 3. POST request
# ----------------------------
res <- POST(
  url = url,
  body = json_body,
  encode = "raw",
  add_headers(
    "Content-Type" = "application/json",
    "Accept" = "application/json"
  )
)

# ----------------------------
# 4. Stop if error
# ----------------------------
stop_for_status(res)

# ----------------------------
# 5. Retrieve JSON
# ----------------------------
json_text <- content(res, as = "text", encoding = "UTF-8")

# Save raw JSON
writeLines(json_text, "RLV215_full.json")

# ----------------------------
# 6. Convert json-stat → data frame
# ----------------------------
df <- fromJSONstat("RLV215_full.json")

DT <- as.data.table(df)

colnames(DT) <- c(
  "Municipality",
  "type_of_building",
  "year",
  "occupancy",
  "value"
)


drop_vals <- c(
  "Whole country",
  "..City settlement region",
  "..Town settlement region",
  "..Rural settlement region",
  "Northern Estonia",
  "..Northern Estonia: city settlement region",
  "..Northern Estonia: town settlement region",
  "..Northern Estonia: rural settlement region",
  "Central Estonia",
  "..Central Estonia: city settlement region",
  "..Central Estonia: town settlement region",
  "..Central Estonia: rural settlement region",
  "Northeastern Estonia",
  "..Northeastern Estonia: city settlement region",
  "..Northeastern Estonia: town settlement region",
  "..Northeastern Estonia: rural settlement region",
  "Southern Estonia",
  "..Southern Estonia: city settlement region",
  "..Southern Estonia: town settlement region",
  "..Southern Estonia: rural settlement region",
  "Western Estonia",
  "..Western Estonia: city settlement region",
  "..Western Estonia: town settlement region",
  "..Western Estonia: rural settlement region",
  "..Harju county: city settlement region",
  "..Harju county: town settlement region",
  "..Harju county: rural settlement region"
)

DT <- DT[!grepl("\\bcounty\\b", Municipality, ignore.case = TRUE)]
DT <- DT[!Municipality %in% drop_vals]
DT[, Municipality := trimws(Municipality)]
DT <- DT[grepl("^\\.\\.[^\\.]", Municipality)]

# drop entries containing "excl."
DT <- DT[!grepl("excl\\.", Municipality, ignore.case = TRUE)]
DT[, municipality := gsub("^\\.+\\s*", "", Municipality)]
DT[, Municipality := sub("^\\.\\.", "", Municipality)]



# --------------------------------------------------
# Keep residential buildings, create type, filter
# --------------------------------------------------
DT_clean <- DT[
  type_of_building %in% c(
    "All types of building",
    "Residential building with 1 flat (private house)",
    "Residential building with 2 flats",
    "Residential building with 2 or more flats",
    "Residential building with 3 or more flats"
  ) & occupancy == "Total"
][, type := fifelse(
  type_of_building == "All types of building", "all",
  fifelse(type_of_building == "Residential building with 1 flat (private house)", "house", "flat")
)][, .(municipality = Municipality, year, value, type)]

DT_clean <- DT_clean[, .(value = sum(value, na.rm = TRUE)), by = .(municipality, year, type)]

# --------------------------------------------------
# Harmonise municipality names
# --------------------------------------------------
DT_clean[, municipality := gsub("rural municipality", "vald", municipality, ignore.case = TRUE)]
DT_clean[, municipality := gsub("city", "linn", municipality, ignore.case = TRUE)]
DT_clean$municipality <- tolower(DT_clean$municipality)
DT_clean[, municipality := trimws(municipality)]


# --------------------------------------------------
# Merge with official LAU file
# --------------------------------------------------
lau_off <- fread(file.path(source_lau_path, "EE_LAU2.csv"), encoding = "Latin-1")
lau_off <- lau_off[, c(1:5)]
lau_off$`LAU NAME NATIONAL` <- tolower(lau_off$`LAU NAME NATIONAL`)

DT_merged <- merge(DT_clean, lau_off, by.x = "municipality", by.y = "LAU NAME NATIONAL", all.x = TRUE)
DT_merged <- DT_merged[, c(1:7)]



# Save to CSV in the usual output folder
# Using fwrite (fast, from data.table)
write.csv(
  DT_merged,
  file = file.path(output_path, "EE_dwelling_2000_11_21.csv"),
  row.names = FALSE
)

