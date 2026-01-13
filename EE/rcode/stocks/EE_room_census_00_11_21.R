# ==================================================
# RLV2053 – FULL DATA DOWNLOAD + TIDY CONVERSION
# Statistics Estonia (json-stat2)
# ==================================================


# ----------------------------
# 1. API URL
# ----------------------------
url <- "https://andmed.stat.ee/api/v1/en/stat/RLV2053"

# ----------------------------
# 2. RAW JSON body (EXACT as API page)
# ----------------------------
json_body <- '{
  "query": [],
  "response": {
    "format": "json-stat2"
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
writeLines(json_text, "RLV2053_full.json")

# ----------------------------
# 6. Convert json-stat2 → data.frame
# ----------------------------
df <- fromJSONstat("RLV2053_full.json")

# ----------------------------
# 7. Convert to data.table
# ----------------------------
DT <- as.data.table(df)

# ----------------------------
# 8. Ensure numeric values
# ----------------------------
setnames(DT, "value", "dwellings")
DT[, dwellings := as.numeric(dwellings)]

# ----------------------------
# 9. Drop fully missing rows (optional)
# ----------------------------
DT <- DT[!is.na(dwellings)]


# Save to CSV in the usual output folder
# Using fwrite (fast, from data.table)
write.csv(
  DT,
  file = file.path(output_path, "EE_room_2000_11_21.csv"),
  row.names = FALSE
)

