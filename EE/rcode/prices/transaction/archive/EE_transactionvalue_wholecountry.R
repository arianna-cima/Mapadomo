# ==================================================
# KV010 – PKV010: NOTARISED PURCHASE-SALE CONTRACTS OF REAL ESTATE by Year, Type of purchase-sale contract and Indicator
# ==================================================

# ----------------------------
# 1. API URL
# ----------------------------
url <- "https://andmed.stat.ee/api/v1/en/stat/KV010"

# ----------------------------
# 2. RAW JSON body (EXACT as website)
# ----------------------------
json_body <- '{
  "query": [
    {
      "code": "Ostu-müügilepingu liik",
      "selection": {
        "filter": "item",
        "values": ["1", "2", "3", "4", "5"]
      }
    }
  ],
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
writeLines(json_text, "KV010_full.json")

# ----------------------------
# 6. Convert json-stat2 → data.frame
# ----------------------------
df <- fromJSONstat("KV010_full.json")

# ----------------------------
# 7. Convert to data.table
# ----------------------------
DT <- as.data.table(df)
