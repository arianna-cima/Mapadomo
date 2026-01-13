# ==================================================
# KV042 – KV042: NOTARISED PURCHASE-SALE TRANSACTIONS OF REAL ESTATE by Year, Object, Location of the object, Indicator and Quarter
#2007-2016
# ==================================================

# ----------------------------
# 1. API URL
# ----------------------------
url <- "https://andmed.stat.ee/api/v1/en/stat/KV042"

# ----------------------------
# 2. RAW JSON body (EXACT as provided)
# ----------------------------
json_body <- '{
  "query": [
    {
      "code": "Objekt",
      "selection": {
        "filter": "item",
        "values": [
          "1","2","3","4","5","6",
          "7","8","9","10","11"
        ]
      }
    },
    {
      "code": "Objekti asukoht",
      "selection": {
        "filter": "item",
        "values": [
          "00","37","794","39","44","49","51","57","59",
          "65","67","625","70","74","78","795","82","84","86"
        ]
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
writeLines(json_text, "KV042_full.json")

# ----------------------------
# 6. Convert json-stat2 → data.frame
# ----------------------------
df <- fromJSONstat("KV042_full.json")

# ----------------------------
# 7. Convert to data.table
# ----------------------------
DT <- as.data.table(df)


# Save to CSV in the usual output folder
# Using fwrite (fast, from data.table)
write.csv(
  DT,
  file = file.path(source_prices_path, "transaction_prices/EE_transactionvalue_2007_2017.csv"),
  row.names = FALSE
)

