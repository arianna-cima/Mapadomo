# ==================================================
# KV03 – KV03: NOTARISED PURCHASE-SALE CONTRACTS by Year, County, Type of purchase-sale contract and Indicator
#1998 1999 2000
# ==================================================

# ----------------------------
# 0. Packages
# ----------------------------
required_pkgs <- c("httr", "jsonlite", "jsonstat", "data.table")

for (p in required_pkgs) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}

# ----------------------------
# 1. API URL
# ----------------------------
url <- "https://andmed.stat.ee/api/v1/en/stat/KV03"

# ----------------------------
# 2. RAW JSON body (EXACT as provided)
# ----------------------------
json_body <- '{
  "query": [
    {
      "code": "Maakond",
      "selection": {
        "filter": "item",
        "values": [
          "00","37","39","44","49","51","57","59",
          "65","67","70","74","78","82","84","86"
        ]
      }
    },
    {
      "code": "Ostu-müügilepingu liik",
      "selection": {
        "filter": "item",
        "values": [
          "1","2","3","4","5","6",
          "7","8","9","10","11"
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
writeLines(json_text, "KV03_full.json")

# ----------------------------
# 6. Convert json-stat2 → data.frame
# ----------------------------
df <- fromJSONstat("KV03_full.json")

# ----------------------------
# 7. Convert to data.table
# ----------------------------
DT <- as.data.table(df)



# Save to CSV in the usual output folder
# Using fwrite (fast, from data.table)
write.csv(
  DT,
  file = file.path(price_path, "EE_transactionvalue_1998_2000.csv"),
  row.names = FALSE
)
