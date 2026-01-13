#RLV215: CONVENTIONAL DWELLINGS by Location, Type of building, Year and Occupancy

# ----------------------------
# 1. URL
# ----------------------------
url <- "https://andmed.stat.ee/api/v1/en/stat/RLV215"

# ----------------------------
# 2. JSON body (your query, translated 1-to-1 into R)
# ----------------------------
body <- list(
  query = list(
    list(
      code = "Hoone tüüp",
      selection = list(
        filter = "item",
        values = c("2", "3", "4", "5")
      )
    ),
    list(
      code = "Asustatus",
      selection = list(
        filter = "item",
        values = c("1")
      )
    )
  ),
  response = list(
    format = "json-stat2"
  )
)

# ----------------------------
# 3. POST request (IMPORTANT: header)
# ----------------------------
res <- POST(
  url,
  body = body,
  encode = "json",
  add_headers(`Content-Type` = "application/json")
)

stop_for_status(res)