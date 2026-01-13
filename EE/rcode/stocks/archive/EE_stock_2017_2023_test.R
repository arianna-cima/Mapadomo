

url <- "https://andmed.stat.ee/api/v1/en/stat/KVE01"

query <- list(
  query = list(
    list(
      code = "Maakond",
      selection = list(
        filter = "item",
        values = c(
          "1",  "37", "39", "44", "51", "49", "57", "59",
          "67", "65", "70", "74", "78", "82", "84", "86"
        )
      )
    ),
    list(
      code = "Aasta",
      selection = list(
        filter = "item",
        values = c("2017","2018","2019","2020","2021","2022","2023")
      )
    )
  ),
  response = list(format = "json-stat2")
)

res <- POST(url, body = query, encode = "json")
stop_for_status(res)

json_txt <- content(res, as = "text", encoding = "UTF-8")
df <- fromJSONstat(json_txt)

head(df)
df <- df[
  df$Indicator == "Number of dwellings" &
    df$`Type of dwelling` == "Dwellings, total" &
    df$County != "Whole country",
]