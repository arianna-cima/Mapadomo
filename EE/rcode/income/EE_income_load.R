## ------------------------------------------------------------------
## PA107: Average monthly gross wages, median and number of employees 21-2024
## ------------------------------------------------------------------

url <- "https://andmed.stat.ee/api/v1/en/stat/PA107"

query <- list(
  query = list(
    list(
      code = "Haldusüksus",
      selection = list(
        filter = "item",
        values = c(
          "EE",
          "EE00370000000000","EE00370141000001","EE00370198000001",
          "EE00370245000001","EE00370296000004","EE00370305000001",
          "EE00370338000001","EE00370353000001","EE00370424000004",
          "EE00370431000001","EE00370446000004","EE00370651000001",
          "EE00370653000001","EE00370719000001","EE00370725000001",
          "EE00370784000004","EE00370890000001","EE00390000000000",
          "EE00390205000001","EE00450000000000","EE00450130000001",
          "EE00450251000001","EE00450321000004","EE00450442000001",
          "EE00450511000004","EE00450514000001","EE00450515000001",
          "EE00450735000004","EE00450736000004","EE00450803000001",
          "EE00500000000000","EE00500247000001","EE00500486000001",
          "EE00500618000001","EE00520000000000","EE00520255000001",
          "EE00520567000001","EE00520834000001","EE00560000000000",
          "EE00560184000001","EE00560441000001","EE00560907000001",
          "EE00600000000000","EE00600191000001","EE00600272000001",
          "EE00600661000001","EE00600663000004","EE00600792000001",
          "EE00600901000001","EE00600903000001","EE00600928000001",
          "EE00640000000000","EE00640284000001","EE00640622000001",
          "EE00640708000001","EE00680000000000","EE00680214000001",
          "EE00680303000001","EE00680430000001","EE00680624000001",
          "EE00680637000001","EE00680638000001","EE00680712000001",
          "EE00680806000001","EE00680809000001","EE00710000000000",
          "EE00710293000001","EE00710317000001","EE00710502000001",
          "EE00710668000001","EE00740000000000","EE00740478000001",
          "EE00740689000001","EE00740714000001","EE00790000000000",
          "EE00790171000001","EE00790283000001","EE00790291000001",
          "EE00790432000001","EE00790528000001","EE00790586000001",
          "EE00790793000001","EE00790796000001","EE00810000000000",
          "EE00810557000001","EE00810824000001","EE00810855000001",
          "EE00810857000001","EE00840000000000","EE00840480000001",
          "EE00840615000001","EE00840897000004","EE00840899000001",
          "EE00870000000000","EE00870142000001","EE00870145000001",
          "EE00870698000001","EE00870732000001","EE00870917000001",
          "EE00870919000004"
        )
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

## ------------------------------------------------------------------
## Keep municipalities and relevant indicators
## ------------------------------------------------------------------

df_muni <- df %>%
  rename(municipality = `Administrative unit`) %>%
  filter(
    grepl("Municipality|City|Tallinn", municipality, ignore.case = TRUE)
  )

## Optional: harmonise indicator names if you want short codes
df_muni <- df_muni %>%
  mutate(
    Indicator = recode(
      Indicator,
      "Average monthly gross wages (salaries), euros" = "avg_wage",
      "Median (5th decile) of monthly gross wages (salaries), euros"  = "median_wage",
      "Number of employees"                    = "employees", 
      "Percentage change in average gross wages (salaries) compared to same period in previous year, %" ="%change"
    )
  )

## ------------------------------------------------------------------
## Prepare municipality data
## ------------------------------------------------------------------

setnames(
  df_muni,
  c("indicator", "municipality", "year",  "value")
)

df_muni <- data.table(df_muni)
df_muni$municipality<-tolower(df_muni$municipality)
## ------------------------------------------------------------------
## Merge with LAU municipality conversion
## ------------------------------------------------------------------

df_muni$municipality <- gsub("rural municipality", "vald", df_muni$municipality, ignore.case = TRUE)
df_muni$municipality <- gsub("\\bcity\\b", "linn", df_muni$municipality, ignore.case = TRUE)

lau_off <- fread(file.path(source_lau_path, "EE_LAU2.csv"), encoding = "Latin-1")
lau_off <- lau_off[, c(1:5,22)]
lau_off$`LAU NAME NATIONAL` <- tolower(lau_off$`LAU NAME NATIONAL`)

DT_merged <- merge(df_muni, lau_off, by.x = "municipality", by.y = "LAU NAME NATIONAL", all.x = TRUE)
DT_merged <- DT_merged[, c(1:7,9)]



## ------------------------------------------------------------------
## Save output
## ------------------------------------------------------------------

write.csv(
  DT_merged,
  file = file.path(output_path, "EE_wages_PA107.csv"),
  row.names = FALSE
)
