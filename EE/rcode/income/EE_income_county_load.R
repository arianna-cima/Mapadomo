## ------------------------------------------------------------------
## PA21: Average monthly gross and net wages (salaries)
## ------------------------------------------------------------------

url <- "https://andmed.stat.ee/api/v1/en/stat/PA21"

## API query — selecting only gross and net indicators
query <- list(
  query = list(
    list(
      code = "Näitaja",
      selection = list(
        filter = "item",
        values = c("1", "7")   # 1 = Gross, 7 = Net
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
## Filter municipalities and harmonise admin unit names
## ------------------------------------------------------------------


df_muni <- df %>%
  mutate(
    Indicator = recode(
      Indicator,
      "Average monthly gross wages (salaries), euros"     = "avg_gross_wage",
      "Average monthly net wages (salaries), euros"       = "avg_net_wage"
    )
  )


df_muni <- setDT(df_muni)[, .(value = mean(value, na.rm = TRUE)), by = .(Year, County, Indicator)]
df_muni<-df_muni[Indicator=="avg_gross_wage",]
df_muni[, County := sub("^\\.\\.", "", County)]

dd <- fread(file.path(source_lau_path, "conversion_county_nuts3.csv"))
df_muni <- merge(df_muni,dd,by = "County",all.x = TRUE)

## ------------------------------------------------------------------
## Save output
## ------------------------------------------------------------------

write.csv(
  df_muni,
  file = file.path(output_path, "EE_wages_PA21.csv"),
  row.names = FALSE
)
