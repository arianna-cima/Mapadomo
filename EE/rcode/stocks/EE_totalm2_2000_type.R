library(httr)
library(jsonstat)
library(data.table)

## ------------------------------------------------------------------
## RL7102: AREA OF CONVENTIONAL DWELLINGS, 31 March 2000
## by Location, Type of dwelling, Occupancy and Owner
## https://andmed.stat.ee/api/v1/en/stat/RL7102
## ------------------------------------------------------------------

url <- "https://andmed.stat.ee/api/v1/en/stat/RL7102"

json_body <- '{
  "query": [
    {
      "code": "Asukoht",
      "selection": {
        "filter": "item",
        "values": [
          "000000","000004","000001","370000","370004","377844","371766",
          "372986","373396","373876","374826","375246","375966","376146",
          "379996","372964","374464","370001","390000","390004","393714",
          "390001","440000","440004","442534","443094","443224","445114",
          "447354","440001","490000","490004","492494","490001","510000",
          "510004","515664","518374","510001","570000","570004","571834",
          "570001","590000","590004","596634","597914","590001","650000",
          "650004","656204","650001","670000","670004","676254","670001",
          "700000","700004","706704","700001","740000","740004","743494",
          "740001","780000","780004","787954","780001","820000","820004",
          "828544","820001","840000","840004","848974","840001","860000",
          "860004","869194","860001"
        ]
      }
    },
    {
      "code": "Eluruumi tüüp",
      "selection": {
        "filter": "item",
        "values": ["+","1","2","3","4"]
      }
    },
    {
      "code": "Asustatus",
      "selection": {
        "filter": "item",
        "values": ["+","1","2","3","4"]
      }
    },
    {
      "code": "Omanik",
      "selection": {
        "filter": "item",
        "values": ["+","1","2","3","4","5","6","7"]
      }
    }
  ],
  "response": { "format": "json-stat2" }
}'

res <- POST(
  url,
  body = json_body,
  encode = "raw",
  add_headers(
    "Content-Type" = "application/json",
    "Accept" = "application/json"
  )
)

stop_for_status(res)

txt <- content(res, "text", encoding = "UTF-8")
df  <- fromJSONstat(txt)

## ------------------------------------------------------------------
## Clean & harmonise
## ------------------------------------------------------------------

setDT(df)

setnames(
  df,
  c(
    "municipality",
    "dwelling_type",
    "occupancy",
    "owner",
    "value"
  )
)

df[, indicator := "totalm2"]

## ------------------------------------------------------------------
## Drop aggregates & non-municipal units
## ------------------------------------------------------------------

df <- df[
  !grepl(
    "county|settlement unit|district",
    municipality,
    ignore.case = TRUE
  ) &
    !municipality %in% c(
      "Whole country",
      "Cities",
      "Rural municipalities",
      "Urban settlements",
      "Rural settlements"
    )
]

## ------------------------------------------------------------------
## Keep totals & harmonise categories
## ------------------------------------------------------------------

df <- df[
  owner == "All owners" &
    occupancy == "Occupancy total"
]

## ------------------------------------------------------------------
## Residential type classification
## ------------------------------------------------------------------

DT_clean <- df[
  ,
  type := fifelse(
    dwelling_type == "All types of dwelling",
    "all",
    fifelse(
      dwelling_type %in% c(
        "Family dwelling",
        "Part of the family dwelling"
      ),
      "house",
      "flat"
    )
  )
][
  ,
  .(municipality, value, type)
]

DT_clean[
  municipality == "..Rapla city",
  municipality := "Rapla rural municipality"
]

DT_clean[, municipality := sub("^\\.\\.", "", municipality)]
DT_clean <- DT_clean[
  !grepl("without municipal status", municipality, ignore.case = TRUE)
]
DT_clean <- DT_clean[
  !grepl("town", municipality, ignore.case = TRUE)
]
DT_clean[
  municipality == "Tallinn city",
  municipality := "Tallinn"
]

DT_sum <- DT_clean[
  ,
  .(value = sum(value, na.rm = TRUE)),
  by = .(municipality, type)
]

## ------------------------------------------------------------------
## Merge with LAU municipality conversion
## ------------------------------------------------------------------

lau <- fread(
  file.path(source_lau_path, "lau_municipality_conversion.csv"),
  encoding = "UTF-8"
)
lau$municipality <- gsub("\\*", "", lau$municipality)

DT_merged <- merge(
  DT_clean,
  lau,
  by.x  = "municipality",
  by.y  = "municipality",
  all.x = TRUE
)

## ------------------------------------------------------------------
## Merge with official LAU file
## ------------------------------------------------------------------

lau_off <- fread(
  file.path(source_lau_path, "EE_LAU2.csv"),
  encoding = "Latin-1"
)

lau_off <- lau_off[, 1:5]

DT_merged$lau_name <- tolower(DT_merged$lau_name)
lau_off$`LAU NAME NATIONAL` <- tolower(lau_off$`LAU NAME NATIONAL`)

DT_merged <- merge(
  DT_merged,
  lau_off,
  by.x  = "lau_name",
  by.y  = "LAU NAME NATIONAL",
  all.x = TRUE
)

DT_merged <- DT_merged[, c(1:4,6:)]

## ------------------------------------------------------------------
## Aggregate & compute shares
## ------------------------------------------------------------------

DT_merged <- DT_merged[
  ,
  .(value = sum(value, na.rm = TRUE)),
  by = .(
    `LAU CODE`,
    lau_name,
    type,
    `NUTS 3 CODE 2016`
  )
]

DT_merged[
  ,
  all_value := value[type == "all"][1],
  by = .(`LAU CODE`, lau_name, `NUTS 3 CODE 2016`)
][
  ,
  share_on_all := value / all_value
]

## ------------------------------------------------------------------
## Save output
## ------------------------------------------------------------------

write.csv(
  DT_merged,
  file = file.path(output_path, "EE_totalm2_2000_type.csv"),
  row.names = FALSE
)
