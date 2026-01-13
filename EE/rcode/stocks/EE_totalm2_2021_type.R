## ------------------------------------------------------------------
## RL21218: AREA OF CONVENTIONAL DWELLINGS BY TYPE OF BUILDING, 
## OCCUPANCY, COMFORT CHARACTERISTICS AND LOCATION (ADMIN UNIT)
## 31 DECEMBER 2021
## https://andmed.stat.ee/api/v1/en/stat/RL21218
## ------------------------------------------------------------------


url <- "https://andmed.stat.ee/api/v1/en/stat/RL21218"

json_body <- '{
  "query": [
    {
      "code": "Aasta",
      "selection": {
        "filter": "item",
        "values": ["2021"]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
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

head(df)

df<-df[df$`Comfort characteristics`=="Total" & df$Location=="Total",]


setnames(df, c("year", "municipality", "random", "type","indicator", "value"))

setDT(df)
df[, random := NULL]
df$indicator<-"totalm2"



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

df <- df[
  !municipality %in% drop_vals &
    !grepl("county", municipality, ignore.case = TRUE)
]

df <- df[
  !municipality %in% drop_vals &
    !grepl("county|settlement unit|district|v.a suure-jaani and võhma linn asustusüksusena", municipality, ignore.case = TRUE)
]


## --------------------------------------------------
## Keep residential buildings, create type
## --------------------------------------------------

DT_clean <- df[
  type %in% c(
    "All types of building",
    "Residential building with 1 flat (private house)",
    "Residential building with 2 flats",
    "Residential building with 2 or more flats",
    "Residential building with 3 or more flats"
  ) 
][
  ,
  type := fifelse(
    type== "All types of building", "all",
    fifelse(
      type == "Residential building with 1 flat (private house)",
      "house",
      "flat"
    )
  )
][
  ,
  .(municipality, year, value, type)
]

DT_clean <- DT_clean[, .(value = sum(value, na.rm = TRUE)), by = .(municipality, year, type)]
DT_clean[, municipality := gsub("rural municipality", "vald", municipality, ignore.case = TRUE)]
DT_clean[, municipality := gsub("city", "linn", municipality, ignore.case = TRUE)]
DT_clean$municipality <- tolower(DT_clean$municipality)
DT_clean[, municipality := trimws(municipality)]

DT_clean <- DT_clean[!grepl("excl\\.", municipality, ignore.case = TRUE)]
DT_clean[, municipality := gsub("^\\.+\\s*", "", municipality)]
DT_clean[, Municipality := sub("^\\.\\.", "", municipality)]


# --------------------------------------------------
# Merge with official LAU file
# --------------------------------------------------
lau_off <- fread(file.path(source_lau_path, "EE_LAU2.csv"), encoding = "Latin-1")
lau_off <- lau_off[, c(1:5)]
lau_off$`LAU NAME NATIONAL` <- tolower(lau_off$`LAU NAME NATIONAL`)

DT_merged <- merge(DT_clean, lau_off, by.x = "municipality", by.y = "LAU NAME NATIONAL", all.x = TRUE)
DT_merged <- DT_merged[, c(1:4,6:8)]


DT_merged[
  , all_value := value[type == "all"][1],
  by = .(`LAU CODE`, municipality, `NUTS 3 CODE 2016`)
][
  , share_on_all := value / all_value
]

# Save to CSV in the usual output folder
# Using fwrite (fast, from data.table)
write.csv(
  DT_merged,
  file = file.path(output_path, "EE_totalm2_21_type.csv"),
  row.names = FALSE
)
