library(httr)
library(jsonstat)
library(data.table)

## ------------------------------------------------------------------
## RL0204: AREA OF CONVENTIONAL DWELLINGS, 31 DECEMBER 2011
## by Occupancy, Location, Owner and Type of building
## https://andmed.stat.ee/api/v1/en/stat/RL0204
## ------------------------------------------------------------------

url <- "https://andmed.stat.ee/api/v1/en/stat/RL0204"

json_body <- '{
  "query": [
    {
      "code": "Asustatus",
      "selection": {
        "filter": "item",
        "values": ["TOTAL","1","2","9"]
      }
    },
    {
      "code": "Asukoht",
      "selection": {
        "filter": "item",
        "values": [
          "EE00","AS_LIN","AS_MAA","LIN_4","VAL_1",
          "37","0037_4","784","176","298","339","387","482","524","596","614",
          "296","424","446","580","728","0037_1","112","1088","140","2928",
          "198","245","295","297","304","3039","337","353","363","518","562",
          "651","653","718","727","868","890","39","0039_4","371","0039_1",
          "175","392","368","639","44","0044_4","309","322","511","513","645",
          "735","0044_1","122","154","164","224","229","251","2270","320",
          "323","3270","420","437","449","498","751","802","815","851","49",
          "0049_4","249","485","617","0049_1","248","657","573","576","578",
          "611","616","713","773","810","51","0051_4","566","0051_1","129",
          "134","234","257","2341","288","314","325","565","684","835","8595",
          "937","57","0057_4","183","0057_1","195","342","411","4330","452",
          "520","531","552","674","680","776","907","59","0059_4","345","663",
          "0059_1","190","272","381","660","662","702","770","786","8130",
          "790","8140","887","900","902","926","65","0065_4","620","0065_1",
          "117","285","354","385","465","473","547","619","707","7216","856",
          "872","879","934","67","0067_4","625","741","0067_1","149","159",
          "188","6617","213","303","334","395","4165","568","710","3083",
          "730","756","848","805","8316","808","826","863","929","931",
          "9663","70","0070_1","240","260","2346","277","292","317","3268",
          "375","504","5280","654","669","6826","884","74","0074_4","349",
          "0074_1","270","301","373","386","403","440","478","483","550",
          "592","634","689","721","807","858","78","0078_4","170","279",
          "795","0078_1","126","185","282","331","383","432","454","501",
          "528","587","595","605","666","694","794","831","861","915","949",
          "82","0082_4","823","854","0082_1","203","208","289","636","5754",
          "582","608","613","724","779","820","943","84","0084_4","490",
          "897","912","0084_1","105","1060","192","600","2761","328","357",
          "360","570","629","715","758","7836","797","892","86","0086_4",
          "919","0086_1","143","1301","181","389","460","468","493","697",
          "767","843","865","874","918"
        ]
      }
    },
    {
      "code": "Omanik",
      "selection": {
        "filter": "item",
        "values": ["TOTAL","1","2","3","4","9"]
      }
    },
    {
      "code": "Hoone liik",
      "selection": {
        "filter": "item",
        "values": ["TOTAL","1","2","3","4"]
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
    "occupancy",
    "municipality",
    "owner",
    "type_of_building",
    "value"
  )
)

df[, indicator := "totalm2"]

## ------------------------------------------------------------------
## Drop aggregates, settlement units, districts
## ------------------------------------------------------------------

df <- df[
  !grepl("county|settlement unit|district", municipality, ignore.case = TRUE) &
    !municipality %in% c(
      "Whole country",
      "Northern Estonia",
      "Central Estonia",
      "Southern Estonia",
      "Western Estonia",
      "Northeastern Estonia",
      "Cities",
      "Rural municipalities",
      "Urban settlements",
      "Rural settlements"
    )
]

df<-df[df$owner=="All owners"]
df<-df[df$occupancy=="Occupancy total"]
df<-df[df$type_of_building!="Non-residential building with dwelling(s)"]

## ------------------------------------------------------------------
## Keep residential buildings & create type
## ------------------------------------------------------------------

DT_clean <- df[
  type_of_building %in% c(
    "All types of building",
    "Apartment building",
    "One-family dwelling",
    "Other small residential building",
    "Non-residential building with dwelling(s)"
  ) 
][
  ,
  type := fifelse(
    type_of_building == "All types of building", "all",
    fifelse(
      grepl("One-family dwelling", type_of_building),
      "house",
      "flat"
    )
  )
][
  ,
  .(municipality, owner, value, type)
]

DT_clean[, owner := NULL]
DT_clean[, municipality := sub("^\\.\\.", "", municipality)]
DT_clean <- DT_clean[!grepl("without municipal status", municipality, ignore.case = TRUE)]
DT_clean <- DT_clean[!grepl("town", municipality, ignore.case = TRUE)]
DT_clean[municipality == "Tallinn city", municipality := "Tallinn"]

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
  all.x = TRUE   # keep all df_muni rows
)


## ------------------------------------------------------------------
## Merge with official LAU file
## ------------------------------------------------------------------

lau_off <- fread(
  file.path(source_lau_path, "EE_LAU2.csv"),
  encoding = "Latin-1"
)

lau_off <- lau_off[, c(1:5)]


## ------------------------------------------------------------------
## Harmonise names and final merge
## ------------------------------------------------------------------

DT_merged$lau_name <- tolower(DT_merged$lau_name)
lau_off$`LAU NAME NATIONAL` <- tolower(lau_off$`LAU NAME NATIONAL`)

DT_merged <- merge(
  DT_merged,
  lau_off,
  by.x  = "lau_name",
  by.y  = "LAU NAME NATIONAL",
  all.x = TRUE
)

DT_merged<-DT_merged[,c(1:4,6:9)]
DT_merged <- DT_merged[
  ,
  .(value = sum(value, na.rm = TRUE)),
  by = .(
    `LAU CODE`,
    lau_name,
    type,
    `NUTS 3 CODE 2016`, `NUTS 3 CODE 2021`
  )
]

DT_merged[
  , all_value := value[type == "all"][1],
  by = .(`LAU CODE`,  lau_name, `NUTS 3 CODE 2016`, `NUTS 3 CODE 2021`)
][
  , share_on_all := value / all_value
]



# Save to CSV in the usual output folder
# Using fwrite (fast, from data.table)
write.csv(
  DT_merged,
  file = file.path(output_path, "EE_totalm2_11_type.csv"),
  row.names = FALSE
)