#KVE75: DWELLING STOCK BY REGION/ADMINISTRATIVE UNIT (2003–2007)
#https://andmed.stat.ee/en/stat/Lepetatud_tabelid__Majandus.%20Arhiiv__Kinnisvara.%20Arhiiv__elamumajandus/KVE75
url <- "https://andmed.stat.ee/api/v1/en/stat/KVE75"

query <- list(
  query = list(
    list(
      code = "Piirkond/Haldusüksus",
      selection = list(
        filter = "item",
        values = c(
          "00","EE001","EE006","EE007","EE004","EE008","37","296","424","446",
          "580","728","784","112","140","198","245","295","297","304","337",
          "352","363","423","518","562","651","653","718","727","868","890",
          "39","371","175","392","368","639","44","253","309","322","511",
          "513","645","735","122","154","164","224","229","252","320","323",
          "420","437","449","498","751","802","815","851","49","249","485",
          "617","248","657","573","576","578","611","616","713","773","810",
          "51","566","837","129","134","234","257","271","288","314","325",
          "400","537","565","684","836","937","57","183","195","342","411",
          "452","520","531","552","674","680","776","907","59","345","663",
          "788","791","161","190","273","381","660","662","702","716","770",
          "787","H122","887","900","902","927","65","620","117","285","354",
          "385","465","473","547","619","707","856","872","879","934","67",
          "306","625","741","149","159","188","213","276","303","334","395",
          "568","711","730","756","848","782","805","808","826","863","931",
          "930","70","240","260","277","292","317","375","504","654","669",
          "884","74","349","270","301","373","386","403","440","478","483",
          "550","592","634","689","721","807","858","78","170","279","795",
          "126","185","282","331","383","432","454","501","528","587","595",
          "605","666","694","794","831","861","915","949","82","823","854",
          "203","208","289","636","582","608","613","724","779","820","943",
          "84","490","760","897","912","105","192","600","328","357","360",
          "545","570","629","715","759","797","870","892","86","919","143",
          "181","389","460","468","493","697","767","843","865","874","918",
          "H264"
        )
      )
    )
  ),
  response = list(format = "json-stat2")
)

res <- POST(
  url,
  body = query,
  encode = "json"
)

stop_for_status(res)

json_txt <- content(res, as = "text", encoding = "UTF-8")
json_obj <- fromJSON(json_txt, simplifyVector = FALSE)

df <- fromJSONstat(json_txt)

head(df)

df_muni <- df %>%
  dplyr::rename(region = `Region/Administrative unit`) %>%
  dplyr::filter(
    grepl("Municipality|City|Tallinn", region, ignore.case = TRUE),
    `Change of dwelling stock` == "Dwelling stock at end-year"
  )


df_muni <- df_muni %>%
  mutate(region = gsub("^\\.+", "", region))


df_muni <- df_muni %>%
  mutate(
    Indicator = recode(
      Indicator,
      "Number of dwellings" = "dwelling",
      "Floor area of dwellings, m²" = "totalm2"
    )
  )

## ------------------------------------------------------------------
## Prepare municipality data
## ------------------------------------------------------------------

setnames(df_muni, c("year", "municipality", "random", "indicator", "value"))

df_muni <- data.table(df_muni)
df_muni[, random := NULL]


## ------------------------------------------------------------------
## Merge with LAU municipality conversion
## ------------------------------------------------------------------

lau <- fread(
  file.path(source_lau_path, "lau_municipality_conversion.csv"),
  encoding = "UTF-8"
)

DT_merged <- merge(
  df_muni,
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

DT_merged<-DT_merged[,c(1:5,7:9)]

# Save to CSV in the usual output folder
# Using fwrite (fast, from data.table)
write.csv(
  DT_merged,
  file = file.path(output_path, "EE_stock_2003_07.csv"),
  row.names = FALSE
)
