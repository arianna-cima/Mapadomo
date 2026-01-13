# ==================================================
# EH44: DWELLING COMPLETIONS (NEW CONSTRUCTION)
# by Year, Region/Administrative unit, Number of rooms and Indicator
# ==================================================

# ----------------------------
# 1. API URL
# ----------------------------
url <- "https://andmed.stat.ee/api/v1/en/stat/EH44"

# ----------------------------
# 2. RAW JSON body (exactly as API requires)
# ----------------------------
json_body <- '{
  "query": [
    {
      "code": "Piirkond/Haldusüksus",
      "selection": {
        "filter": "item",
        "values": [
          "00","H2","H3","H4","H5","H6",
          "37","290","296","424","446","580","728","784",
          "112","140","198","245","295","297","304","337","353","363","423","518","562","651","653","718","727","868","890",
          "39","371","175","204","392","368","639",
          "44","253","309","322","511","513","645","735",
          "122","154","164","224","229","251","320","323","420","437","449","498","751","802","815","851",
          "49","249","485","617","248","657","573","576","578","611","616","713","773","810",
          "51","566","837","129","134","234","257","271","288","314","325","400","537","565","684","835","937",
          "57","183","195","342","411","436","452","520","531","552","674","680","776","907",
          "59","345","663","788","791","161","190","272","381","660","662","702","716","770","786","790","887","900","902","926",
          "65","620","705","117","285","354","385","465","473","547","619","707","856","872","879","934",
          "67","306","625","741","149","159","188","213","276","303","334","395","568","710","730","756","848","782","805","808","826","863","931","930",
          "70","670","240","260","277","292","318","317","375","427","505","504","654","669","884",
          "74","349","270","301","373","386","403","433","440","478","483","550","592","634","689","721","807","858",
          "78","170","279","795","126","185","282","331","383","432","454","501","528","587","595","605","666","694","794","831","861","915","949",
          "82","823","854","203","208","289","636","582","608","613","724","779","820","943",
          "84","490","760","897","912","105","192","600","328","357","360","545","570","629","715","758","797","870","892","898",
          "86","919","143","181","389","460","468","493","697","767","843","865","874","918"
        ]
      }
    },
    {
      "code": "Tubade arv",
      "selection": {
        "filter": "item",
        "values": ["1"]
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
writeLines(json_text, "EH44_rooms1_full.json")

# ----------------------------
# 6. Convert json-stat → data frame
# ----------------------------
df <- fromJSONstat("EH44_rooms1_full.json")

DT <- as.data.table(df)

# ----------------------------
# 7. Rename columns
# ----------------------------
colnames(DT) <- c("year","Municipality","random","indicator","value")

# ----------------------------
# 8. Basic cleaning
# ----------------------------

# Convert value to numeric
DT[, value := as.numeric(value)]



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

DT <- DT[!grepl("\\bcounty\\b", Municipality, ignore.case = TRUE)]
DT <- DT[!Municipality %in% drop_vals]
DT[, Municipality := trimws(Municipality)]
DT <- DT[grepl("^\\.\\.[^\\.]", Municipality)]

# drop entries containing "excl."
DT <- DT[!grepl("excl\\.", Municipality, ignore.case = TRUE)]
DT[, Municipality := gsub("^\\.+\\s*", "", Municipality)]
DT[, Municipality := sub("^\\.\\.", "", Municipality)]


