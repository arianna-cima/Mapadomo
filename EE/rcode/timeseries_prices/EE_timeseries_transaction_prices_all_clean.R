tt <- fread(file.path(output_path, "EE_LAU_ppm2_smoothed.csv"))

tm <- fread(file.path(output_path, "EE_totalm2_timeseries_clean.csv"))
tm <- tm[level == "laucode"]

lau_off <- fread(file.path(source_lau_path, "EE_LAU2.csv"),encoding = "Latin-1")
lau_off <- lau_off[, c(1:3)]

# merge tt with lau_off by nuts (tt) and LAU CODE (lau_off)
tt <- merge(tt,lau_off, by.x = "laucode",by.y = "LAU CODE",all.x = TRUE)
lau_off$`LAU CODE`<-as.character(lau_off$`LAU CODE`)

tm<- merge(tm,lau_off, by.x = "nuts",by.y = "LAU CODE",all.x = TRUE)
tt[, nuts := NULL]
setnames(tt, "laucode", "nuts")
tt$nuts<-as.character(tt$nuts)
tm$nuts<-as.character(tm$nuts)
## --------------------------------------------------
## 1. Keep only totalm2 needed for weights
## --------------------------------------------------

tm_w <- tm[
  indicator == "totalm2",
  .(nuts, year, type, totalm2 = value, `NUTS 3 CODE 2016`)
]

## --------------------------------------------------
## 2. Merge price with totalm2 weights (LAU level)
## --------------------------------------------------

DT_price_w <- merge(
  tt,
  tm_w,
  by = c( "nuts", "year", "type", "NUTS 3 CODE 2016"),
  all.x = FALSE,
  all.y = FALSE
)

## --------------------------------------------------
## 3. Helper: weighted aggregation from NUTS-3 prefixes
## --------------------------------------------------

weighted_price_from_nuts3 <- function(DT, substr_len) {
  
  DT[
    ,
    .(
      value = sum(value * totalm2, na.rm = TRUE) /
        sum(totalm2, na.rm = TRUE)
    ),
    by = .(
      nuts = substr(`NUTS 3 CODE 2016`, 1, substr_len),
      year,
      type
    )
  ][
    ,
    `:=`(
      indicator = "ppm2",
      level = fifelse(
        substr_len == 5, "nuts3",
        fifelse(
          substr_len == 4, "nuts2",
          fifelse(substr_len == 3, "nuts1", "nuts0")
        )
      )
    )
  ]
}

## --------------------------------------------------
## 4. Build prices at all NUTS levels
## --------------------------------------------------

DT_nuts3 <- weighted_price_from_nuts3(DT_price_w, 5)
DT_nuts2 <- weighted_price_from_nuts3(DT_price_w, 4)
DT_nuts1 <- weighted_price_from_nuts3(DT_price_w, 3)
DT_nuts0 <- weighted_price_from_nuts3(DT_price_w, 2)

## --------------------------------------------------
## 5. Combine all price datasets
## --------------------------------------------------

price_all_levels <- rbindlist(
  list(
    tt[, .(nuts , year, type, indicator, value, level = "laucode")],
    DT_nuts3,
    DT_nuts2,
    DT_nuts1,
    DT_nuts0
  ),
  use.names = TRUE,
  fill = TRUE
)



# Optional save
fwrite(
  price_all_levels,
  file.path(output_path, "EE_LAU_ppm2_clean.csv")
)