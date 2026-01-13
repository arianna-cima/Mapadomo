library(data.table)

# --------------------------------------------------
# Read data
# --------------------------------------------------
municipality <- fread(file.path(output_path, "EE_wages_PA107.csv"))
county <- fread(file.path(output_path, "EE_wages_PA21.csv"))

# --------------------------------------------------
# Standardize column names
# --------------------------------------------------
setnames(municipality, tolower(names(municipality)))
setnames(county, tolower(names(county)))

# --------------------------------------------------
# Keep only wage indicators
# --------------------------------------------------
municipality <- municipality[indicator == "avg_wage"]
county <- county[indicator == "avg_gross_wage"]

# --------------------------------------------------
# Rename columns (only what is needed)
# --------------------------------------------------
setnames(municipality, "municipality", "muni")
setnames(municipality, "nuts 3 code 2016", "nuts")
setnames(municipality, "lau code", "lau")
setnames(county, "value", "county_value")


# Ensure data.table
setDT(municipality)

# 1. Create base municipality name (strip ", until YYYY")
municipality[, muni_base := sub(", until.*$", "", muni)]

# 2. Extract end year from names like ", until 2023"
municipality[
  ,
  until_year := as.integer(
    sub(".*until ([0-9]{4}).*", "\\1", muni)
  )
]

# 3. Drop observations that should no longer apply
#    (i.e. "until YYYY" used after that year)
municipality <- municipality[
  is.na(until_year) | year <= until_year
]

# 4. Collapse to a unique municipality × year
municipality <- municipality[
  ,
  .(
    value = value[!is.na(value)][1],
    nuts  = nuts[!is.na(nuts)][1],
    lau   = lau[!is.na(lau)][1],
    county   = county[!is.na(county)][1]
  ),
  by = .(muni_base, indicator, year)
]

# 5. Final clean name
setnames(municipality, "muni_base", "muni")
#municipality[, until_year := NULL]


# --------------------------------------------------
# 1) Compute NUTS3 mean wage using municipality data (2021–2024)
# --------------------------------------------------
nuts_mean <- municipality[
  year >= 2021 & year <= 2024,
  .(nuts_mean = mean(value, na.rm = TRUE)),
  by = nuts
]

# --------------------------------------------------
# 2) Compute municipality / NUTS3 ratio (share)
# --------------------------------------------------
muni_ratio <- merge(
  municipality,
  nuts_mean,
  by = "nuts",
  all.x = TRUE
)

muni_ratio <- muni_ratio[
  year >= 2021 & year <= 2024,
  .(theta = mean(value / nuts_mean, na.rm = TRUE)),
  by = .(muni, lau, nuts, county)
]

setorder(muni_ratio, muni)

# --------------------------------------------------
# 3) Backcast municipality wages using ratios (2000–2018)
# --------------------------------------------------
setDT(muni_ratio)

muni_ratio <- rbindlist(
  lapply(2000:2018, function(y) {
    copy(muni_ratio)[, year := y]
  })
)

muni_imputed <- merge(
  muni_ratio,
  county[, .(nuts, year, county_value, county)],
  by =c("county","year"),
  allow.cartesian = TRUE
)

setorder(muni_imputed, muni)


muni_imputed <- muni_imputed[
  year <= 2018,
  .(
    muni,
    lau,
    year,
    indicator = "avg_wage",
    value = theta * county_value,
    nuts.x
  )
]
setorder(muni_imputed, muni, year)
setnames(muni_imputed, "nuts.x", "nuts")

# --------------------------------------------------
# 4) Keep observed municipality data (2021–2024)
# --------------------------------------------------
muni_true <- municipality[
  year >= 2021,
  .(muni, lau, year, indicator, value, nuts)
]

# --------------------------------------------------
# 5) Combine into longest municipality-level panel
# --------------------------------------------------
municipality_long <- rbindlist(
  list(muni_imputed, muni_true),
  use.names = TRUE
)

setorder(municipality_long, muni, year)
municipality_long



# -------------------------------
# choose the cluster (NUTS3 code)
# -------------------------------
cluster_nuts <- "EE001"   # change to EE001, EE004, etc.

# -------------------------------
# subset data
# -------------------------------
plot_data <- municipality_long[
  nuts == cluster_nuts
]

# -------------------------------
# plot time series
# -------------------------------
ggplot(plot_data, aes(x = year, y = value, group = muni, color = muni)) +
  geom_line(alpha = 0.6) +
  labs(
    title = paste("Municipality wage time series – NUTS3", cluster_nuts),
    x = "Year",
    y = "Average wage"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )




county <- fread(file.path(output_path, "EE_wages_PA21.csv"))
setDT(county)

# Ensure nuts is character
county[, nuts := as.character(nuts)]

## --------------------------------------------------
## Helper: aggregate counties to higher NUTS levels
## --------------------------------------------------

avg_from_nuts3 <- function(DT, substr_len, level_name) {
  
  DT[
    ,
    .(
      value = mean(value, na.rm = TRUE)
    ),
    by = .(
      nuts = substr(nuts, 1, substr_len),
      Year,
      Indicator
    )
  ][
    ,
    level := level_name
  ]
}

## --------------------------------------------------
## Build NUTS levels
## --------------------------------------------------

# NUTS-3 (already implicit, but standardise)
DT_nuts3 <- county[
  ,
  .(value = mean(value, na.rm = TRUE)),
  by = .(nuts, Year, Indicator)
][
  ,
  level := "nuts3"
]

# NUTS-2 (EE00)
DT_nuts2 <- avg_from_nuts3(county, 4, "nuts2")

# NUTS-1 (EE0)
DT_nuts1 <- avg_from_nuts3(county, 3, "nuts1")

# NUTS-0 (EE)
DT_nuts0 <- avg_from_nuts3(county, 2, "nuts0")

## --------------------------------------------------
## Combine all levels
## --------------------------------------------------

county_nuts_all <- rbindlist(
  list(DT_nuts3, DT_nuts2, DT_nuts1, DT_nuts0),
  use.names = TRUE
)


municipality_long<-municipality_long[,c("lau","year","indicator","value")]
municipality_long$level<-"laucode"
municipality_long$indicator<-"avg_gross_wage"
setnames(municipality_long, "lau", "nuts")


setnames(county_nuts_all, "Indicator", "indicator")
setnames(county_nuts_all, "Year", "year")

income<- rbind(municipality_long, county_nuts_all)

write.csv(
  income,
  file = file.path(output_path, "EE_income_clean.csv"),
  row.names = FALSE
)
