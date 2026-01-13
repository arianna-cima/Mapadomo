## --------------------------------------------------
## Load and prepare total area (ALL)
## --------------------------------------------------
area <- fread(file.path(output_path, "EE_stock_2003_07.csv"))
area <- area[indicator == "totalm2"]
area$type<-"all"

area <- area[
  ,
  .(value = sum(value, na.rm = TRUE)),
  by = .(
    `LAU CODE`,
    lau_name,
    type,
    `NUTS 3 CODE 2016`, year, `NUTS 3 CODE 2021`
  )
]

## --------------------------------------------------
## Load area-by-type datasets (shares already computed)
## --------------------------------------------------

area21 <- fread(file.path(output_path, "EE_totalm2_21_type.csv"))
area11 <- fread(file.path(output_path, "EE_totalm2_11_type.csv"))

shares11 <- area11[
  type %in% c("flat", "house"),
  .(`LAU CODE`, type, share_on_all)
]

shares21 <- area21[
  type %in% c("flat", "house"),
  .(`LAU CODE`, type, share_on_all)
]



# Merge total area with 2011 shares
area_flat_house <- merge(
  area,
  shares11,
  by = "LAU CODE",
  allow.cartesian = TRUE
)

# Compute type-specific area
area_flat_house[, value := value * share_on_all]

# Replace "all" with flat / house
area_flat_house[, type := type.y]

# Clean up
area_flat_house[, c("type.x", "type.y", "share_on_all") := NULL]

# (Optional) Keep also the original "all" rows
area_final <- rbindlist(
  list(area, area_flat_house),
  use.names = TRUE,
  fill = TRUE
)

# Order nicely
setorder(area_final, `LAU CODE`, year, type)

area11[, c("all_value", "share_on_all") := NULL]
area11$year<-"2011"

area21[, c("all_value", "share_on_all") := NULL]
area21$year<-"2021"
setnames(area21, "municipality", "lau_name")

area_final <- rbindlist(
  list(area_final, area11, area21),
  use.names = TRUE,
  fill = TRUE
)





setDT(area_final)
area_final$year<-as.numeric(area_final$year)
# Keep full period
DT <- area_final[year >= 2003 & year <= 2021]

# Create full panel
DT_full <- DT[
  CJ(
    `LAU CODE` = unique(DT$`LAU CODE`),
    type       = unique(DT$type),
    year       = 2003:2021
  ),
  on = .(`LAU CODE`, type, year)
]

# Carry forward/backward identifiers
DT_full[
  ,
  `:=`(
    lau_name = zoo::na.locf(lau_name, na.rm = FALSE),
    `NUTS 3 CODE 2016` = zoo::na.locf(`NUTS 3 CODE 2016`, na.rm = FALSE),
    `NUTS 3 CODE 2021` = zoo::na.locf(`NUTS 3 CODE 2021`, na.rm = FALSE)
  ),
  by = .(`LAU CODE`, type)
]

# Store original values
DT_full[, value_orig := value]

# Linear interpolation (full, then restrict)
DT_full[
  ,
  value_interp := zoo::na.approx(value, x = year, na.rm = FALSE),
  by = .(`LAU CODE`, type)
]

# Apply interpolation ONLY to target years
DT_full[
  year %in% c(2008:2010, 2012:2020),
  value := value_interp
]

# Restore observed years explicitly
DT_full[
  !year %in% c(2008:2010, 2012:2020),
  value := value_orig
]

# Flag imputed observations
DT_full[
  ,
  imputed := year %in% c(2008:2010, 2012:2020) & is.na(value_orig)
]

# Clean up
DT_full[, c("value_orig", "value_interp") := NULL]

# Final dataset
area_final_complete <- DT_full




library(data.table)
library(ggplot2)

## --------------------------------------------------
## Plot time series of totalm2 by LAU CODE and type
## --------------------------------------------------

# Ensure data.table
setDT(area_final_complete)

# Output directory
out_dir <- "C:/Users/annai/Documents/backup_computer_ec/Contratti_lavoro/EU_Temporary_agent/materiali_di_lavoro/mapadomo/Mapadomo_v2/EE/outputdata/checks_graphs/totalm2"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Prepare plotting data
DT_all <- copy(area_final_complete)
DT_all[, year := as.integer(year)]
DT_all[, imputed := as.logical(imputed)]
setorder(DT_all, `LAU CODE`, type, year)

# Clustering
cluster_size <- 20
lau_list <- sort(unique(DT_all$`LAU CODE`))
clusters <- split(
  lau_list,
  ceiling(seq_along(lau_list) / cluster_size)
)

types_list <- sort(unique(DT_all$type))

## --------------------------------------------------
## Loop over types and LAU clusters
## --------------------------------------------------

for (tt in types_list) {
  
  DT_t <- DT_all[type == tt]
  
  for (i in seq_along(clusters)) {
    
    lau_subset <- clusters[[i]]
    
    p <- ggplot(
      DT_t[`LAU CODE` %in% lau_subset],
      aes(
        x = year,
        y = value,
        group = `LAU CODE`,
        color = imputed
      )
    ) +
      geom_line(color = "steelblue") +
      geom_point(size = 1.2) +
      scale_color_manual(
        values = c(`FALSE` = "black", `TRUE` = "red"),
        labels = c("Observed", "Imputed"),
        name = "Data status"
      ) +
      facet_wrap(~ `LAU CODE`, scales = "free_y") +
      coord_cartesian(ylim = c(0, NA)) +
      theme_minimal() +
      theme(
        legend.position = "bottom"
      ) +
      labs(
        title = paste("Total dwelling area (m²) –", tt, "– cluster", i),
        x = "Year",
        y = "Total m²"
      )
    
    ggsave(
      filename = file.path(
        out_dir,
        paste0("totalm2_timeseries_", tt, "_cluster_", i, ".png")
      ),
      plot = p,
      width = 14,
      height = 10,
      dpi = 300
    )
  }
}




setDT(DT_all)

## --------------------------------------------------
## Helper: aggregate from NUTS-3 to higher NUTS levels
## --------------------------------------------------

aggregate_from_nuts3 <- function(DT, substr_len) {
  
  DT_agg <- DT[
    ,
    .(
      value = sum(value, na.rm = TRUE),
      imputed = all(imputed)
    ),
    by = .(
      nuts_code = substr(`NUTS 3 CODE 2016`, 1, substr_len),
      type,
      year
    )
  ]
  
  DT_agg[
    ,
    `:=`(
      `LAU CODE` = nuts_code,
      lau_name = nuts_code,
      `NUTS 3 CODE 2016` = nuts_code,
      `NUTS 3 CODE 2021` = nuts_code
    )
  ]
  
  DT_agg[, nuts_code := NULL]
  setcolorder(DT_agg, names(DT_all))
  DT_agg
}

## --------------------------------------------------
## Build NUTS levels
## --------------------------------------------------

# NUTS-3 (direct aggregation)
DT_nuts3 <- aggregate_from_nuts3(DT_all, substr_len = 5)

# NUTS-2 (EE00*)
DT_nuts2 <- aggregate_from_nuts3(DT_all, substr_len = 4)

# NUTS-1 (EE0)
DT_nuts1 <- aggregate_from_nuts3(DT_all, substr_len = 3)

# NUTS-0 (EE)
DT_nuts0 <- aggregate_from_nuts3(DT_all, substr_len = 2)

## --------------------------------------------------
## Append everything
## --------------------------------------------------

DT_all_extended <- rbindlist(
  list(
    DT_all,     # LAU
    DT_nuts3,
    DT_nuts2,
    DT_nuts1,
    DT_nuts0
  ),
  use.names = TRUE,
  fill = TRUE
)


DT_all_extended[
  ,
  level := fifelse(
    # LAU: numeric codes (do NOT start with EE)
    !grepl("^EE", as.character(`LAU CODE`)), "laucode",
    fifelse(
      # NUTS 3: EE007
      grepl("^EE", `LAU CODE`) & nchar(`LAU CODE`) == 5, "nuts3",
      fifelse(
        # NUTS 2: EE00
        grepl("^EE", `LAU CODE`) & nchar(`LAU CODE`) == 4, "nuts2",
        fifelse(
          # NUTS 1: EE0
          grepl("^EE", `LAU CODE`) & nchar(`LAU CODE`) == 3, "nuts1",
          # NUTS 0: EE
          "nuts0"
        )
      )
    )
  )
]
## (Optional) make it an ordered factor
DT_all_extended[
  ,
  level := factor(level, levels = c("laucode", "nuts3", "nuts2", "nuts1", "nuts0"))
]

## --------------------------------------------------
## Save clean interpolated panel
## --------------------------------------------------

# Drop columns
DT_all_extended[,c("lau_name", "NUTS 3 CODE 2016", "NUTS 3 CODE 2021", "imputed") := NULL]

# Rename LAU CODE to nuts
setnames(DT_all_extended, "LAU CODE", "nuts")

# Add indicator
DT_all_extended[, indicator := "totalm2"]

# Melt to long format
DT_long <- melt(
  DT_all_extended,
  id.vars = c("nuts", "year", "type", "indicator","level"),
  measure.vars = "value",
  value.name = "value"
)
DT_long[,c("variable") := NULL]


write.csv(
  DT_long,
  file = file.path(output_path, "EE_totalm2_timeseries_clean.csv"),
  row.names = FALSE
)
