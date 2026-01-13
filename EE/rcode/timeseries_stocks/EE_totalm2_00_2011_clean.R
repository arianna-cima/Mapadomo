# ==================================================
# DESCRIPTION OF THE DATA PROCESSING 
# ==================================================
#
#
# ==================================================
# --------------------------------------------------
# 1. Load municipality stock data (already all/house/flat)
# --------------------------------------------------
df_muni <- fread(file.path(output_path, "EE_stock_2003_07.csv"))
df_muni <- df_muni[indicator == "dwelling"]
df_muni$type<-"all"

setnames(
  df_muni,
  c("NUTS 3 CODE 2016","NUTS 3 CODE 2021","LAU CODE"),
  c("nuts2016","nuts2021","laucode")
)

# --------------------------------------------------
# 2. Load census dwelling data (2000 / 2011 / 2021)
# --------------------------------------------------

DT <- fread(file.path(output_path, "EE_conventionalstock_byarea_2000_11_21.csv"))
DT$indicator<-"totalm2"

DT<-DT[type_of_building== "Area of dwelling total",]
DT[, c("type_of_building") := NULL]
DT$type<-"all"
DT_merged<-DT
setnames(
  DT_merged,
  c("NUTS 3 CODE 2016","NUTS 3 CODE 2021","LAU CODE"),
  c("nuts2016","nuts2021","laucode")
)

common_cols <- c("municipality","year","indicator","type","value","nuts2021","nuts2016","laucode")
df_muni   <- df_muni[, ..common_cols]
DT_merged <- DT_merged[, ..common_cols]

stopifnot(identical(colnames(df_muni), colnames(DT_merged)))

# --------------------------------------------------
# 3. Collapse safely (laucode × year × type)
# --------------------------------------------------
df_muni   <- df_muni[,   .(value = sum(value, na.rm = TRUE)),
                     by = .(laucode, year, type, indicator, nuts2021, nuts2016)]

DT_merged <- DT_merged[, .(value = sum(value, na.rm = TRUE)),
                       by = .(laucode, year, type, indicator, nuts2021, nuts2016)]

# --------------------------------------------------
# 4. Combine real data sources
# --------------------------------------------------
DT_all <- rbindlist(list(df_muni, DT_merged), use.names = TRUE)
DT_all <- unique(DT_all, by = c("laucode","year","type"))
DT_all[, municipality := NULL]
setorder(DT_all, laucode, type, year)

# --------------------------------------------------
# 5. Build FULL panel: laucode × year × type
# --------------------------------------------------
missing_years <- c(2001, 2002, 2008:2020)

DT_panel <- DT_all[
  CJ(
    laucode = unique(laucode),
    type    = unique(type),
    year    = unique(c(year, missing_years)),
    unique  = TRUE
  ),
  on = .(laucode, type, year)
]

DT_panel <- DT_panel[
  DT_all,
  on = .(laucode, type, year),
  `:=`(
    indicator = fcoalesce(i.indicator, indicator),
    nuts2021  = fcoalesce(i.nuts2021, nuts2021),
    nuts2016  = fcoalesce(i.nuts2016, nuts2016),
    value     = i.value
  )
]

DT_all <- DT_panel
rm(DT_panel, missing_years)
setorder(DT_all, laucode, type, year)

# --------------------------------------------------
# 6. FILL IDENTIFIERS — robust multi-pass (THIS FIXES 2001)
# --------------------------------------------------
setorder(DT_all, laucode, type, year)

for (cc in c("indicator","nuts2016","nuts2021")) {
  
  DT_all[, (cc) := {
    x <- get(cc)
    
    # forward fill UNTIL convergence
    repeat {
      x_old <- x
      x[is.na(x)] <- shift(x, 1L, type = "lag")[is.na(x)]
      if (identical(x, x_old)) break
    }
    
    # backward fill UNTIL convergence
    repeat {
      x_old <- x
      x[is.na(x)] <- shift(x, 1L, type = "lead")[is.na(x)]
      if (identical(x, x_old)) break
    }
    
    x
  }, by = .(laucode, type)]
}

# --------------------------------------------------
# 7. Linear interpolation (SAFE: laucode × type)
# --------------------------------------------------
DT_all[, value := {
  y <- year
  v <- value
  idx <- !is.na(v)
  
  if (sum(idx) < 2) v else {
    v_int <- approx(x = y[idx], y = v[idx], xout = y, rule = 1)$y
    fifelse(is.na(v), v_int, v)
  }
}, by = .(laucode, type)]




# --------------------------------------------------
# 10. Plot time series by LAU CODE, separately by type
# --------------------------------------------------

out_dir <- file.path(out_check, "dwelling")
#dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

DT_all[, year := as.integer(year)]
setorder(DT_all, laucode, type, year)

cluster_size <- 20
lau_list <- sort(unique(DT_all$laucode))
clusters <- split(lau_list, ceiling(seq_along(lau_list) / cluster_size))

types_list <- sort(unique(DT_all$type))

for (tt in types_list) {
  
  DT_t <- DT_all[type == tt]
  
  for (i in seq_along(clusters)) {
    
    lau_subset <- clusters[[i]]
    
    p <- ggplot(
      DT_t[laucode %in% lau_subset],
      aes(x = year, y = value, group = laucode)
    ) +
      geom_line(color = "steelblue") +
      geom_point(size = 0.8) +
      facet_wrap(~ laucode, scales = "free_y") +
      coord_cartesian(ylim = c(0, NA)) +
      theme_minimal() +
      labs(
        title = paste("Totalm2 stock time series –", tt, "– cluster", i),
        x = "Year",
        y = "Value"
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

# --------------------------------------------------
# 9. Save clean panel
# --------------------------------------------------
write.csv(
  DT_all,
  file = file.path(output_path, "EE_totalm2_timeseries_clean.csv"),
  row.names = FALSE
)
