# ==============================================================================
# DESCRIPTION
# ------------------------------------------------------------------------------
# This script constructs a harmonised panel of residential property prices at the
# LAU (municipality) level in Estonia, combining information on flats, houses,
# and their aggregate. It first loads transaction-based price data for flats and
# houses, cleans and harmonises municipality names, and merges the data with
# official LAU codes. 
#
# To address excess volatility at the municipal level due to thin transaction
# volumes, the script applies a smoothing procedure that combines local price
# growth with national house price index (HPI) growth. For flats, the apartments
# HPI is used; for houses, the houses HPI is used; and for the aggregate ("all")
# category, the national HPI is defined as the average of the two. Smoothed prices
# are constructed by blending LAU-level and national log growth rates and
# reconstructing price levels anchored at a common base year.
#
# The script produces (i) LAU-level smoothed price series for flats, houses, and
# all dwellings, (ii) diagnostic plots comparing raw and smoothed prices for
# clusters of municipalities, and (iii) a final stacked dataset 
# ==============================================================================
#==================================================
# 0. Load data
#==================================================

flat <- fread(
  file.path(source_prices_path, "EE_price_flat_2003_2026.csv"),
  encoding = "UTF-8"
)

house <- fread(
  file.path(source_prices_path, "EE_price_house_2003_2026.csv"),
  encoding = "UTF-8"
)

#==================================================
# 1. Combine flat and house
#==================================================

all_dt <- rbindlist(list(flat, house), use.names = TRUE)

avg_by_vald_year <- all_dt[
  ,
  .(average_price_final = mean(average_price_final, na.rm = TRUE)),
  by = .(vald, year)
]

#==================================================
# 2. Create TYPE == "all" (weighted averages)
#==================================================

all_weighted <- all_dt[
  ,
  {
    w <- number
    w[is.na(w)] <- 0.5  # assign 0.5 weight when missing
    
    .(
      number = sum(number, na.rm = TRUE),
      average_price_final = weighted.mean(average_price_final, w, na.rm = TRUE),
      median_price_final  = weighted.mean(median_price_final,  w, na.rm = TRUE)
    )
  },
  by = .(vald, year)
]


all_weighted[, type := "all"]

all_weighted <- merge(all_weighted, all_dt[, c(1:4, 6:7)], by = c("vald","year"), all.x = TRUE)

all_weighted <- all_weighted[, .SD[1], by = .(vald, year)]
#==================================================
# 3. Append back
#==================================================

all_dt <- rbindlist(
  list(all_dt, all_weighted),
  use.names = TRUE,
  fill = TRUE
)

final_dt<-all_dt
final_dt<-final_dt[,c(1:3,5:10)]
setnames(final_dt, c("year","nuts","municipality","type","laucode","nuts2016","number","median_ppm2","ppm2"))


final_dt[, number := as.numeric(number)]

final_long <- melt(
  final_dt,
  id.vars = c(
    "municipality",
    "year",
    "type",
    "nuts",
    "nuts2016",
    "laucode"
  ),
  measure.vars = c(
    "number",
    "ppm2",
    "median_ppm2"
  ),
  variable.name = "indicator",
  value.name = "value"
)

final_long$value<-as.numeric(final_long$value)

final_long$value<-round(final_long$value,0)

dd<-final_long
aa<-dd[, .(n_unique_vald = uniqueN(municipality)), by = .(year, type)]

missing_ppm2_by_year <- final_long[
  indicator == "ppm2" & is.na(value),
  .(n_unique_vald_missing = uniqueN(municipality)),
  by = year
][order(year)]

# Save to CSV in the usual output folder
# Using fwrite (fast, from data.table)
write.csv(
  final_long,
  file = file.path(output_path, "EE_transaction_price_clean.csv"),
  row.names = FALSE
)




# ==================================================
# Time series of PPM2 by LAU code (clustered plots)
# ==================================================

# Keep only price per m2
dt_ppm2 <- final_long[
  indicator == "ppm2" & type == "flat"
]

# Ensure numeric + ordered
dt_ppm2[, year := as.integer(year)]
dt_ppm2[, value := as.numeric(value)]

# List of LAU codes
lau_list <- sort(unique(dt_ppm2$laucode))

# Split into clusters of 12
lau_groups <- split(
  lau_list,
  ceiling(seq_along(lau_list) / 12)
)

# ==================================================
# Plot each cluster
# ==================================================
out_dir <- file.path(out_check, "transactions")

for (i in seq_along(lau_groups)) {
  
  lau_subset <- lau_groups[[i]]
  
  dt_plot <- dt_ppm2[laucode %in% lau_subset]
  
  p <- ggplot(
    dt_plot,
    aes(x = year, y = value, group = laucode)
  ) +
    geom_line(color = "steelblue", linewidth = 0.8) +
    geom_point(size = 1.6) +
    facet_wrap(~ laucode, scales = "free_y", ncol = 4) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(
      title = paste("Flat price per m² – LAU group", i),
      x = "Year",
      y = "Price per m²"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      strip.text = element_text(size = 9),
      plot.title = element_text(face = "bold")
    )
  
  ggsave(
    filename = file.path(
      out_dir,
      paste0("flat_ppm2_LAU_group_", i, ".png")
    ),
    plot = p,
    width = 12,
    height = 8,
    dpi = 300
  )
}


hpi   <- fread(file.path(source_prices_path, "EE_hpi.csv"),           encoding = "UTF-8")


# # Reshape to long format
# hpi_long <- melt(
#   hpi,
#   id.vars = "year",
#   variable.name = "type",
#   value.name = "index"
# )
#
# # Nice labels
# hpi_long[, type := fifelse(
#   type == "apartments_index", "Apartments",
#   "Houses"
# )]
#
# # Plot
# ggplot(hpi_long, aes(x = year, y = index, color = type)) +
#   geom_line(linewidth = 1.1) +
#   geom_point(size = 2) +
#   labs(
#     title = "House Price Indices in Estonia",
#     subtitle = "Apartments vs Houses",
#     x = "Year",
#     y = "HPI",
#     color = ""
#   ) +
#   theme_minimal(base_size = 12) +
#   theme(
#     plot.title = element_text(face = "bold"),
#     legend.position = "top"
#   )


# ==================================================
# 0. INPUTS REQUIRED (must already exist)
# --------------------------------------------------
# final_long : LAU-level long dataset
# hpi        : national HPI dataset with apartments_index
# lau_groups : list of LAU clusters
# base_path  : project base path
# ==================================================

# --------------------------------------------------
# 1. Keep LAU-level flat price per m²
# --------------------------------------------------
lau_ppm2 <- final_long[
  indicator == "ppm2" & type == "flat"
]

lau_ppm2[, year := as.integer(year)]
lau_ppm2[, value := as.numeric(value)]

# --------------------------------------------------
# 2. Choose base year
# --------------------------------------------------
base_year <- 2010

# --------------------------------------------------
# 3. Prepare national HPI (apartments)
# --------------------------------------------------
hpi_use <- hpi[, .(year, apartments_index)]
hpi_use[, year := as.integer(year)]
setorder(hpi_use, year)

# National HPI log growth
hpi_use[
  ,
  d_log_hpi := log(apartments_index) - log(shift(apartments_index))
]

# --------------------------------------------------
# 4. Merge HPI growth into LAU data
# --------------------------------------------------
lau_ppm2 <- merge(
  lau_ppm2,
  hpi_use[, .(year, d_log_hpi)],
  by = "year",
  all.x = TRUE
)

# --------------------------------------------------
# 5. Compute LAU-level log growth
# --------------------------------------------------
setorder(lau_ppm2, laucode, year)

lau_ppm2[
  ,
  d_log_lau := log(value) - log(shift(value)),
  by = laucode
]


# ==================================================
# PARAMETERS
# ==================================================
base_year <- 2010
alpha     <- 0.7   # 1 = no smoothing, <1 = smoother

out_dir <- file.path(
  base_path,
  "EE",
  "outputdata",
  "checks_graphs",
  "transactions"
)
stopifnot(dir.exists(out_dir))

# ==================================================
# PREPARE NATIONAL HPI (ALL VARIANTS)
# ==================================================
hpi_use <- copy(hpi)
hpi_use[, year := as.integer(year)]

hpi_use[, hpi_flat  := apartments_index]
hpi_use[, hpi_house := houses_index]
hpi_use[, hpi_all   := rowMeans(.SD, na.rm = TRUE),
        .SDcols = c("apartments_index", "houses_index")]

setorder(hpi_use, year)

# log growths
hpi_use[, d_log_flat  := log(hpi_flat)  - log(shift(hpi_flat))]
hpi_use[, d_log_house := log(hpi_house) - log(shift(hpi_house))]
hpi_use[, d_log_all   := log(hpi_all)   - log(shift(hpi_all))]

# ==================================================
# FUNCTION: smooth one housing type
# ==================================================
smooth_lau_prices <- function(type_use, hpi_growth_col, label) {
  
  cat("\n--- Processing:", label, "---\n")
  
  dt <- final_long[
    indicator == "ppm2" & type == type_use
  ]
  
  dt[, year := as.integer(year)]
  dt[, value := as.numeric(value)]
  
  # merge HPI growth
  dt <- merge(
    dt,
    hpi_use[, .(year, d_log_hpi = get(hpi_growth_col))],
    by = "year",
    all.x = TRUE
  )
  
  setorder(dt, laucode, year)
  
  # local growth
  dt[, d_log_lau := log(value) - log(shift(value)), by = laucode]
  
  # smoothed growth
  dt[, d_log_smooth := alpha * d_log_lau + (1 - alpha) * d_log_hpi]
  
  # reconstruct levels (anchor at base year)
  dt[, ppm2_smoothed := {
    
    yrs  <- year
    g    <- d_log_smooth
    base <- value[year == base_year][1]
    
    if (is.na(base)) {
      rep(NA_real_, .N)
    } else {
      t0 <- which(yrs == base_year)
      out <- numeric(.N)
      out[t0] <- base
      
      if (t0 < .N)
        for (t in (t0 + 1):.N)
          out[t] <- out[t - 1] * exp(g[t])
      
      if (t0 > 1)
        for (t in (t0 - 1):1)
          out[t] <- out[t + 1] / exp(g[t + 1])
      
      out
    }
  }, by = laucode]
  
  dt <- dt[!is.na(ppm2_smoothed)]
  
  # --------------------------------------------------
  # PLOTS (RAW vs SMOOTHED)
  # --------------------------------------------------
  for (i in seq_along(lau_groups)) {
    
    lau_subset <- lau_groups[[i]]
    
    dt_plot <- dt[
      laucode %in% lau_subset &
        !is.na(ppm2_smoothed)
    ]
    
    if (nrow(dt_plot) == 0) next
    
    p <- ggplot(dt_plot, aes(x = year, group = laucode)) +
      geom_line(aes(y = value, color = "Raw"),
                linewidth = 0.7, alpha = 0.6) +
      geom_line(aes(y = ppm2_smoothed, color = "Smoothed"),
                linewidth = 1.0) + scale_y_continuous(limits = c(0, NA))+
      facet_wrap(~ laucode, scales = "free_y", ncol = 4) +
      scale_color_manual(
        values = c("Raw" = "steelblue",
                   "Smoothed" = "firebrick")
      ) +
      labs(
        title = paste(label, "price per m² – Raw vs Smoothed (LAU group", i, ")"),
        x = "Year",
        y = "Price per m²",
        color = ""
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(face = "bold"),
        strip.text = element_text(size = 9),
        legend.position = "top"
      )
    
    ggsave(
      filename = file.path(
        out_dir,
        sprintf("%s_ppm2_raw_vs_smoothed_LAU_group_%02d.png",
                type_use, i)
      ),
      plot = p,
      width = 12,
      height = 8,
      dpi = 300
    )
  }
  
  # return clean output
  out <- dt[, .(
    laucode,
    year,
    nuts,
    indicator = "ppm2",
    value = round(ppm2_smoothed, 0),
    type = type_use
  )]
  
  return(out)
}

# ==================================================
# RUN FOR FLAT / HOUSE / ALL
# ==================================================
flat_smoothed  <- smooth_lau_prices("flat",  "d_log_flat",  "Flat")
house_smoothed <- smooth_lau_prices("house", "d_log_house", "House")
all_smoothed   <- smooth_lau_prices("all",   "d_log_all",   "All dwellings")

# ==================================================
# FINAL STACKED DATASET
# ==================================================
lau_ppm2_smoothed <- rbindlist(
  list(flat_smoothed, house_smoothed, all_smoothed),
  use.names = TRUE
)

# Optional save
fwrite(
  lau_ppm2_smoothed,
  file.path(output_path, "EE_LAU_ppm2_smoothed.csv")
)
