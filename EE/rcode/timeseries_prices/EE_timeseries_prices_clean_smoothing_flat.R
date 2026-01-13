library(data.table)
library(ggplot2)

#==================================================
# 0. Load data
#==================================================

flat  <- fread(file.path(source_prices_path, "EE_flat_existing.csv"), encoding = "UTF-8")
house <- fread(file.path(source_prices_path, "EE_flat_all.csv"),      encoding = "UTF-8")
hpi   <- fread(file.path(source_prices_path, "EE_hpi.csv"),           encoding = "UTF-8")

setDT(flat)
setDT(house)
setDT(hpi)

#==================================================
# 1. LOCAL HOUSE INDEX (LAU, base = 2024)
#==================================================

setorder(house, `LAU CODE`, year)
house[, price_m2 := as.numeric(price_m2)]

house[
  ,
  local_base_price := price_m2[year == 2024 & !is.na(price_m2)][1],
  by = `LAU CODE`
]

house[
  ,
  local_index := fifelse(
    !is.na(price_m2) & !is.na(local_base_price),
    price_m2 / local_base_price * 100,
    NA_real_
  )
]

#==================================================
# 2. NATIONAL HPI (APARTMENTS, base = 2024)
#==================================================

hpi[, apartments_index := as.numeric(apartments_index)]

hpi[
  ,
  nat_index := apartments_index /
    apartments_index[year == 2024] * 100
]

nat_index <- hpi[, .(year, nat_index)]

#==================================================
# 3. MERGE LOCAL + NATIONAL INDICES
#==================================================

index <- merge(
  house[, .(`LAU CODE`, year, local_index)],
  nat_index,
  by = "year",
  all.x = TRUE
)

#==================================================
# 4. CONDITIONAL SMOOTHING (KEY STEP)
#==================================================

threshold <- 0.05   # 25% deviation allowed
lambda    <- 0.7    # smoothing intensity

index[
  ,
  deviation := abs(local_index - nat_index) / nat_index
]

index[
  ,
  final_index := fifelse(
    deviation <= threshold | is.na(deviation),
    local_index,
    (1 - lambda) * local_index + lambda * nat_index
  )
]

index <- index[, .(`LAU CODE`, year, final_index)]

#==================================================
# 5. CREATE SYNTHETIC FLAT DATA FOR 2007–2022
#==================================================

years_target <- 2007:2022

flat_2007_2022 <- rbindlist(
  lapply(years_target, function(y) {
    tmp <- copy(flat[year == 2024])
    tmp[, year := y]
    tmp
  })
)

#==================================================
# 6. RBIND WITH ORIGINAL FLAT DATA
#==================================================

flat_full <- rbindlist(
  list(flat, flat_2007_2022),
  use.names = TRUE
)

setorder(flat_full, `LAU CODE`, year)
flat_full[, price_m2 := as.numeric(price_m2)]

#==================================================
# 7. MERGE FINAL INDEX
#==================================================

setkey(flat_full, `LAU CODE`, year)
setkey(index,     `LAU CODE`, year)

flat_full[index, index_used := i.final_index]

#==================================================
# 8. IMPUTE FLAT PRICES (2007–2022)
#==================================================

flat_full[
  ,
  flat_price_2024 := price_m2[year == 2024 & !is.na(price_m2)][1],
  by = `LAU CODE`
]

flat_full[is.na(index_used), price_m2 := NA_real_]

flat_full[
  year <= 2022 &
    !is.na(flat_price_2024) &
    !is.na(index_used),
  price_m2 := flat_price_2024 * (index_used / 100)
]

#==================================================
# 9. FLAG REAL VS IMPUTED
#==================================================

flat_full[
  ,
  price_real := year %in% c(2023, 2024, 2025)
]

flat_full[, flat_price_2024 := NULL]
setorder(flat_full, `LAU CODE`, year)

#==================================================
# 10. PLOTTING (UNCHANGED)
#==================================================

out_dir <- "C:/Users/annai/Documents/backup_computer_ec/Contratti_lavoro/EU_Temporary_agent/materiali_di_lavoro/mapadomo/Mapadomo_v2/EE/outputdata/checks_graphs/offer/"

lau_list   <- sort(unique(flat_full$`LAU CODE`))
lau_groups <- split(lau_list, ceiling(seq_along(lau_list) / 12))

for (i in seq_along(lau_groups)) {
  
  lau_subset <- lau_groups[[i]]
  dt_plot <- flat_full[`LAU CODE` %in% lau_subset]
  
  p <- ggplot(dt_plot, aes(x = year, y = price_m2)) +
    geom_line(color = "steelblue", linewidth = 0.8) +
    geom_point(aes(shape = price_real), size = 1.8) +
    scale_y_continuous(limits = c(0, NA)) +
    facet_wrap(~ `LAU CODE`, scales = "free_y", ncol = 4) +
    labs(
      title = paste("Flat price per m² – LAU codes",
                    paste(lau_subset, collapse = ", ")),
      x = "Year",
      y = "Price per m²",
      shape = "Observed price"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      strip.text = element_text(size = 9),
      plot.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
  
  ggsave(
    filename = file.path(out_dir, paste0("flat_prices_LAU_group_", i, ".png")),
    plot = p,
    width = 12,
    height = 8,
    dpi = 300
  )
}





flat_full<- flat_full[, c(2:4, 6,8,9,10)]
colnames(flat_full) <- c("year", "ppm2_offer",  "number_transactions", "type", "nuts2021", "nuts2016","nuts")

flat_long <- melt(flat_full, id.vars = c("nuts", "year", "type", "nuts2021", "nuts2016"), measure.vars = c("ppm2_offer", "number_transactions"))


write.csv(
  flat_long ,
  file = file.path(output_path, "EE_flat_offer_price_clean.csv"),
  row.names = FALSE
)
