
#==================================================
# 0. Load data
#==================================================

flat  <- fread(file.path(source_prices_path, "EE_flat_existing.csv"),  encoding = "UTF-8")
house <- fread(file.path(source_prices_path, "EE_flat_all.csv"), encoding = "UTF-8")

setDT(flat)
setDT(house)

#==================================================
# 1. HOUSE price index (base year = 2024 ONLY)
#==================================================

setorder(house, `LAU CODE`, year)

house[,house_base_price := price_m2[year == 2024 & !is.na(price_m2)][1],by = `LAU CODE`]

house[
  ,
  house_index := fifelse(
    !is.na(price_m2) & !is.na(house_base_price),
    price_m2 / house_base_price * 100,
    NA_real_
  )
]

index <- house[, .(`LAU CODE`, year, house_index)]

#==================================================
# 2. Create synthetic FLAT data for 2007–2022
#    (copied ONLY from 2024 flats)
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
# 3. rbind with original flat data (2023–2025 stay untouched)
#==================================================

flat_full <- rbindlist(
  list(flat, flat_2007_2022),
  use.names = TRUE
)

setorder(flat_full, `LAU CODE`, year)

#==================================================
# 4. Merge house index (SAFE join)
#==================================================

setkey(flat_full, `LAU CODE`, year)
setkey(index,     `LAU CODE`, year)

flat_full[index, house_index := i.house_index]

#==================================================
# 5. Adjust prices ONLY for 2007–2022
#==================================================

flat_full[, price_m2 := as.numeric(price_m2)]
flat_full[is.na(house_index), price_m2 := NA_real_]


flat_full[
  year <= 2022 &
    !is.na(price_m2) &
    !is.na(house_index),
  price_m2 := price_m2 * (house_index / 100)
]

#==================================================
# 6. Flag real vs adjusted prices
#==================================================

flat_full[,price_real := year %in% c(2023, 2024, 2025)]

# Final order
setorder(flat_full, `LAU CODE`, year)

setDT(flat_full)
setorder(flat_full, `LAU CODE`, year)



write.csv(
  flat_full,
  file = file.path(source_prices_path, "EE_flat_offer_price_clean.csv"),
  row.names = FALSE
)


#--------------------------------------------------
# 1. Output directory
#--------------------------------------------------

out_dir <- "C:/Users/annai/Documents/backup_computer_ec/Contratti_lavoro/EU_Temporary_agent/materiali_di_lavoro/mapadomo/Mapadomo_v2/EE/outputdata/checks_graphs/offer"

#dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

#--------------------------------------------------
# 2. Split LAU CODEs into clusters of 12
#--------------------------------------------------

lau_list <- sort(unique(flat_full$`LAU CODE`))
lau_groups <- split(lau_list, ceiling(seq_along(lau_list) / 12))

#--------------------------------------------------
# 3. Loop over clusters and plot
#--------------------------------------------------

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

#hpi <- fread(file.path(source_prices_path, "EE_hpi.csv"), encoding = "UTF-8")



