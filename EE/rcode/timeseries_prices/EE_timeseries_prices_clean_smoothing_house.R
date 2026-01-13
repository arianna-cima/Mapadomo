#==================================================
# 0. Load data
#==================================================

house <- fread(file.path(source_prices_path, "EE_house_existing.csv"),encoding = "UTF-8")
hpi <- fread(file.path(source_prices_path, "EE_hpi.csv"),encoding = "UTF-8")

setDT(house)
setDT(hpi)

#==================================================
# 1. LOCAL HOUSE INDEX (LAU, base = 2024)
#==================================================

setorder(house, `LAU CODE`, year)
house[, price_m2 := as.numeric(price_m2)]

house[,local_base_price := price_m2[year == 2024 & !is.na(price_m2)][1],by = `LAU CODE`]

house[,local_index := fifelse(  !is.na(price_m2) & !is.na(local_base_price),  price_m2 / local_base_price * 100,  NA_real_ )]

#==================================================
# 2. NATIONAL HOUSE HPI (base = 2024)
#==================================================

hpi[, houses_index := as.numeric(houses_index)]

hpi[,nat_index := houses_index /  houses_index[year == 2024] * 100]

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
# 4. CONDITIONAL NATIONAL SMOOTHING
#==================================================

threshold <- 0.05   # 5% tolerance
lambda    <- 0.7    # strong pull if smoothing applies

index[,deviation := abs(local_index - nat_index) / nat_index]

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
# 5. MERGE FINAL INDEX BACK TO HOUSE DATA  ✅ FIXED
#==================================================

house <- merge(
  house,
  index,
  by = c("LAU CODE", "year"),
  all.x = TRUE
)

#==================================================
# 6. RECONSTRUCT SMOOTHED HOUSE PRICES
#==================================================

house[
  ,
  price_m2_smooth := fifelse(
    !is.na(final_index) & !is.na(local_base_price),
    local_base_price * (final_index / 100),
    price_m2
  )
]




#==================================================
# 7. PLOTTING (RAW vs SMOOTHED)
#==================================================
out_dir <- file.path(out_check, "offer")


lau_list   <- sort(unique(house$`LAU CODE`))
lau_groups <- split(lau_list, ceiling(seq_along(lau_list) / 12))

for (i in seq_along(lau_groups)) {
  
  lau_subset <- lau_groups[[i]]
  dt_plot <- house[`LAU CODE` %in% lau_subset]
  
  p <- ggplot(dt_plot, aes(x = year)) +
    geom_line(aes(y = price_m2), color = "grey70", linewidth = 0.6) +
    geom_line(aes(y = price_m2_smooth), color = "steelblue", linewidth = 1) +
    facet_wrap(~ `LAU CODE`, scales = "free_y", ncol = 4) +
    labs(
      title = paste(
        "House price per m² – raw vs nationally anchored",
        paste(lau_subset, collapse = ", ")
      ),
      x = "Year",
      y = "Price per m²"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      strip.text = element_text(size = 9),
      plot.title = element_text(face = "bold")
    )
  
  ggsave(
    filename = file.path(out_dir, paste0("house_prices_LAU_group_", i, ".png")),
    plot = p,
    width = 12,
    height = 8,
    dpi = 300
  )
}



house <- house[, c(1:2, 4:5, 7, 9, 10)]
colnames(house) <- c("nuts", "year", "ppm2_offer", "number_transactions", "type", "nuts2021", "nuts2016")
house_long <- melt(house, id.vars = c("nuts", "year", "type", "nuts2021", "nuts2016"), measure.vars = c("ppm2_offer", "number_transactions"))


write.csv(
  house_long ,
  file = file.path(output_path, "EE_house_offer_price_clean.csv"),
  row.names = FALSE
)
