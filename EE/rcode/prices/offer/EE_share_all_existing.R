all <- fread(file.path(source_prices_path, "EE_flat_all.csv"), encoding = "UTF-8")
ext <- fread(file.path(source_prices_path, "EE_flat_existing.csv"), encoding = "UTF-8")

merged <- merge(
  ext,
  all,
  by = c("LAU CODE", "year"),
  all.x = TRUE
)
# Compute share
merged[, share := price_m2.x / price_m2.y]

# Keep only required columns
merged_final <- merged[, .(`LAU CODE`, year, share)]


avg_share <- merged_final[
  , .(avg_share = mean(share, na.rm = TRUE)),
  by = `LAU CODE`
]

overall_mean <- avg_share[!is.nan(avg_share), mean(avg_share, na.rm = TRUE)]

# Replace NaN with overall mean
avg_share[is.nan(avg_share), avg_share := overall_mean]


write.csv(
  avg_share,
  file = file.path(source_prices_path, "EE_share_all_existing.csv"),
  row.names = FALSE
)