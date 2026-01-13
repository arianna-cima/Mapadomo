library(data.table)
library(zoo)

# ============================
# 0. Load data
# ============================

flat <- fread(
  file.path(source_prices_path, "EE_price_flat_2003_2026_v1.csv"),
  encoding = "UTF-8"
)

setDT(flat)
flat[, year := as.integer(year)]

# Restrict years
flat <- flat[year %in% 2003:2025]

# ============================
# 1. Balanced panel
# ============================

all_years <- sort(unique(flat$year))

valds <- unique(
  flat[, .(
    vald_clean,
    vald,
    `LAU CODE`,
    `NUTS 3 CODE 2021`,
    `NUTS 3 CODE 2016`,
    type
  )]
)

panel <- CJ(year = all_years, vald_clean = valds$vald_clean)
panel <- merge(panel, valds, by = "vald_clean", all.x = TRUE)

panel <- merge(
  panel,
  flat,
  by = c(
    "vald_clean", "vald", "year", "type",
    "LAU CODE", "NUTS 3 CODE 2021", "NUTS 3 CODE 2016"
  ),
  all.x = TRUE
)

setorder(panel, vald_clean, year)

# ============================
# 2. NUTS3 YEAR LEVELS
# ============================

nuts3_level <- panel[
  !is.na(median_price_m2),
  .(
    nuts3_median = median(median_price_m2, na.rm = TRUE),
    nuts3_average = mean(average_price_m2, na.rm = TRUE)
  ),
  by = .(`NUTS 3 CODE 2021`, year)
]

panel <- merge(
  panel,
  nuts3_level,
  by = c("NUTS 3 CODE 2021", "year"),
  all.x = TRUE
)

# ============================
# 3. NUTS3 GROWTH RATES
# ============================

setorder(nuts3_level, `NUTS 3 CODE 2021`, year)

nuts3_level[, `:=`(
  g_median  = nuts3_median  / shift(nuts3_median),
  g_average = nuts3_average / shift(nuts3_average)
), by = `NUTS 3 CODE 2021`]

panel <- merge(
  panel,
  nuts3_level[, .(
    `NUTS 3 CODE 2021`, year, g_median, g_average
  )],
  by = c("NUTS 3 CODE 2021", "year"),
  all.x = TRUE
)

# ============================
# 4. Within-municipality interpolation
# ============================

panel[, median_interp :=
        zoo::na.approx(median_price_m2, x = year, na.rm = FALSE),
      by = vald_clean]

panel[, average_interp :=
        zoo::na.approx(average_price_m2, x = year, na.rm = FALSE),
      by = vald_clean]

# ============================
# 5. Initialize final variables
# ============================

panel[, `:=`(
  median_price_final  = median_price_m2,
  average_price_final = average_price_m2
)]

panel[is.na(median_price_final),
      median_price_final := median_interp]

panel[is.na(average_price_final),
      average_price_final := average_interp]

# ============================
# 6. NUTS3 GROWTH PROPAGATION
# ============================

panel[, median_price_final :=
        {
          p <- median_price_final
          g <- g_median
          for (i in seq_along(p)) {
            if (is.na(p[i]) && i > 1 && !is.na(p[i-1]) && !is.na(g[i])) {
              p[i] <- p[i-1] * g[i]
            }
          }
          for (i in rev(seq_along(p))) {
            if (is.na(p[i]) && i < length(p) && !is.na(p[i+1]) && !is.na(g[i+1])) {
              p[i] <- p[i+1] / g[i+1]
            }
          }
          p
        },
      by = vald_clean]

panel[, average_price_final :=
        {
          p <- average_price_final
          g <- g_average
          for (i in seq_along(p)) {
            if (is.na(p[i]) && i > 1 && !is.na(p[i-1]) && !is.na(g[i])) {
              p[i] <- p[i-1] * g[i]
            }
          }
          for (i in rev(seq_along(p))) {
            if (is.na(p[i]) && i < length(p) && !is.na(p[i+1]) && !is.na(g[i+1])) {
              p[i] <- p[i+1] / g[i+1]
            }
          }
          p
        },
      by = vald_clean]

# ============================
# 7. FINAL FALLBACK: NUTS3 LEVEL
# ============================

panel[is.na(median_price_final),
      median_price_final := nuts3_median]

panel[is.na(average_price_final),
      average_price_final := nuts3_average]

# ============================
# 8. NATIONAL FALLBACK (SAFETY)
# ============================

national_level <- panel[
  !is.na(median_price_m2),
  .(
    nat_median  = median(median_price_m2, na.rm = TRUE),
    nat_average = mean(average_price_m2, na.rm = TRUE)
  ),
  by = year
]

panel <- merge(panel, national_level, by = "year", all.x = TRUE)

panel[is.na(median_price_final),
      median_price_final := nat_median]

panel[is.na(average_price_final),
      average_price_final := nat_average]

# ============================
# 9. Diagnostics
# ============================

cat("Missing median_price_final:",
    panel[, sum(is.na(median_price_final))], "\n")

cat("Missing average_price_final:",
    panel[, sum(is.na(average_price_final))], "\n")

# panel is now complete and ready


panel<-panel[,c(1:8,17:18)]


# Save to CSV in the usual output folder
# Using fwrite (fast, from data.table)
write.csv(
  panel,
  file = file.path(source_prices_path, "EE_price_flat_2003_2026.csv"),
  row.names = FALSE
)

View(panel[is.na(average_price_final),])

unique_vald_by_year <- panel[
  , .(n_unique_vald = uniqueN(vald_clean)),
  by = year
][order(year)]

