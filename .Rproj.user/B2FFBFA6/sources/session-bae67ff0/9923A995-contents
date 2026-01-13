
# -----------------------------
# 1) Build shares from census dwelling data (2000/2011/2021)
# -----------------------------
DT <- fread(file.path(output_path, "EE_dwelling_2000_11_21.csv"))
DT[, indicator := "dwelling"]
DT <- DT[type %in% c("all","house","flat")]
setnames(DT, c("municipality","year","type","value","nuts2021","nuts2016","laucode","indicator"))

DT_wide <- dcast(DT, laucode + year ~ type, value.var = "value")
DT_wide <- DT_wide[!is.na(all) & all > 0]
DT_wide[, `:=`(share_house = house / all, share_flat = flat / all)]
DT_shares <- DT_wide[, .(laucode, census_year = year, share_house, share_flat)]

# -----------------------------
# 2) Load stock data (2003–2007) with only "all"
# -----------------------------
df_muni <- fread(file.path(output_path, "EE_stock_2003_07.csv"))
df_muni <- df_muni[indicator == "dwelling"]
df_muni[, type := "all"]
setnames(df_muni, "LAU CODE", "laucode")
setDT(df_muni)
df_muni[tolower(lau_name) == "vasalemma", laucode := 431]

# -----------------------------
# 3) Assign nearest census-year shares (no merge on year)
# -----------------------------
census_years <- sort(unique(DT_shares$census_year))
df_muni[, census_year := census_years[which.min(abs(census_years - year))], by = .(laucode, year)]

df_muni <- merge(df_muni, DT_shares, by = c("laucode","census_year"), all.x = TRUE)

# -----------------------------
# 4) Impute house & flat using those shares, then stack types
# -----------------------------
df_house <- df_muni[!is.na(share_house),
                    .(lau_name, municipality, year, indicator,
                      value = value * share_house,
                      `NUTS 3 CODE 2021`, `NUTS 3 CODE 2016`, laucode,
                      type = "house")
]

df_flat <- df_muni[!is.na(share_flat),
                   .(lau_name, municipality, year, indicator,
                     value = value * share_flat,
                     `NUTS 3 CODE 2021`, `NUTS 3 CODE 2016`, laucode,
                     type = "flat")
]

df_muni_final <- rbindlist(list(df_muni[, .(lau_name, municipality, year, indicator, value,
                                            `NUTS 3 CODE 2021`, `NUTS 3 CODE 2016`, laucode, type)],
                                df_house, df_flat), use.names = TRUE, fill = TRUE)




# Save to CSV in the usual output folder
# Using fwrite (fast, from data.table)
write.csv(
  df_muni_final,
  file = file.path(output_path, "EE_stock_flathouse_2003_2007.csv"),
  row.names = FALSE
)
