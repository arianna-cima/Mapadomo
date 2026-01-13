
# Save to CSV in the usual output folder
# Using fwrite (fast, from data.table)
DT <- data.table::fread(file.path(source_prices_path, "transaction_prices/EE_transactionvalue_2001_2007.csv"))


DT_clean <- DT[
  `Type of purchase-sale contract` ==
    "..purchase-sale contracts of ownership of dwellings" &
    Indicator == "Value of contracts, thousand euros" & Quarter=="Ascending total"
][
  , Quarter := NULL
]

DT_clean$value<-DT_clean$value*1000


# Save to CSV in the usual output folder
# Using fwrite (fast, from data.table)
DT_2 <- data.table::fread(file.path(source_prices_path, "transaction_prices/EE_transactionvalue_2007_2017.csv"))

DT_clean_2 <- DT_2[
  `Object` ==
    "..appartments sold as dwellings" &
    Indicator == "Value of transactions, euros" & Quarter=="1st-4th quarters"
][
  , Quarter := NULL
]

colnames(DT_clean_2)<-c("Year","type","County","indicator","value")
colnames(DT_clean)<-c("Year","County","type","indicator","value")
DT_clean_2$value<-as.numeric(DT_clean_2$value)
DT_clean$value<-as.numeric(DT_clean$value)

DT_clean<-rbind(DT_clean_2, DT_clean)
DT_clean <- DT_clean[grepl("county", County, ignore.case = TRUE)]

DT_clean[, base_2003 := sum(value[Year == 2003], na.rm = TRUE), by = County]


# 5. Construct HPI (base = 2003)
DT_clean[
  , HPI_2003 := (value / base_2003) * 100
]




# 6. Save to CSV
fwrite(
  DT_clean,
  file = file.path(
    source_prices_path,
    "transaction_prices/EE_transactionvalue_2001_2007.csv"
  )
)




# ---------------------------------------------------
# 2. Create county clusters (6 per page)
# ---------------------------------------------------
county_list <- sort(unique(DT_clean$County))

county_groups <- split(
  county_list,
  ceiling(seq_along(county_list) / 6)
)

# ---------------------------------------------------
# 3. Output directory
# ---------------------------------------------------
out_dir <- file.path(
  base_path,
  "EE/outputdata/graphs_checks/county_hpi"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------
# 4. Plot each cluster
# ---------------------------------------------------
for (i in seq_along(county_groups)) {
  
  dt_i <- DT_clean[County %in% county_groups[[i]]]
  
  p <- ggplot(
    dt_i,
    aes(x = Year, y = HPI_2003, group = County)
  ) +
    geom_line(color = "steelblue", linewidth = 0.9) +
    geom_point(size = 2) +
    facet_wrap(~ County, scales = "free_y", ncol = 3) +
    labs(
      title = paste("County HPI (2003 = 100) – Group", i),
      x = "Year",
      y = "HPI (2003 = 100)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      strip.text = element_text(size = 9),
      plot.title = element_text(face = "bold")
    )
  
  ggsave(
    filename = file.path(
      out_dir,
      paste0("county_HPI_group_", i, ".png")
    ),
    plot = p,
    width = 12,
    height = 8,
    dpi = 300
  )
}