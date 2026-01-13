#--------------------------------------------------
# 1. Read the Excel file
#--------------------------------------------------

path <- "C:/Users/annai/Documents/backup_computer_ec/Contratti_lavoro/EU_Temporary_agent/materiali_di_lavoro/mapadomo/Mapadomo_v2/EE/sourcedata/prices/hpi_stat.xlsx"

hpi_q <- read_excel(
  path,
  sheet = 1,
  col_names = c("year", "quarter", "total", "apartments", "houses")
)

setDT(hpi_q)

# Drop empty rows (if any)
hpi_q <- hpi_q[!is.na(year)]

# Ensure correct types
hpi_q[, year := as.integer(year)]
hpi_q <- hpi_q[!is.na(year)]
#--------------------------------------------------
# 2. Compute ANNUAL indices (mean of quarters)
#--------------------------------------------------
hpi_q[
  ,
  `:=`(
    total       = as.numeric(total),
    apartments  = as.numeric(apartments),
    houses      = as.numeric(houses)
  )
]
hpi_annual <- hpi_q[
  ,
  .(
    apartments_index = mean(apartments, na.rm = TRUE),
    houses_index     = mean(houses, na.rm = TRUE)
  ),
  by = year
]

#--------------------------------------------------
# 3. (Optional) Order and inspect
#--------------------------------------------------

setorder(hpi_annual, year)
print(hpi_annual)


write.csv(
  hpi_annual,
  file = file.path(source_prices_path, "EE_hpi.csv"),
  row.names = FALSE
)