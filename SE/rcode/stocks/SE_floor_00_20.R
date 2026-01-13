#https://www.statistikdatabasen.scb.se/pxweb/en/ssd/START__MI__MI0810__MI0810B/BostLokiTatortReg/table/tableViewLayout1/
#comma delimited wiht heading

#Residential and non-residential floor space within localities, by region by region, type of property, observations and every fifth year

dd<-fread(file.path(source_stock_path, "000001RM_20251124-105521.csv"), skip=2, header=TRUE)
melted_dt <- melt(
  dd,
  id.vars = c("region", "type of property"),
  variable.name = "variable",
  value.name = "value"
)
setDT(melted_dt)
melted_dt[, year := as.numeric(sub(".*(\\d{4})$", "\\1", variable))]

melted_dt[, variable := sub("\\s*\\d{4}$", "", variable) ]

melted_dt$value[melted_dt$value == ".."] <- NA

melted_dt[grepl("^Residential floor space", variable),
          variable := "residential"]

melted_dt[grepl("^Non-residential floor space", variable),
          variable := "non_residential"]



# Save to CSV in the usual output folder
# Using fwrite (fast, from data.table)
write.csv(
  melted_dt,
  file = file.path(source_stock_path, "SE_floor_residential_00_24.csv"),
  row.names = FALSE
)




#check to see how many regions we have by year 
setDT(melted_dt)

result <- melted_dt[
  , .(unique_regions = uniqueN(region)),
  by = .(variable, `type of property`, year)
]

result

