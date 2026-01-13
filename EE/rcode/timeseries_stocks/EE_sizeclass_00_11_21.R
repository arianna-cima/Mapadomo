library(data.table)

# --------------------------------------------------
# Load census total m2 data (2000 / 2011 / 2021)
# --------------------------------------------------
DT <- fread(file.path(output_path, "EE_conventionalstock_byarea_2000_11_21.csv"))

# Set indicator
DT[, indicator := "totalm2"]

# Recode dwelling size into harmonised area groups
DT[, area_group := fcase(
  grepl("Less than 30|30-39|40-49|50-59", type_of_building), "<60sqm",
  grepl("60-69|70-79|80-99",             type_of_building), "61_100sqm",
  grepl("100-119|120-149",               type_of_building), "101_150sqm",
  grepl("150",                           type_of_building), ">150sqm",
  grepl("unknown",                       type_of_building), "data_missing",
  default = NA_character_
)]

# Drop total area row to avoid double counting
DT <- DT[type_of_building != "Area of dwelling total"]

# Create area-specific indicator
DT[, indicator := paste0("totalm2_", area_group)]

# Keep only required columns and rename LAU code
DT <- DT[, .(`LAU CODE`, year, `NUTS 3 CODE 2021`, `NUTS 3 CODE 2016`, indicator, value)]
setnames(DT, "LAU CODE", "laucode")

# Final dataset is already in long format
DT_final <- copy(DT)

# --------------------------------------------------
# Melt (correct usage)
# --------------------------------------------------
DT_long <- melt(
  DT,
  id.vars = c("laucode","year","NUTS 3 CODE 2021","NUTS 3 CODE 2016","indicator"),
  measure.vars = "value",
  variable.name = "variable",
  value.name = "value"
)

# Drop redundant variable column
DT_long[, variable := NULL]



# Save to CSV in the usual output folder
# Using fwrite (fast, from data.table)
write.csv(
  DT_long,
  file = file.path(output_path, "EE_sizeclass_00_11_21.csv"),
  row.names = FALSE
)