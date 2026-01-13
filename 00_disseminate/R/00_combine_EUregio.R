
rm(list = ls()); gc()
ROOT <- getwd()

# 
# cc <- c(
#   "AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR",
#   "DE","GR","HU","IE","IT","LV","LT","LU","MT","NL",
#   "PL","PT","RO","SK","SI","ES","SE"
# )

# List of EU country codes (ISO2)
# "ES","SI","SE","FR",
cc <- c(
 "EE","SE"
)

cpaths <- file.path(
  ROOT,
  cc,
  "outputdata",
  "final",
  paste0(cc, "_regio.csv")
)
names(cpaths) <- cc

# Keep only existing files
cexist <- cpaths[file.exists(cpaths)]
message("Reading in: ", paste(names(cexist), collapse = ", "))

# --------------------------------------------------
# Read and combine data
# --------------------------------------------------
data_list <- lapply(cexist, data.table::fread)

combined_data <- data.table::rbindlist(
  data_list,
  use.names = TRUE,
  fill = TRUE
)

# --------------------------------------------------
# Basic cleaning
# --------------------------------------------------
combined_data[, value := as.numeric(value)]
combined_data[, value := round(value, 0)]

combined_data[indicator == "pricem2", indicator := "ppm2"]

# --------------------------------------------------
# Save EU-level combined file
# --------------------------------------------------
out_file <- file.path(
  ROOT,
  "00_disseminate",
  "EU_regio.csv"
)

dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)

data.table::fwrite(combined_data, out_file)

# --------------------------------------------------
# Filter for specific analysis
# --------------------------------------------------
combined_data <- combined_data[
  level == "laucode" &
    indicator == "ppm2" &
    year == 2021 &
    type == "all" &
    !is.na(value)
]