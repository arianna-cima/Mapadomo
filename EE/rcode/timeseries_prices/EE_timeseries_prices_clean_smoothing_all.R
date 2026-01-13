
flat  <- fread(file.path(output_path, "EE_flat_offer_price_clean.csv"), encoding = "UTF-8")
house <- fread(file.path(output_path, "EE_house_offer_price_clean.csv"),      encoding = "UTF-8")


dt <- rbindlist(list(flat, house), use.names = TRUE, fill = TRUE)

all_type <- dt[
  , .(
    value = if (variable == "ppm2_offer") mean(value, na.rm = TRUE)
    else sum(value, na.rm = TRUE)
  ),
  by = .(nuts, year, variable, nuts2021, nuts2016)
][, type := "all"]



write.csv(
  all_type ,
  file = file.path(output_path, "EE_all_offer_price_clean.csv"),
  row.names = FALSE
)
