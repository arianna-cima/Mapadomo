#---------------------------------------------------------
# Estonia Main
# This script calls all Estonian housing data scripts and runs them in order.
#---------------------------------------------------------
rm(list=ls())	
cc <- "EE"

ROOT <- getwd()

# Paths using the 'cc' variable
rcode_path <- file.path(ROOT, cc, "rcode")
source_stock_path <- file.path(ROOT, cc, "sourcedata", "stocks")
source_prices_path <- file.path(ROOT, cc, "sourcedata", "prices")
source_lau_path <- file.path(ROOT, cc, "sourcedata", "laucodes")
output_path <- file.path(ROOT, cc, "outputdata", "temp")
output_webscrape_path <- file.path(ROOT, cc, "outputdata", "webscraped")
output_end_path <- file.path(ROOT, cc, "outputdata", "final")
source_income_path <- file.path(ROOT, cc, "sourcedata", "income")
source_geo<-file.path(ROOT, "00_georeference")
out_check<-file.path(ROOT, cc, "outputdata", "checks_graphs")
website_path            <- file.path(ROOT, cc, "website", "maps_EE")
diss<-file.path(ROOT, "00_disseminate")

run_script <- function(script_path) {
  cat("▶ Running:", script_path, "\n")
  source(script_path, encoding = "UTF-8")
  cat("✔ Finished:", script_path, "\n\n")
}

# --------------------------------------------------
# Call all sub-scripts for stock time series creation
# --------------------------------------------------
run_script(file.path(rcode_path, "stocks", paste0(cc, "_stock_2003_2007.R")))
run_script(file.path(rcode_path, "stocks", paste0(cc, "_stock_2003_2007_type.R")))
run_script(file.path(rcode_path, "stocks", paste0(cc, "_stock_census_00_11_21.R")))
run_script(file.path(rcode_path, "stocks", paste0(cc, "_conventionalstock_area_00_11_21.R")))

# --------------------------------------------------
# Call all sub-scripts for income time series creation
# --------------------------------------------------
run_script(file.path(rcode_path, "income", paste0(cc, "_income_county_load.R")))
run_script(file.path(rcode_path, "income", paste0(cc, "_income_load.R")))

# --------------------------------------------------
# Call all sub-scripts for offer price time series
# --------------------------------------------------
run_script(file.path(rcode_path, "prices", paste0("offer_", cc, "_load_flat_all.R")))
run_script(file.path(rcode_path, "prices", paste0("offer_", cc, "_load_flat_existing.R")))
run_script(file.path(rcode_path, "prices", paste0("offer_", cc, "_load_house_all.R")))
run_script(file.path(rcode_path, "prices", paste0("offer_", cc, "_load_house_rent.R")))
run_script(file.path(rcode_path, "prices", paste0("offer_", cc, "_load_flat_rent.R")))
run_script(file.path(rcode_path, "prices", paste0("offer_", cc, "_share_all_existing.R")))

# --------------------------------------------------
# Call all sub-scripts for price indices & transactions
# --------------------------------------------------
run_script(file.path(rcode_path, "prices", paste0(cc, "_hpi.R")))
# run_script(file.path(rcode_path, "prices", paste0(cc, "_hpi_county.R")))  # not used (too volatile in 2007)
run_script(file.path(rcode_path, "prices", paste0("transaction_", cc, "_transaction_prices_flat_2003_2026.R")))
run_script(file.path(rcode_path, "prices", paste0("transaction_", cc, "_transaction_house_flat_2003_2026.R")))
run_script(file.path(rcode_path, "timeseries_prices", paste0(cc, "_timeseries_transaction_prices_clean.R")))


cat("All Estonia scripts executed successfully.\n")


########Regional file on stocks statistics##############
#Load and clean dwelling data from from statistical office -- SE_dwell_clean(dwelling)###
run_script(file.path(rcode_path, "timeseries_stock", paste0(cc, "_timeseries_dwelling_clean.R")))



########Regional file on price statistics##############
#Load and clean price data from webscraped file (mean and median)-- SE_price_clean###
run_script(file.path(rcode_path, "timeseries_price", paste0(cc, "_price_timeseries_houses.R")))
run_script(file.path(rcode_path, "timeseries_price", paste0(cc, "_timeseries_prices_clean_smoothing_house.R")))
run_script(file.path(rcode_path, "timeseries_price", paste0(cc, "_timeseries_prices_clean_smoothing_flat.R")))


########Regional file on income statistics##############
run_script(file.path(rcode_path, "timeseries_income", paste0(cc, "_timeseries_income_clean.R")))






# ===============================
# 1. Load CSV files
# ===============================
trans_price               <- fread(file.path(output_path, "EE_LAU_ppm2_clean.csv"))
setDT(trans_price)
setorder(trans_price, nuts, year, type, indicator, level, value)

dwell  <- fread(file.path(output_path, "EE_dwelling_timeseries_clean.csv"))

offer_flat <- fread(file.path(output_path, "EE_flat_offer_price_clean.csv"))
offer_flat$level<-"laucode"
offer_flat[, nuts2016 := NULL]
offer_flat[, nuts2021 := NULL]
setnames(offer_flat, "variable", "indicator")

offer_house <- fread(file.path(output_path, "EE_house_offer_price_clean.csv"))
offer_house$level<-"laucode"
offer_house[, nuts2016 := NULL]
offer_house[, nuts2021 := NULL]
setnames(offer_house, "variable", "indicator")



offer_all <- fread(file.path(output_path, "EE_all_offer_price_clean.csv"))
offer_all$level<-"laucode"
offer_all[, nuts2016 := NULL]
offer_all[, nuts2021 := NULL]
setnames(offer_all, "variable", "indicator")

totalm2<- fread(file.path(output_path, "EE_totalm2_timeseries_clean.csv"))
income<- fread(file.path(output_path, "EE_income_clean.csv"))
income$type<-"all"
# ===============================
# 7. Combine all datasets
# ===============================

combined <- rbindlist(
  list(
    trans_price,
    dwell, 
    offer_flat,
    offer_house, 
    offer_all, 
    totalm2, 
    income
    
  ),
  use.names = TRUE,
  fill = TRUE
)

# ===============================
# 8. Export final dataset
# ===============================

unique(combined$indicator)
combined$value <- round(combined$value, 0)

combined$nuts <- ifelse(
  grepl("^[1-9]", combined$nuts) & nchar(combined$nuts) == 3,
  paste0("0", combined$nuts),
  combined$nuts
)

# If level == "laucode", add "SE_" before the nuts column
combined <- combined %>%
  mutate(
    nuts = ifelse(level == "laucode", paste0("EE_", nuts), nuts)
  )


colnames(combined) <- c("nuts","year", "type","indicator", "value", "level")


setDT(combined)
# 1. Move numeric data from `level` to `value`
combined$value<-as.numeric(combined$value)

combined[, (setdiff(names(combined), "value")) := lapply(.SD, as.character),
         .SDcols = setdiff(names(combined), "value")]

fwrite(combined, file.path(output_end_path, "EE_regio.csv"))





combined_data <- combined %>%
  filter(level == "laucode", 
         indicator == "pricem2", 
         year=="2021",type=="all",
         !is.na(value))




##### updating and downloading data####
### data are taken from the swedish statistical office https://www.scb.se/en/###
##for dwelling https://www.statistikdatabasen.scb.se/pxweb/en/ssd/START__BO__BO0104__BO0104D/BO0104T04/
### for rooms https://www.statistikdatabasen.scb.se/pxweb/en/ssd/START__BO__BO0104__BO0104D/BO0104T09/
### for totalm2 https://www.statistikdatabasen.scb.se/pxweb/en/ssd/START__MI__MI0810__MI0810B/BostLokAreaTatort/#
### for size https://www.statistikdatabasen.scb.se/pxweb/en/ssd/START__BO__BO0104__BO0104D/BO0104T5/
###  for hpi https://stats.oecd.org/Index.aspx?DataSetCode=HOUSE_PRICES#
### for building https://www.statistikdatabasen.scb.se/pxweb/en/ssd/START__MI__MI0803__MI0803B/Bostadsbyggnad/
## for age https://www.statistikdatabasen.scb.se/pxweb/en/ssd/START__BO__BO0104__BO0104D/BO0104T02/
