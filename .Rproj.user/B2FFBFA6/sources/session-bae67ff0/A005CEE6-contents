#---------------------------------------------------------
# SWEDEN Main
# This script calls all Sweden housing data scripts and runs them in order.
#---------------------------------------------------------
rm(list=ls())	
cc <- "SE"

# Base paths
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
website_path            <- file.path(ROOT, cc, "website", "maps_SE")


run_script <- function(script_path) {
  cat("▶ Running:", script_path, "\n")
  source(script_path, encoding = "UTF-8")
  cat("✔ Finished:", script_path, "\n\n")
}

# --- Call all sub-scripts for stock timeseries creation (add as needed) ---
run_script(file.path(rcode_path, "stocks", paste0(cc, "_dwelling_13_24.R")))
run_script(file.path(rcode_path, "stocks", paste0(cc, "_rooms_13_24.R")))
run_script(file.path(rcode_path, "stocks", paste0(cc, "_dwelling_age_13_24.R")))
run_script(file.path(rcode_path, "stocks", paste0(cc, "_dwelling_area_13_24.R"))) ##equal to _dwelling_13_24#####
run_script(file.path(rcode_path, "stocks", paste0(cc, "_floor_00_20.R")))
run_script(file.path(rcode_path, "income", paste0(cc, "_buildings_seasonal.R")))

# --- Call all sub-scripts for income timeseries creation (add as needed) ---
run_script(file.path(rcode_path, "income", paste0(cc, "_income.R")))


# --- Call all sub-scripts for price timeseries creation (add as needed) ---
run_script(file.path(rcode_path, "prices", paste0(cc, "_loadhpi.R")))
run_script(file.path(rcode_path, "prices", paste0(cc, "_rent.R")))

cat("✅ All Sweden housing scripts executed successfully.\n")



########Regional file on stocks statistics##############
#Load and clean dwelling data from from statistical office -- SE_dwell_clean(dwelling)###
run_script(file.path(rcode_path, "timeseries_stock", paste0(cc, "_timeseries_dwelling_clean.R")))

#Load and clean dwelling data from from statistical office -- SE_m2_clean (totalm2)###
run_script(file.path(rcode_path, "timeseries_stock", paste0(cc, "_timeseries_totalm2_clean.R")))

#Load and clean dwelling data from from statistical office -- SE_m2_mean###
run_script(file.path(rcode_path, "timeseries_stock", paste0(cc, "_timeseries_area_sizeclass_clean.R")))


#Load and clean room data from from statistical office -- SE_room_clean###
run_script(file.path(rcode_path, "timeseries_stock", paste0(cc, "_timeseries_room_clean.R")))

#Load and clean room data from from statistical office -- SE_room_clean###
run_script(file.path(rcode_path, "timeseries_stock", paste0(cc, "_timeseries_dwelling_tenure_clean.R")))


#Load and clean room data from from statistical office -- SE_dwell_age###
run_script(file.path(rcode_path, "timeseries_stock", paste0(cc, "_timeseries_dwelling_age_clean.R")))


#Load and clean room data from from statistical office -- SE_dwell_age###
run_script(file.path(rcode_path, "timeseries_stock", paste0(cc, "_timeseries_nbdwelling_size_class_clean.R")))


#Load and clean room data from from statistical office -- SE_dwell_age###
run_script(file.path(rcode_path, "timeseries_stock", paste0(cc, "_timeseries_area_averagesm2_clean.R")))



########Regional file on price statistics##############
#Load and clean price data from webscraped file (mean and median)-- SE_price_clean###
run_script(file.path(rcode_path, "timeseries_price", paste0(cc, "_price_timeseries_houses.R")))

#Load and clean price data from webscraped file (mean and median)-- SE_price_clean###
run_script(file.path(rcode_path, "timeseries_price", paste0(cc, "_price_timeseries_flat_all.R")))


#Load and clean price data from webscraped file (mean and median)-- SE_price_clean###
run_script(file.path(rcode_path, "timeseries_price", paste0(cc, "_price_timeseries_rents.R")))

#Load and clean price data from webscraped file (mean and median)-- SE_price_clean###
run_script(file.path(rcode_path, "timeseries_price", paste0(cc, "_transprice_clean.R")))

#merge everything together and create the final regional file -- SE_offer_price_clean###
run_script(file.path(rcode_path, "timeseries_price", paste0(cc, "_offerprice_clean.R")))

## Load and clean webscraped data (...ads_nuts.csv)
##run_script(file.path(rcode_path, "timeseries_price", paste0(cc, "_micro.R")))



########Regional file on income statistics##############
run_script(file.path(rcode_path, "timeseries_income", paste0(cc, "_timeseries_income_clean.R")))






# ===============================
# 1. Load CSV files
# ===============================
dwell               <- fread(file.path(output_path, "SE_dwell_clean.csv"))
dwell$indicator<-"dwl"

m2_clean            <- fread(file.path(output_path, "SE_m2_clean.csv"))
setnames(m2_clean   , "measure", "indicator")  # rename column

room_clean          <- fread(file.path(output_path, "SE_room_clean.csv"))
setnames(room_clean    , "measure", "indicator")  # rename column
room_clean$indicator<-"rooms"

dwell_tenure_clean  <- fread(file.path(output_path, "SE_dwell_tenure.csv"))
setnames(dwell_tenure_clean    , "measure", "indicator")  # rename column

dwell_age_clean     <- fread(file.path(output_path, "SE_dwell_age.csv"))
setnames(dwell_age_clean    , "measure", "indicator")  # rename column


income              <- fread(file.path(output_path, "SE_income_clean.csv"))
setnames(income, "sex", "type.of.building")  # rename columnù
income$indicator <- paste(income$measure, income$type.of.building, sep = "_")
income$type.of.building    <- "NA"
income <- income %>% dplyr::select(-measure)


offer_price         <- fread(file.path(output_path, "SE_offer_price_clean.csv"))
setnames(offer_price     , "code", "nuts")  # rename column
setnames(offer_price, old = "measure",     new = "indicator")


trans_price <- fread(file.path(output_path, "SE_prices_clean.csv")) 
setnames(trans_price, old = "measure",     new = "indicator")
trans_price$indicator<-"ppm2"


m2_mean             <- fread(file.path(output_path, "SE_m2_avg_all_levels.csv"))
m2_mean[, nuts := as.character(nuts)]
m2_mean$indicator <- "mean_m2"

rent<- fread(file.path(output_path, "SE_rent_clean.csv"))
setnames(rent, old = "laucode",     new = "nuts")
rent$indicator<-"rent"
rent$type.of.building<-"all"
# ===============================
# 7. Combine all datasets
# ===============================

combined <- rbindlist(
  list(
    trans_price,
    m2_clean,
    m2_mean,
    room_clean,
    dwell_tenure_clean,
    dwell_age_clean,
    dwell,
    offer_price,
    income, 
    rent
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
  grepl("^[1-8]", combined$nuts) & nchar(combined$nuts) == 3,
  paste0("0", combined$nuts),
  combined$nuts
)

# If level == "laucode", add "SE_" before the nuts column
combined <- combined %>%
  mutate(
    nuts = ifelse(level == "laucode", paste0("SE_", nuts), nuts)
  )


colnames(combined) <- c("year", "level", "nuts", "type", "value", "indicator")


setDT(combined)
# 1. Move numeric data from `level` to `value`
combined$value<-as.numeric(combined$value)

combined[, (setdiff(names(combined), "value")) := lapply(.SD, as.character),
         .SDcols = setdiff(names(combined), "value")]

fwrite(combined, file.path(output_end_path, "SE_regio.csv"))





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
