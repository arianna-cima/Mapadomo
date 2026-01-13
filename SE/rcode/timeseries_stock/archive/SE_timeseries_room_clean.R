# --- Load room data ---
stock <- fread(file.path(source_stock_path , "/SE_room_13_24.csv"), encoding = "UTF-8")


# --- Map building types to house/flat ---
type_map <- c(
  "one- or two-dwelling buildings" = "house",
  "multi-dwelling buildings"       = "flat",
  "other buildings"                = "other",
  "special housing"                = "special"
)
stock$type.of.building <- type_map[stock$type.of.building]

# --- Map and recode size.of.dwelling ---
type_map <- c(
  "dwellings without kitchen equipment"= "dwell_1room", 
  "1 room and kitchen"="dwell_2room", 
  "1 room and kitchenette"="dwell_2room",
  "2 rooms and kitchen"="dwell_3room", 
  "2 or more rooms with kitchenette"="dwell_3room", ##not sure about this
  "3 rooms and kitchen"="dwell_4room", 
  "4 rooms and kitchen"="dwell_5room", 
  "5 rooms and kitchen"="dwell_6room", 
  "6 or more rooms and kitchen"="dwell_7room", 
  "data missing"="noclass"
)

# Ensure the column is character
stock$size.of.dwelling <- as.character(stock$size.of.dwelling)

# Map values using the dictionary
stock$size.of.dwelling <- type_map[stock$size.of.dwelling]


# --- Number of rooms lookup ---
room_numbers <- c(
  "dwell_1room" = 1,
  "dwell_2room" = 2,
  "dwell_3room" = 3,
  "dwell_4room" = 4,
  "dwell_5room" = 5,
  "dwell_6room" = 6,
  "dwell_7room" = 7,
  "noclass" = 0
)

stock$room <- room_numbers[stock$size.of.dwelling]

# Create new variable 'room' with number of rooms
stock$value <- stock$value * stock$room

##################creating all type############
# Make sure stock is a data.table
stock <- as.data.table(stock)

# Sum over type.of.building to create type "all"
stock_all <- stock[, .(value = sum(value, na.rm = TRUE)),  # sum the values
                   by = .(year, observations, size.of.dwelling, region, room)]  # keep other grouping variables
stock_all[, type.of.building := "all"]  # set type.of.building = "all"

# Combine with the original stock
stock_combined <- rbindlist(list(stock, stock_all), fill = TRUE)

# Optional: reorder columns if you want
setcolorder(stock_combined, c("year", "observations", "size.of.dwelling", "type.of.building", "region", "value", "room"))


# --- Load LAU codes ---
lau <- fread(file.path(source_lau_path, "SE_lau.csv"), encoding = "UTF-8")
lau$`LAU NAME NATIONAL` <- iconv(lau$`LAU NAME NATIONAL`, from = "Latin1", to = "UTF-8")

stock_long<-stock
# --- Normalize region names ---
normalize_name <- function(x) stri_trans_general(x, "Latin-ASCII") %>% tolower()
stock_long$region_norm <- normalize_name(stock_long$region)
lau$region_norm   <- normalize_name(lau$`LAU NAME NATIONAL`)

# --- Merge with LAU codes ---
stock_final <- stock_long %>%
  left_join(lau, by = "region_norm")

# Keep selected columns
stock_final <- stock_final %>%
  dplyr::select(year, type.of.building,  value, 
         `NUTS 3 CODE`, `LAU CODE`)

# --- Rename NUTS column safely ---
stock_final <- dplyr::rename(stock_final, nuts3 = `NUTS 3 CODE`)

# --- Create other NUTS codes ---
stock_final <- stock_final %>%
  mutate(
    nuts3 = as.character(nuts3),
    nuts2 = str_sub(nuts3, 1, 4),
    nuts1 = str_sub(nuts3, 1, 3),
    nuts0 = str_sub(nuts3, 1, 2),
    year = as.character(year),
    type.of.building = as.character(type.of.building),
    value = as.numeric(value)
  )

setnames(stock_final, old = "LAU CODE", new = "laucode")

dt<-stock_final
agg_lau_total<-dt

agg_lau_total <- agg_lau_total %>%
  dplyr::select(year, type.of.building, value, laucode) %>%
  dplyr::rename(nuts = laucode) %>%
  dplyr::mutate(level = "laucode")

# 1. Aggregate at nuts3 level
agg_nuts3_total <-dt[, .(value = sum(value, na.rm = TRUE)), by = .(nuts3, year, type.of.building)]
agg_nuts3_total[, level := "nuts3"]
setnames(agg_nuts3_total, "nuts3", "nuts")  # rename column

# 2. Aggregate at nuts2 level
agg_nuts2_total <- dt[, .(value = sum(value, na.rm = TRUE)), by = .(nuts2, year, type.of.building)]
agg_nuts2_total[, level := "nuts2"]
setnames(agg_nuts2_total, "nuts2", "nuts")

# 3. Aggregate at nuts1 level
agg_nuts1_total <- dt[, .(value = sum(value, na.rm = TRUE)), by = .(nuts1, year, type.of.building)]
agg_nuts1_total[, level := "nuts1"]
setnames(agg_nuts1_total, "nuts1", "nuts")

# 4. Aggregate at nuts0 level
agg_nuts0_total <- dt[, .(value = sum(value, na.rm = TRUE)), by = .(nuts0, year, type.of.building)]
agg_nuts0_total[, level := "nuts0"]
setnames(agg_nuts0_total, "nuts0", "nuts")



# 5. Combine all into one table
agg_all_levels <- rbindlist(list(agg_nuts3_total, agg_nuts2_total, agg_nuts1_total, agg_nuts0_total,agg_lau_total), fill = TRUE)
agg_all_levels$measure<-"room"

all<-agg_all_levels 
all$type.of.building<-"all"
all <- all[, .(value = sum(value, na.rm = TRUE)), by = .(nuts, year, level, measure, type.of.building)]


agg_all_levels<-rbind( agg_all_levels,all)
# Using fwrite (fast, from data.table)
write.csv(
  agg_all_levels,
  file = file.path(output_path, "SE_room_clean.csv"),
  row.names = FALSE
)




## ----------------------------------------------------------
## Prepare data
## ----------------------------------------------------------
setDT(agg_all_levels)

output_dir <- file.path(base_path, "SE/outputdata/checks_graphs/")

cat("Saving room plots into:\n", output_dir, "\n")

## ----------------------------------------------------------
## Helper function: plot 12 regions per figure
## ----------------------------------------------------------
plot_group <- function(dt, region_list, group_id, output_dir, prefix) {
  
  data_sub <- dt[nuts %in% region_list]
  
  if (nrow(data_sub) == 0) {
    message("Skipping group ", group_id, " (NO DATA)")
    return()
  }
  
  data_sub[, year := as.integer(year)]
  
  p <- ggplot(
    data_sub,
    aes(x = year, y = value, group = nuts, color = nuts)
  ) +
    geom_line(size = 0.9) +
    geom_point(size = 1.5) +
    facet_wrap(~ nuts, ncol = 3, scales = "free_y") +
    labs(
      title = paste(prefix, "– Total Rooms – Group", group_id),
      x = "Year",
      y = "Total Rooms"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none")
  
  outfile <- file.path(output_dir, paste0(prefix, "_group_", group_id, ".png"))
  message("Saving: ", outfile)
  
  ggsave(
    filename = outfile,
    plot = p,
    width = 12,
    height = 10,
    dpi = 150
  )
  
  message("Saved: ", outfile)
}

## ----------------------------------------------------------
## Helper: process a level (laucode, nuts3)
## ----------------------------------------------------------
process_level <- function(level_name, prefix) {
  
  dt_sub <- agg_all_levels[
    level == level_name &
      measure == "room" &
      type.of.building == "all"
  ]
  
  region_list <- unique(dt_sub$nuts)
  
  message("Found ", length(region_list), " regions for ", prefix)
  
  groups <- split(region_list, ceiling(seq_along(region_list) / 12))
  
  for (i in seq_along(groups)) {
    plot_group(dt_sub, groups[[i]], i, output_dir, prefix)
  }
  
  message("✔ Done with ", prefix, "\n")
}

## ----------------------------------------------------------
## 1. Municipality (LAU) total rooms time-series
## ----------------------------------------------------------
process_level("laucode", "lau_rooms")

## ----------------------------------------------------------
## 2. NUTS3 total rooms time-series
## ----------------------------------------------------------
process_level("nuts3", "nuts3_rooms")

cat("\n🎉 Finished generating TOTAL ROOMS plots for MUNICIPALITY + NUTS3!\n")
