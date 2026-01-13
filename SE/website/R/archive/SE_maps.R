library(data.table)
library(tidyverse)
library(sf)
library(plotly)
library(jsonlite)
library(geojsonio)

#------------------------------------------------------
# Load Data
#------------------------------------------------------
base_path <- "C:/Users/annai/Documents/backup_computer_ec/Contratti_lavoro/EU_Temporary_agent/materiali_di_lavoro/mapadomo/mapadomo_v2/SE/outputdata/final/"
file_name <- "SE_regio.csv"

data <- fread(file.path(base_path, file_name))
data_tbl <- as_tibble(data)

#------------------------------------------------------
# Load Sweden LAU Shapefile (WGS84)
#------------------------------------------------------
shp_eu <- st_read(
  "C:/Users/annai/Documents/backup_computer_ec/Contratti_lavoro/EU_Temporary_agent/materiali_di_lavoro/mapadomo/Mapadomo_v2/00_georeference/LAU_RG_01M_2020_3035.shp"
) %>%
  filter(CNTR_CODE == "SE") %>%
  st_transform(4326)

#------------------------------------------------------
# Filter indicator-year-type
#------------------------------------------------------
ind <- "price_m2"
yr  <- "2023"
tp  <- "all"

dd <- data_tbl %>%
  filter(
    level == "laucode",
    indicator == ind,
    year == yr,
    type == tp
  )

dd$value <- as.numeric(dd$value)

#------------------------------------------------------
# JOIN
#------------------------------------------------------
shp_eu <- shp_eu %>%
  left_join(dd, by = c("GISCO_ID" = "nuts"))

#------------------------------------------------------
# Build quantile classes (for better colors)
#------------------------------------------------------
shp_eu$z_quantile <- cut(
  shp_eu$value,
  breaks = quantile(shp_eu$value, probs = seq(0, 1, 0.2), na.rm = TRUE),
  include.lowest = TRUE,
  labels = FALSE
)

#------------------------------------------------------
# Convert sf → geojson list
#------------------------------------------------------
shp_geojson_str <- geojson_json(shp_eu)
shp_geojson <- jsonlite::fromJSON(shp_geojson_str, simplifyVector = FALSE)

#------------------------------------------------------
# Plot
#------------------------------------------------------
fig <- plot_ly()

fig <- fig %>% add_trace(
  type = "choropleth",
  geojson = shp_geojson,
  locations = shp_eu$GISCO_ID,
  z = shp_eu$z_quantile,     # <--- QUANTILE COLORS
  colorscale = "Blues",
  featureidkey = "properties.GISCO_ID",
  hovertext = paste0(
    "<b>", shp_eu$LAU_NAME, "</b><br>",
    "Value: ", format(shp_eu$value, big.mark = ",")
  ),
  hoverinfo = "text"
)

fig <- fig %>% layout(
  title = paste0("Total m2 — Sweden — ", yr),
  geo = list(
    fitbounds = "locations",
    visible = FALSE,
    projection = list(type = "mercator")   # <---- FLAT MAP
  )
)

fig

#------------------------------------------------------
# Save map
#------------------------------------------------------
out_folder <- "C:/Users/annai/Documents/backup_computer_ec/Contratti_lavoro/EU_Temporary_agent/materiali_di_lavoro/mapadomo/Mapadomo_v2/SE/website/maps_SE"
dir.create(out_folder, showWarnings = FALSE, recursive = TRUE)

file_out <- file.path(out_folder, paste0("map_", ind, "_", yr, "_", tp, ".html"))
saveWidget(fig, file_out, selfcontained = TRUE)
