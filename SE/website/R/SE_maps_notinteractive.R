#------------------------------------------------------
# Libraries
#------------------------------------------------------
library(data.table)
library(dplyr)
library(sf)
library(ggplot2)
library(viridis)

#------------------------------------------------------
# Load Data
#------------------------------------------------------
file_name <- "SE_regio.csv"

data <- fread(file.path(output_end_path, file_name))

# make sure key variables are correct type
data[, LAU_CODE := as.character(nuts)]
data[, year := as.integer(year)]
data[, value := as.numeric(value)]

#------------------------------------------------------
# Load Sweden LAU Shapefile (WGS84)
#------------------------------------------------------
file_name <- "LAU_RG_01M_2020_3035.shp"

shp_eu <- st_read(
  file.path(source_geo, file_name),
  quiet = TRUE
) %>%
  filter(CNTR_CODE == cc) %>%
  st_transform(4326) %>%
  mutate(LAU_CODE = as.character(GISCO_ID))

#------------------------------------------------------
# Join data to geometry
#------------------------------------------------------
map_data <- shp_eu %>%
  left_join(data, by = "LAU_CODE")

#------------------------------------------------------
# Output folder
#------------------------------------------------------
base_path <- "C:/Users/annai/Documents/backup_computer_ec/Contratti_lavoro/EU_Temporary_agent/materiali_di_lavoro/mapadomo/Mapadomo_v2/SE"
output_path <- file.path(base_path, "outputdata", "checks_graphs", "maps")

#------------------------------------------------------
# Get dimensions
#------------------------------------------------------
years      <- sort(unique(map_data$year[map_data$year > 2013]))
indicators <- unique(map_data$indicator)
types      <- unique(map_data$type)

#------------------------------------------------------
# Mapping function
#------------------------------------------------------
plot_map <- function(df, y, ind, tp) {
  
  ggplot(
    df %>% filter(year == y, indicator == ind, type == tp)
  ) +
    geom_sf(aes(fill = value), color = NA) +
    scale_fill_viridis_c(
      option = "C",
      na.value = "grey90"
    ) +
    labs(
      title    = paste("Sweden –", ind),
      subtitle = paste("Type:", tp, "| Year:", y),
      fill     = ind,
      caption  = "Source: SE_regio"
    ) +
    theme_minimal() +
    theme(
      axis.text  = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
}

#------------------------------------------------------
# Loop: year × indicator × type
#------------------------------------------------------
for (y in years) {
  for (ind in indicators) {
    for (tp in types) {
      
      p <- plot_map(map_data, y, ind, tp)
      
      ggsave(
        filename = paste0(
          "SE_", ind, "_", tp, "_", y, ".png"
        ),
        plot  = p,
        path  = output_path,
        width = 7,
        height = 9,
        dpi   = 300
      )
    }
  }
}
