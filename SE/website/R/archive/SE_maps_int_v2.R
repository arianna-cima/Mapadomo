library(data.table)
library(tidyverse)
library(sf)
library(plotly)
library(jsonlite)
library(geojsonio)

#------------------------------------------------------
# Load Data
#------------------------------------------------------
base_path <- "C:/Users/annai/Documents/backup_computer_ec/Contratti_lavoro/EU_Temporary_agent/materiali_di_lavoro/mapadomo/Mapadomo_v2/SE/outputdata/final/"
file_name <- "SE_regio.csv"

data <- fread(file.path(base_path, file_name)) %>% as_tibble()
data<-data[data$type=="all",]
#------------------------------------------------------
# Load Sweden LAU Shapefile (WGS84)
#------------------------------------------------------
shp_eu <- st_read(
  "C:/Users/annai/Documents/backup_computer_ec/Contratti_lavoro/EU_Temporary_agent/materiali_di_lavoro/mapadomo/Mapadomo_v2/00_georeference/LAU_RG_01M_2020_3035.shp"
) %>%
  filter(CNTR_CODE == "SE") %>%
  st_transform(4326)


#------------------------------------------------------
# Indicators & Years Available
#------------------------------------------------------
#indicators <- sort(unique(data$indicator))
#years <- sort(unique(data$year))

indicators<- c("dwelling","totalm2","price_m2")
years<-c("2021","2022")
#------------------------------------------------------
# Robust Binning Function
#------------------------------------------------------
make_bins <- function(values, n_bins = 6) {
  
  v <- values[!is.na(values)]
  
  # Case 1: No data
  if (length(v) == 0) {
    return(list(
      z_numeric = rep(NA, length(values)),
      label = rep("No data", length(values))
    ))
  }
  
  # Case 2: Only one unique value
  if (length(unique(v)) == 1) {
    lbl <- paste0("=", unique(v))
    return(list(
      z_numeric = rep(1, length(values)),
      label = rep(lbl, length(values))
    ))
  }
  
  # Case 3: Normal case → pretty bins
  rng <- range(v, na.rm = TRUE)
  bins <- pretty(rng, n_bins)
  
  # Ensure bins cover full range
  bins[1] <- min(bins[1], rng[1])
  bins[length(bins)] <- max(bins[length(bins)], rng[2])
  
  lbls <- paste0(head(bins, -1), "–", tail(bins, -1))
  
  z_bin <- cut(values, breaks = bins, include.lowest = TRUE, labels = lbls)
  
  list(
    z_numeric = as.numeric(z_bin),
    label = z_bin
  )
}


#------------------------------------------------------
# Convert shapefile to GeoJSON list once
#------------------------------------------------------
shp_geojson_str <- geojson_json(shp_eu)
shp_geojson <- jsonlite::fromJSON(shp_geojson_str, simplifyVector = FALSE)


#------------------------------------------------------
# Create all traces (indicator × year)
#------------------------------------------------------
all_traces <- list()
trace_names <- c()
trace_index <- 0

for (ind in indicators) {
  for (yr in years) {
    
    dd <- data %>%
      filter(
        level == "laucode",
        indicator == ind,
        year == yr,
        type == "all"
      )
    
    dd$value <- as.numeric(dd$value)
    
    shp_tmp <- shp_eu %>%
      left_join(dd, by = c("GISCO_ID" = "nuts"))
    
    # Create bins
    b <- make_bins(shp_tmp$value, n_bins = 6)
    shp_tmp$z_numeric <- b$z_numeric
    shp_tmp$z_label <- b$label
    
    # Create trace name
    name <- paste0(ind, "_", yr)
    trace_names <- c(trace_names, name)
    
    # Add trace
    trace_index <- trace_index + 1
    all_traces[[trace_index]] <- list(
      type = "choropleth",
      geojson = shp_geojson,
      locations = shp_tmp$GISCO_ID,
      z = shp_tmp$z_numeric,
      colorscale = "Blues",
      featureidkey = "properties.GISCO_ID",
      hovertext = paste0(
        "<b>", shp_tmp$LAU_NAME, "</b><br>",
        ind, " (", yr, "): ", format(shp_tmp$value, big.mark = ","),
        "<br>Bracket: ", shp_tmp$z_label
      ),
      hoverinfo = "text",
      visible = FALSE,
      name = name
    )
  }
}


#------------------------------------------------------
# Build FIG with all traces
#------------------------------------------------------
fig <- plot_ly()

for (tr in all_traces) {
  fig <- fig %>% add_trace(
    type = tr$type,
    geojson = tr$geojson,
    locations = tr$locations,
    z = tr$z,
    colorscale = tr$colorscale,
    featureidkey = tr$featureidkey,
    hovertext = tr$hovertext,
    hoverinfo = tr$hoverinfo,
    visible = tr$visible,
    name = tr$name
  )
}

# Show first trace by default
fig$x$data[[1]]$visible <- TRUE


#------------------------------------------------------
# Dropdown for Indicator
#------------------------------------------------------
dropdown_indicator <- list(
  type = "dropdown",
  x = 0,
  y = 1.15,
  buttons = lapply(seq_along(indicators), function(i) {
    
    vis <- rep(FALSE, length(trace_names))
    vis[grepl(paste0("^", indicators[i], "_"), trace_names)] <- TRUE
    
    list(
      method = "update",
      args = list(list(visible = vis)),
      label = indicators[i]
    )
  })
)

#------------------------------------------------------
# Dropdown for Year
#------------------------------------------------------
dropdown_year <- list(
  type = "dropdown",
  x = 0.25,
  y = 1.15,
  buttons = lapply(seq_along(years), function(i) {
    
    vis <- rep(FALSE, length(trace_names))
    vis[grepl(paste0("_", years[i], "$"), trace_names)] <- TRUE
    
    list(
      method = "update",
      args = list(list(visible = vis)),
      label = as.character(years[i])
    )
  })
)


#------------------------------------------------------
# Layout
#------------------------------------------------------
fig <- fig %>% layout(
  title = "Sweden Municipal Indicators",
  geo = list(
    fitbounds = "locations",
    visible = FALSE,
    projection = list(type = "mercator")
  ),
  updatemenus = list(dropdown_indicator, dropdown_year)
)

fig


#------------------------------------------------------
# Save HTML
#------------------------------------------------------
out_folder <- "C:/Users/annai/Documents/backup_computer_ec/Contratti_lavoro/EU_Temporary_agent/materiali_di_lavoro/mapadomo/Mapadomo_v2/SE/website/maps_SE"
dir.create(out_folder, showWarnings = FALSE, recursive = TRUE)

file_out <- file.path(out_folder, "map_ALL_INDICATORS_ALL_YEARS.html")
htmlwidgets::saveWidget(fig, file_out, selfcontained = TRUE)
