#---------------------------------------------------------
# Load Data
#---------------------------------------------------------
file_name <- "EE_regio.csv"

data <- data.table::fread(file.path(output_end_path, file_name))
data_tbl <- tibble::as_tibble(data)

#---------------------------------------------------------
# Load Sweden LAU Shapefile (WGS84)
#---------------------------------------------------------
shp_eu <- sf::st_read(
  file.path(source_geo, "LAU_RG_01M_2020_3035.shp"),
  quiet = TRUE
) %>%
  dplyr::filter(CNTR_CODE == cc) %>%
  sf::st_transform(4326)

#---------------------------------------------------------
# Indicators to include in dropdown
#---------------------------------------------------------
indicators <- c("dwellings", "ppm2", "totalm2")
yr <- "2019"
tp <- "all"



#---------------------------------------------------------
# Build a list of datasets, one per indicator
#---------------------------------------------------------
indicator_data <- list()

for (ind in indicators) {
  
  dd <- data_tbl %>%
    dplyr::filter(
      level == "laucode",
      indicator == ind,
      year == yr,
      type == tp
    )
  
  dd$value <- as.numeric(dd$value)
  
  shp_tmp <- shp_eu %>%
    dplyr::left_join(dd, by = c("GISCO_ID" = "nuts"))
  
  shp_tmp$z_quantile <- cut(
    shp_tmp$value,
    breaks = quantile(shp_tmp$value, probs = seq(0, 1, 0.2), na.rm = TRUE),
    include.lowest = TRUE,
    labels = FALSE
  )
  
  indicator_data[[ind]] <- shp_tmp
}

#---------------------------------------------------------
# Convert sf → geojson (shared geometry)
#---------------------------------------------------------
shp_geojson_str <- geojsonio::geojson_json(shp_eu)
shp_geojson <- jsonlite::fromJSON(shp_geojson_str, simplifyVector = FALSE)

#---------------------------------------------------------
# Build Plotly figure
#---------------------------------------------------------
fig <- plotly::plot_ly()

for (i in seq_along(indicators)) {
  
  ind <- indicators[i]
  shp_tmp <- indicator_data[[ind]]
  
  fig <- fig %>%
    plotly::add_trace(
      type = "choropleth",
      geojson = shp_geojson,
      locations = shp_tmp$GISCO_ID,
      z = shp_tmp$z_quantile,
      colorscale = "Blues",
      featureidkey = "properties.GISCO_ID",
      hovertext = paste0(
        "<b>", shp_tmp$LAU_NAME, "</b><br>",
        ind, ": ", format(shp_tmp$value, big.mark = ",")
      ),
      hoverinfo = "text",
      visible = (i == 1)
    )
}

#---------------------------------------------------------
# Dropdown menu
#---------------------------------------------------------
fig <- fig %>%
  plotly::layout(
    title = paste0("Estonia Municipal Indicators (", yr, ")"),
    geo = list(
      fitbounds = "locations",
      visible = FALSE,
      projection = list(type = "mercator")
    ),
    updatemenus = list(
      list(
        type = "dropdown",
        direction = "down",
        x = 0,
        y = 1.15,
        buttons = lapply(seq_along(indicators), function(i) {
          list(
            method = "update",
            args = list(
              list(visible = {
                v <- rep(FALSE, length(indicators))
                v[i] <- TRUE
                v
              }),
              list(title = paste0("Indicator: ", indicators[i]))
            ),
            label = indicators[i]
          )
        })
      )
    )
  )

fig

#---------------------------------------------------------
# Save HTML (self-contained)
#---------------------------------------------------------
file_out <- file.path(website_path, paste0("map_dropdown_", yr, ".html"))
htmlwidgets::saveWidget(fig, file_out, selfcontained = TRUE)