#############################################
# 1. Load the data
#############################################
file_name <- "EE_regio.csv"

data <- fread(file.path(output_end_path, file_name))
data_tbl <- as_tibble(data)


#############################################
# 2. Summarize indicators at NUTS3 level
#############################################
summary_table <- data_tbl %>% 
  filter(level == "laucode") %>%
  group_by(indicator) %>%
  summarise(
    start_year      = min(year, na.rm = TRUE),
    end_year        = max(year, na.rm = TRUE),
    n_years         = n_distinct(year),
    n_nuts3         = n_distinct(nuts),
    
    avg_start_value = round(mean(value[year == start_year], na.rm = TRUE), 0),
    avg_end_value   = round(mean(value[year == end_year], na.rm = TRUE), 0),
    
    .groups = "drop"
  )


#############################################
# 3. Mark capital regions in NUTS3 dataset
#############################################
dd <- data_tbl %>%
  filter(level == "nuts3") %>%
  mutate(
    nuts = as.factor(nuts),
    is_capital = ifelse(nuts == "EE001", "Capital", "Other"),
    is_capital = factor(is_capital, levels = c("Other", "Capital"))
  )


#############################################
# 4. Filter and reshape data for bubble plot
#############################################
dd_summary <- dd %>% filter(indicator %in% c("ppm2", "totalm2"))
setDT(dd_summary)

dd_wide <- dcast(
  dd_summary, 
  nuts + year + is_capital ~ indicator, 
  value.var = "value",
  fun.aggregate = sum
)


#############################################
# 5. Create bubble plot
#############################################
bubble_plot <- ggplot(dd_wide, aes(x = year, y = ppm2, size = totalm2, color = is_capital)) +
  geom_point(alpha = 0.6) +
  scale_size(range = c(1, 10), name = "Total m2") +
  scale_color_manual(values = c("Other" = "darkgrey", "Capital" = "darkblue")) +
  scale_x_continuous(breaks = seq(min(dd_wide$year), max(dd_wide$year), by = 1)) +
  theme_minimal(base_size = 12) +
  labs(
    x = "Year",
    y = "Price per m2",
    color = "",
    title = "Bubble plot by NUTS3 (Capital vs Other)"
  ) +
  theme(
    legend.position = "right", 
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

bubble_plotly <- plotly::ggplotly(bubble_plot, width = 600, height = 350)


#############################################
# 6. Load metro-region classification & merge
#############################################

metro <- data.table::fread(file.path(diss,"countrypages","sourcedata","Metro-regions-NUTS-2016.csv"),header = TRUE)

dd_merged <- dd %>%
  left_join(metro, by = c("nuts" = "NUTS_ID")) %>%
  mutate(
    is_capital = case_when(
      grepl("MC", MREG_CODE) ~ "capital",
      !is.na(MREG_CODE) ~ "metro",
      TRUE ~ "rural"
    )
  ) %>%
  dplyr::select(1:7)


df <- dd_merged %>%
  filter(indicator == "totalm2",
         type == "flat",
         level == "nuts3")

# --------------------------------------------
# 2. Aggregate: total m2 per year × area type
# --------------------------------------------

df_area <- df %>%
  group_by(year, is_capital) %>%
  summarise(total_m2 = sum(value, na.rm = TRUE), .groups = "drop")

# --------------------------------------------
# 3. Wide format (capital, metro, rural columns)
# --------------------------------------------

df_wide <- df_area %>%
  pivot_wider(
    names_from = is_capital,
    values_from = total_m2
  ) %>%
  arrange(year)


setDT(df_wide)          # convert to data.table (by reference)
setorder(df_wide, year) # make sure data is ordered in time

df_wide[, national_total :=
          as.numeric(capital) +
          as.numeric(rural)]

df_wide[, national_lag := shift(national_total, 1L)]

df_wide[, national_pct_growth :=
          100 * (national_total - national_lag) / national_lag]
# --------------------------------------------
# 4. National totals + national growth rate
# --------------------------------------------


# df_wide <- df_wide %>%
#   mutate(
#     national_total = capital + metro + rural,
#     national_lag   = lag(national_total),
#     
#     # national growth in percentage points
#     national_pct_growth =
#       100 * (national_total - national_lag) / national_lag
#   )

# --------------------------------------------
# 5. Contributions of capital, metro, rural
# --------------------------------------------

setDT(df_wide)
setorder(df_wide, year)

df_wide[, capital_pct_contrib :=
          100 * (capital - shift(capital, 1L)) /
          shift(national_total, 1L)]


df_wide[, rural_pct_contrib :=
          100 * (rural - shift(rural, 1L)) /
          shift(national_total, 1L)]
# 
# df_wide <- df_wide %>%
#   mutate(
#     capital_pct_contrib =
#       100 * (capital - lag(capital)) / lag(national_total),
#     
#     metro_pct_contrib =
#       100 * (metro - lag(metro)) / lag(national_total),
#     
#     rural_pct_contrib =
#       100 * (rural - lag(rural))   / lag(national_total)
#   )

# --------------------------------------------
# 6. Long format for plotting
# --------------------------------------------

df_plot <- df_wide %>%
  select(year, national_pct_growth,
         capital_pct_contrib,  rural_pct_contrib) %>%
  pivot_longer(
    cols = c(capital_pct_contrib, rural_pct_contrib),
    names_to = "region_type",
    values_to = "pct_contribution"
  ) %>%
  mutate(
    region_type = recode(region_type,
                         "capital_pct_contrib" = "capital",
                         
                         "rural_pct_contrib"   = "rural"),
    region_type = factor(region_type,
                         levels = c("capital",  "rural"))
  ) %>%
  filter(!is.na(pct_contribution))

# --------------------------------------------
# 7. Final plot
# --------------------------------------------


stacked_plot <- plot_ly() %>%
  add_bars(
    data = df_plot,
    x = ~year,
    y = ~pct_contribution,
    color = ~region_type,
    colors = c("capital" = "darkblue", 
               "metro" = "steelblue", 
               "rural" = "grey70"),
    marker = list(line = list(color = "black", width = 0.7))
  ) %>%
  add_lines(
    data = df_wide,
    x = ~year,
    y = ~national_pct_growth,
    name = "National growth",
    line = list(width = 3, color = "black")
  ) %>%
  add_markers(
    data = df_wide,
    x = ~year,
    y = ~national_pct_growth,
    name = "National growth",
    marker = list(size = 6, color = "black")
  ) %>%
  layout(
    title = "Contribution of Capital, Metro, and Rural Areas to National Floor Area Growth",
    barmode = "stack",
    xaxis = list(title = "Year"),
    yaxis = list(title = "Percentage Points (pp)"),
    legend = list(title = list(text = "Area Type")),
    width = 600,
    height = 350
  )

stacked_plotly <- stacked_plot

#############################################
# 9. Create datatable widget
#############################################
summary_widget <- datatable(
  summary_table, 
  options = list(pageLength = 10, dom = 't', autoWidth = TRUE),
  class = 'compact'
)


#############################################
# 10. Build final HTML page with plots + table
#############################################
html_page <- tags$html(
  tags$head(
    tags$title("Residential Square Metre Growth Report"),
    tags$style(HTML("
      body { font-family: Arial, sans-serif; margin: 40px; }
      h1, h2 { margin-top: 24px; margin-bottom: 12px; }
      .plot-container { margin-bottom: 32px; }
      .datatable-container { max-width: 700px; margin-bottom: 32px; }
    "))
  ),
  tags$body(
    tags$h1("Residential Square Metre Growth Analysis"),
    
    tags$h2("Summary Table"),
    tags$div(class = "datatable-container", tagList(summary_widget)),
    
    tags$h2("Bubble Plot (Price per m2)"),
    tags$div(class = "plot-container", tagList(bubble_plotly)),
    
    tags$h2("Contribution Total m2 "),
    tags$div(class = "plot-container", tagList(stacked_plotly))
  )
)

#############################################
# 11. Save HTML report
#############################################
save_html(html_page,file.path(  ROOT,  "EE",  "website",  "residential_growth_report.html"))














