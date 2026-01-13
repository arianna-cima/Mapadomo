# Load required libraries
library(data.table)  # Efficient data handling
library(dplyr)       # Data manipulation
library(ggplot2)     # Visualization
library(scales)  # for formatting axis labels
# -----------------------------
# 1. Load the data
# -----------------------------
base_path <- "C:/Users/annai/Documents/backup_computer_ec/Contratti_lavoro/EU_Temporary_agent/materiali_di_lavoro/mapadomo/mapadomo_v2/SE/outputdata/final/"
file_name <- "SE_regio.csv"

# Read CSV as data.table and convert to tibble
data <- fread(file.path(base_path, file_name))
data_tbl <- as_tibble(data)

# -----------------------------
# 2. Summarize indicators at NUTS3 level
# -----------------------------
summary_table <- data_tbl %>%
  filter(level == "nuts3") %>%           # Focus on NUTS3 level
  group_by(indicator) %>%                # Group by indicator
  summarise(
    start_year      = min(year, na.rm = TRUE),                       # Earliest year
    end_year        = max(year, na.rm = TRUE),                       # Latest year
    n_years         = n_distinct(year),                               # Number of unique years
    n_nuts3         = n_distinct(nuts),                               # Number of NUTS3 regions
    avg_start_value = round(mean(value[year == min(year)], na.rm = TRUE), 0),  # Avg value at start year
    avg_end_value   = round(mean(value[year == max(year)], na.rm = TRUE), 0),  # Avg value at end year
    n_laucode_nuts  = n_distinct(data_tbl$nuts[data_tbl$indicator == indicator & data_tbl$level == "laucode"]),  # Count of LAU-level regions per indicator
    .groups = "drop"
  )

# Print summary table
print(summary_table)

# -----------------------------
# 3. Mark capital regions
# -----------------------------
dd <- data_tbl %>%
  filter(level == "nuts3") %>%  # Only NUTS3 level
  mutate(
    nuts = as.factor(nuts),
    is_capital = ifelse(nuts == "SE110", "Capital", "Other"),  # Flag the capital
    is_capital = factor(is_capital, levels = c("Other", "Capital"))  # Order factor
  )

# -----------------------------
# 4. Filter and reshape for plotting
# -----------------------------
# Focus on two indicators
dd_summary <- dd %>% filter(indicator %in% c("pricem2", "totalm2"))

# Convert to data.table for reshaping
setDT(dd_summary)

# Convert long to wide format: one row per nuts+year+is_capital
dd_wide <- dcast(
  dd_summary, 
  nuts + year + is_capital ~ indicator, 
  value.var = "value", 
  fun.aggregate = sum
)

# -----------------------------
# 5. Create bubble plot
# -----------------------------
ggplot(dd_wide, aes(x = year, y = pricem2, size = totalm2, color = is_capital)) +
  geom_point(alpha = 0.6) +                                # Semi-transparent points
  scale_size(range = c(1, 12), name = "Total m2") +     # Size of bubbles
  scale_color_manual(values = c("Other" = "darkgrey", "Capital" = "darkblue")) +  # Custom colors
  theme_minimal(base_size = 14) +                          # Clean minimal theme
  labs(
    x = "Year",
    y = "Price per m2",
    color = "",
    title = "Bubble plot by NUTS3 (Capital vs Other)"
  ) +
  theme(
    legend.position = "right", 
    plot.title = element_text(face = "bold", hjust = 0.5)   # Centered bold title
  )



# 1. Read metro dataset
metro_path <- "C:/Users/annai/Documents/backup_computer_ec/Contratti_lavoro/EU_Temporary_agent/materiali_di_lavoro/mapadomo/Mapadomo_v2/00_disseminate/countrypages/sourcedata/Metro-regions-NUTS-2016.csv"
metro <- fread(metro_path, header = TRUE)

dd_merged <- dd %>%
  left_join(metro, by = c("nuts" = "NUTS_ID")) %>%
  
  # Create new metro/rural/capital classification
  mutate(
    is_capital = case_when(
      grepl("MC", MREG_CODE) ~ "capital",  # MREG_CODE contains "MC"
      !is.na(MREG_CODE) ~ "metro",         # MREG_CODE not NA → metro
      TRUE ~ "rural"                       # NA → rural
    )
  ) %>%
  
  # Keep first 7 columns (force dplyr)
  dplyr::select(1:7)




#################################graph contribution

# 1. Aggregate by year and region type
df <- dd_merged %>%
  filter(level == "nuts3", indicator == "totalm2") %>%
  group_by(year, is_capital) %>%
  summarise(total_m2 = sum(value, na.rm = TRUE), .groups = "drop")

# 2. Pivot to wide format (one column per region type)
df_wide <- df %>%
  pivot_wider(
    names_from = is_capital,
    values_from = total_m2,
    values_fill = list(total_m2 = 0)
  ) %>%
  arrange(year)

# Ensure columns exist for all region types
for (col in c("capital", "metro", "rural")) {
  if (!col %in% names(df_wide)) df_wide[[col]] <- 0
}

# 3. Compute national total and annual % growth
df_wide <- df_wide %>%
  mutate(
    national_total = capital + metro + rural,
    national_total_lag = lag(national_total),
    national_pct_growth = 100 * (national_total - national_total_lag) / national_total_lag
  )

# 4. Compute annual growth per region type (in m² and as % of previous national total)
df_wide <- df_wide %>%
  mutate(
    capital_growth = capital - lag(capital),
    metro_growth   = metro - lag(metro),
    rural_growth   = rural - lag(rural),
    capital_pct_contrib = 100 * capital_growth / national_total_lag,
    metro_pct_contrib   = 100 * metro_growth / national_total_lag,
    rural_pct_contrib   = 100 * rural_growth / national_total_lag
  )

# Convert to data.frame, then tibble for dplyr::select
df_wide <- as.data.frame(df_wide)
df_wide <- tibble::as_tibble(df_wide)

# 5. Prepare data for plotting (stacked bar: y = national % growth, fill = region's % point contribution)
df_plot <- df_wide %>%
  dplyr::select(year, national_pct_growth, capital_pct_contrib, metro_pct_contrib, rural_pct_contrib) %>%
  pivot_longer(
    cols = c(capital_pct_contrib, metro_pct_contrib, rural_pct_contrib),
    names_to = "region_type",
    values_to = "pct_contribution"
  ) %>%
  mutate(region_type = recode(region_type,
                              "capital_pct_contrib" = "capital",
                              "metro_pct_contrib" = "metro",
                              "rural_pct_contrib" = "rural"))

# 6. Plot stacked bar chart (y-axis = national % growth, fill = region's % point contribution)
ggplot(df_plot, aes(x = factor(year), y = pct_contribution, fill = region_type)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("capital" = "darkblue",
                               "metro" = "steelblue",
                               "rural" = "grey70")) +
  scale_y_continuous(labels = label_number(accuracy = 0.1), name = "Annual % growth in total m²") +
  labs(
    x = "Year",
    fill = "Region type",
    title = "Residential Square Metre Growth: Regional % Point Contribution"
  ) +
  theme_minimal(base_size = 8) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )









# 1. Create DT table widget for summary table
summary_widget <- datatable(summary_table, options = list(pageLength = 10))

# 2. Save ggplots as htmlwidgets
library(ggplot2)
library(plotly)

# Bubble plot
bubble_plot <- ggplot(dd_wide, aes(x = year, y = pricem2, size = totalm2, color = is_capital)) +
  geom_point(alpha = 0.6) +
  scale_size(range = c(1, 12), name = "Total m2") +
  scale_color_manual(values = c("Other" = "darkgrey", "Capital" = "darkblue")) +
  theme_minimal(base_size = 14) +
  labs(x = "Year", y = "Price per m2", color = "", title = "Bubble plot by NUTS3 (Capital vs Other)") +
  theme(legend.position = "right", plot.title = element_text(face = "bold", hjust = 0.5))

bubble_plotly <- plotly::ggplotly(bubble_plot)

# Contribution stacked bar
stacked_plot <- ggplot(df_plot, aes(x = factor(year), y = pct_contribution, fill = region_type)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("capital" = "darkblue", "metro" = "steelblue", "rural" = "grey70")) +
  scale_y_continuous(labels = label_number(accuracy = 0.1), name = "Annual % growth in total m²") +
  labs(x = "Year", fill = "Region type", title = "Residential Square Metre Growth: Regional % Point Contribution") +
  theme_minimal(base_size = 8) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1))

stacked_plotly <- plotly::ggplotly(stacked_plot)

# -----------------------------
# 3. Combine everything into a single HTML page
# -----------------------------
html_page <- tags$html(
  tags$head(
    tags$title("Residential Square Metre Growth Report")
  ),
  tags$body(
    tags$h1("Residential Square Metre Growth Analysis"),
    tags$h2("Summary Table"),
    htmltools::tagList(summary_widget),  # table
    tags$h2("Bubble Plot (Price per m2)"),
    htmltools::tagList(bubble_plotly),   # bubble plot
    tags$h2("Contribution Stacked Bar Chart"),
    htmltools::tagList(stacked_plotly)   # stacked bar
  )
)

# -----------------------------
# 4. Save to HTML
# -----------------------------
save_html(html_page, "C:/Users/annai/Documents/backup_computer_ec/Contratti_lavoro/EU_Temporary_agent/materiali_di_lavoro/mapadomo/mapadomo_v2/SE/website/residential_growth_report.html")
