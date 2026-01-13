

# -----------------------------
# Assume your data and plots already exist
# For example:
# summary_table
# dd_wide plot (bubble plot)
# df_plot (contribution stacked bar chart)
# -----------------------------

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
