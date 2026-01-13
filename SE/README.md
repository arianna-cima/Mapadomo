# SE Price Timeseries Flat All

This project contains R scripts for processing and analyzing timeseries price data related to the SE (Sweden) market. The main script, `SE_price_timeseries_flat_all.R`, is located in the `rcode/timeseries_price` directory.

## Project Structure

- `rcode/timeseries_price/SE_price_timeseries_flat_all.R`  
  Main R script for flattening and analyzing SE price timeseries data.

## Requirements

- R (version 4.0 or higher recommended)
- Required R packages:
  - `tidyverse`
  - `lubridate`
  - `data.table`
  - (Add any other packages used in your script)

## Usage

1. Open the R script in your preferred R environment (e.g., RStudio).
2. Install required packages if not already installed:
   ```R
   install.packages(c("tidyverse", "lubridate", "data.table"))
   ```
3. Run the script:
   ```R
   source("rcode/timeseries_price/SE_price_timeseries_flat_all.R")
   ```

## Description

The script processes raw timeseries price data, flattens it, and outputs a cleaned dataset suitable for further analysis or visualization. Please refer to the comments within the script for detailed explanations of each step.

## License

Specify your license here (e.g., MIT, GPL-3.0, etc.).

## Author

Add your name and contact information here.
