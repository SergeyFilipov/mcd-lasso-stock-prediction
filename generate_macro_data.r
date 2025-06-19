# Load Required Libraries
library(dplyr)
library(lubridate)
library(tidyr)
library(readr)
library(tidyquant)
library(writexl)

# Block 1: Generate Synthetic Macroeconomic Data
# Create quarterly dates from 2018 to 2025
dates <- seq(as.Date("2018-03-31"), as.Date("2025-12-31"), by = "3 months")

# Set seed for reproducibility
set.seed(123)

# Generate synthetic macroeconomic indicators
macro_long <- expand.grid(
  date = dates,
  name = c("inflation", "unemployment", "gdp_growth")
) %>%
  mutate(
    value = case_when(
      name == "inflation"    ~ round(runif(n(), 1.5, 5.0), 2),
      name == "unemployment" ~ round(runif(n(), 3.0, 6.0), 2),
      name == "gdp_growth"   ~ round(rnorm(n(), mean = 2.0, sd = 0.8), 2)
    )
  )

# Save to CSV
write_csv(macro_long, "macro_data.csv")

# Preview
print(head(macro_long))


# Block 2: Download and Aggregate McDonald's Stock Data
# Set ticker and date range
ticker <- "MCD"
start_date <- "2018-06-30"
end_date <- "2025-03-31"

# Download daily stock prices using tidyquant
prices <- tq_get(
  ticker,
  from = start_date,
  to = end_date,
  get = "stock.prices"
)

# Aggregate to quarterly OHLCV values
prices_q <- prices %>%
  mutate(quarter = paste0(year(date), "_Q", quarter(date))) %>%
  group_by(quarter) %>%
  summarise(
    open      = mean(open, na.rm = TRUE),
    high      = mean(high, na.rm = TRUE),
    low       = mean(low, na.rm = TRUE),
    close     = mean(close, na.rm = TRUE),
    adj_close = mean(adjusted, na.rm = TRUE),
    volume    = sum(volume, na.rm = TRUE)
  ) %>%
  ungroup()

# Print the result
print(prices_q)

# Save to Excel
write_xlsx(prices_q, path = "MCD_Quarterly_Stock_Prices_2018_2025.xlsx")
