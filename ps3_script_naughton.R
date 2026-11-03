library(tidyverse)
library(fpp3)

retail_raw <- read_csv("data/retail_sales_raw.csv")

head(retail_raw)

# Format date YYYY-MM-DD
retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

# Change data from wide (2 columns) to long
retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")

# observation_date col convert to proper monthly index
retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

# tsibble for retail_ts
head(retail_ts)

# Pings gaps in monthly data
has_gaps(retail_ts)

# See key and index variables in canadian_gas
key_vars(canadian_gas)
index_var(canadian_gas)

# Create copy of data can_gas, check for monthly gaps
can_gas <- canadian_gas |>
  select(Month, Volume)

has_gaps(can_gas)

# git push 1

# Create tsibble, Jan 1980 onward
can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)

# Add ability to pull certain time-frames (added month as col)
can_gas_recent <- can_gas_recent |>
  mutate(year    = year(Month),
         month   = month(Month, label = TRUE),
         quarter = quarter(Month))

# Use new month col^ to get average Volume per cal month
can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

## Since 1990, December has the highest average at ~17.1

# Make & define variables for next month and same month last year
can_gas_recent |>
  mutate(next_month           = Month + 1,
         same_month_last_year = Month - 12) |>
  head()

# Convert quarterly from monthly
can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume)

## Data is summed because we want the total gas produced in that quarter (3 months, so sum month 1, 2 ,3). 
## The mean, which we already did above, would give us the average volume of each month with respect to the current quarter,
## which to put simply, we don't want.

## Yes the data still seems to be seasonal as changing to quarters gets rid of monthly seasonality within the respective quarter,
## but still allows seasonal patterns to be noticed quarterly.

# Simple time series plot + layer original plot

retail_ts |> autoplot(sales) 
retail_ts |> 
  autoplot(sales) + 
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

## The NSA series has a sawtooth-like pattern that seems to repeat annually. Sales spike in December right before the holidays, then drop quickly in
## the following months with smaller spike patterns near summer. The SA series doesn't show a pattern like the previous at all. It is a smooth line
## which goes right through the middle of the NSA line. They have the same upward trend and both show the same general large movements
## (Covid Recession, 2008 Crisis, etc).
## Seasonal adjustment removes only the repeating within-year pattern and not the overall patterns year to year.



