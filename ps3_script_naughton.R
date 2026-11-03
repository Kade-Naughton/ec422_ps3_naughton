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