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

# git push 2 

# Plot for Canadian gas production can_gas
can_gas |>
  autoplot(Volume) +
  labs(
    title = "Canadian Natural Gas Production",
    subtitle = "Monthly, January 1960 – February 2005",
    x = NULL, y = "Billions of cubic metres") +
  theme_minimal()

# Add geom_smooth to add trend line
can_gas |>
  autoplot(Volume) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(
    title = "Canadian Natural Gas Production with Smoothed Trend",
    x = NULL, y = "Billions of cubic metres"
  ) +
  theme_minimal()

## I choose to include a geom_smooth layer which adds a smoothed trendline that follows the data and makes long-run variation and trends easier to 
## see. 


## Seasonal swings grow as the series does only during the first part of the sample. During 1960-1980 the swings get much larger as the cubic meters grow.
## After the late 1980s, the swings decrease back to a more consistent variation, meaning swings are not proportion with respect to the entire sample. 

# Define log(Volume) col
can_gas <- can_gas |>
  mutate(log_volume = log(Volume))

# Plot log(Volume)
can_gas |>
  autoplot(log_volume) +
  labs(title = "Log of Canadian Natural Gas Production",
       x = NULL, y = "log(Volume)") +
  theme_minimal()

## With logging, the steepness of the growth in certain times, like mid 1990s-2000s, are greatly flattened. The swings now look more consistent across the
## plot, apart from the region between 1970 and 1990. That being said, I think the changes point to logging being a good choice, as it makes anomalies
## and obvious differences between time periods less dramatic, and gives a percentage base to compare different times off of.

# git push 3 

# Log for retail data (define and plot)
retail_nsa <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales))

retail_nsa |>
  autoplot(sales) +
  labs(title = "NSA Retail Sales (Level)",
       x = NULL, y = "Millions of dollars") +
  theme_minimal()

retail_nsa |>
  autoplot(log_sales) +
  labs(title = "NSA Retail Sales (Log)",
       x = NULL, y = "log(sales)") +
  theme_minimal()

## The case for logging is stronger because before logging, the December spikes looked much larger as time went on, which is misleading. While the 
## overall dollar amount grew, the actual percentage of the sales spike becomes very consistent after the log, which more accurately shows growth, or 
## lack thereof. 

# Month to month percentage growth rates can_gas
can_gas <- can_gas |>
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

can_gas |>
  autoplot(growth_direct) +
  labs(
    title = "Month-over-Month Growth: Canadian Gas Production",
    x = NULL, y = "Growth rate (%)"
  ) +
  theme_minimal()

can_gas <- can_gas |>
  mutate(growth_gap = abs(growth_direct - growth_logdiff))

can_gas |>
  autoplot(growth_gap) +
  labs(
    title = "Gap Between Direct and Log-Difference Growth Rates",
    x = NULL, y = "Absolute difference (percentage points)"
  ) +
  theme_minimal()

## In most months the log difference is good, but because a lot of the time monthly growth is large the gap gets up to 2-3 percentage points during
## the months with the largest spikes/jumps. 

# Month over month log diff for retail_ts
retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

retail_ts |>
  autoplot(growth_mom) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = NULL
  ) +
  theme_minimal()

## NSA growth is way more noisy than SA's. It varies by double digits almost every year while SA sits pretty consistently in +/- 1-3% range. Its more noisy because 
## it contains that sales spike in December around the holidays, and doesnt adjust anything to account for that factor. Seasonal adjustment removes those 
## trends so it is much smoother.

# git push 4

# Plot seasonal pattern NSA sales
retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

# Plot seasonal pattern SA sales
retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in SA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

## The NSA series shows a clear and repeating seasonal pattern which dips in January to February, then trends upward in the middle of the year, and spikes in December. 
## Once seasonally adjusted, the SA series flattens out across each year, which shows/proves the removal of the seasonal pattern.

# Seasonal pattern Canadian gas
can_gas |>
  gg_season(log_volume) +
  labs(
    title = "Seasonal Pattern in Canadian Gas Production",
    y = "log(Volume)"
  ) +
  theme_minimal()

## Production seems to be higher in the winter and lower in the summer. This makes sense because most people are going to heat their homes in the winter, 
## which would drive gas use, and thus production.

can_gas |>
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)"
  )

# X_13ARIMA_SEATS and plot
retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()

## Seasonal change is largest in December at 1.13–1.15 (13–15% above the trend). Although the overall size of seasonal variations is consistent over time, 
## and the December spike has shrunk in recent years.

# Compare our ARIMA vs official SA
sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL, y = "Difference (%)"
  ) +
  theme_minimal()

## My model matches pretty close with most of the difference being under 1% the whole series. The biggest discrepancies come around and after 2020, 
## which is no surprise in economics at this point.

# Linear trend and seasonal dummies
can_gas_fit <- can_gas |>
  model(TSLM(log_volume ~ trend() + season()))

report(can_gas_fit)

can_gas_fit |>
  tidy() |>
  select(term, estimate, std.error, statistic, p.value) |>
  knitr::kable(digits = 3)

## The estimated growth given via the trend is 0.004 which means its around 0.40% per month. The most significant seasonal effect is in June
## t=-4.777 with it being around 25% below the trend.

# Fitted vals against actual data
can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
       y = "log(Volume)", color = NULL) +
  theme_minimal()

## The linear model captures the overall trend and seasonal times but fits poorly over longer periods. It over predicts in the 1960s and after 2000, 
## under predicts in the 1970s, and over shoots seasonal swings in the 1990s-2000s by forcing that fixed seasonal factor.

# Residuals for ljung-box
can_gas_fit |> gg_tsresiduals()

can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)

## The residuals do not seem to be white noise. The plot show long and slow waves that persist for years. Also the ACF beigns at ~0.98 at lag = 1 and 
## decomposes super slowly so there is still an obvious structure.The Ljung–Box stat is around 7,774 with a p-val of basically 0, so at the 5% level 
## we reject the null hypothesis of the residuals being white noise.

# git push 5
