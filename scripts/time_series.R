## RLionheart
## 21-0771-001
## February 2023
## Shoreline Conservation Areas, Washington State Parks

source("scripts/load_packages.R")


profile.pattern <- "prof_6"
source("scripts/import_profiles.R")

profiles.df %>% 
  filter(season == "s" & year == "97") %>%
  group_by(season, year) %>% 
  mutate(rate = 100 * (count - lag(count))/lag(count)) %>%
  ungroup()




###########################################################

library(healthyR.ts)
library(dplyr)

data_tbl <- ts_to_tbl(AirPassengers) %>%
  select(-index)

output <- ts_ma_plot(
  .data = data_tbl,
  .date_col = date_col,
  .value_col = value
)

output$data_trans_xts %>% head()
output$xts_plt

###########################################################

library(forecast)
library(ggplot2)

# ETS forecasts
USAccDeaths %>%
  ets() %>%
  forecast() %>%
  autoplot()

# Automatic ARIMA forecasts
WWWusage %>%
  auto.arima() %>%
  forecast(h=20) %>%
  autoplot()

# ARFIMA forecasts
library(fracdiff)
x <- fracdiff.sim( 100, ma=-.4, d=.3)$series
arfima(x) %>%
  forecast(h=30) %>%
  autoplot()

# Forecasting with STL
USAccDeaths %>%
  stlm(modelfunction=ar) %>%
  forecast(h=36) %>%
  autoplot()

AirPassengers %>%
  stlf(lambda=0) %>%
  autoplot()

USAccDeaths %>%
  stl(s.window='periodic') %>%
  forecast() %>%
  autoplot()

# TBATS forecasts
USAccDeaths %>%
  tbats() %>%
  forecast() %>%
  autoplot()

