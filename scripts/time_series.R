## RLionheart
## 21-0771-001
## February 2023
## Shoreline Conservation Areas, Washington State Parks


source("scripts/src/load_packages.R")

#profile.pattern <- "prof_6|prof_7|prof_8|prof_9|prof_17|prof_41"
profile.pattern <- "prof_6"
source("scripts/src/import_profiles.R")

## Import erosion file for Base Point data
profile.erosion <- read_csv("data_raw/ProfilesForErosion.csv", 
                            col_names = c("profile", "Park", "MHHW",
                                          "BasePoint_X", "BasePoint_Y", 
                                          "Start_Year", "Start_X", "Start_Y", "Start_Dist",
                                          "End_Year", "End_X", "End_Y", "End_Dist",
                                          "Total_Change", "Years", "Change_per_Year",
                                          "Hannah", "2050", "Comments"), 
                            skip = 3, show_col_types = FALSE) %>%
  filter(profile %in% str_extract_all(profile.pattern,"\\(?[0-9,.]+\\)?")[[1]])

# 98 ----------------------------------------------------------------------
complete.profile.98 <- profile.erosion %>%
  full_join(profiles.df, by = "profile") %>%
  select(profile, Park, MHHW, BasePoint_X, BasePoint_Y, season:z) %>%
  filter(year == "98")

## xyz Linear model
N <- nrow(complete.profile.98) 

mean_profile <- apply(complete.profile.98[, 8:10], 2, mean)
pca_profile  <- princomp(complete.profile.98[, 8:10]) 
vector_profile <- pca_profile$loadings[, 1]

profile_fit <- matrix(rep(mean_profile, each = N), ncol=3) + 
  pca_profile$score[, 1] %*% t(vector_profile) 
fitted.values.98 <- as_tibble(profile_fit) %>%
  rename(x_fit = x, y_fit = y, z_fit = z)


# 99 ----------------------------------------------------------------------
complete.profile.99 <- profile.erosion %>%
  full_join(profiles.df, by = "profile") %>%
  select(profile, Park, MHHW, BasePoint_X, BasePoint_Y, season:z) %>%
  filter(year == "99")

N <- nrow(complete.profile.99) 

mean_profile <- apply(complete.profile.99[, 8:10], 2, mean)
pca_profile  <- princomp(complete.profile.99[, 8:10]) 
vector_profile <- pca_profile$loadings[, 1]

profile_fit <- matrix(rep(mean_profile, each = N), ncol=3) + 
  pca_profile$score[, 1] %*% t(vector_profile) 
fitted.values.99 <- as_tibble(profile_fit) %>%
  rename(x_fit = x, y_fit = y, z_fit = z)

# plot ----------------------------------------------------------------------
plot_ly(x=fitted.values.98$x_fit, y=fitted.values.98$y_fit, z=fitted.values.98$z_fit,
        type="scatter3d", mode="markers", size = 1, name = "1998") %>%
  add_trace(x=fitted.values.99$x_fit, y=fitted.values.99$y_fit, z=fitted.values.99$z_fit,
            type="scatter3d", mode="markers", size = 1, name = "1999")



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

