## RLionheart
## 21-0771-001
## February 2023
## Shoreline Conservation Areas, Washington State Parks


# CalculateChangeRates <- function(x, y) {
#   xval = (x - lag(x))/lag(x)
#   yval = (y - lag(y))/lag(y)
#   rate = xval/yval
# 
#   return(rate)
# }


CalculateChangeRates <- function(x, y) {
  xval = (max(x) - min(x)/min(x))
  yval = (max(y) - min(y)/min(y))
  rate = xval/yval

  return(rate)
}

MakeRateDF <- function(df, description) {
  rate_df <- df %>%
    select(profile, year, rate = paste(description, "_rate", sep = "")) %>%
    mutate(position = description)
  
  return(rate_df)
}

## Take quartile points along profiles and use euclidean distance to BP as the change rate.

profile.pattern <- "prof_17"
year.pattern <- c("00")

source("scripts/src/load_packages.R")
source("scripts/src/import_profiles.R")

## Import erosion file for Base Point data
complete.profile <- read_csv("data_raw/ProfilesForErosion.csv", 
                            col_names = c("profile", "Park", "MHHW",
                                          "X_BasePoint", "Y_BasePoint", 
                                          "Start_Year", "Start_X", "Start_Y", "Start_Dist",
                                          "End_Year", "End_X", "End_Y", "End_Dist",
                                          "Total_Change", "Years", "Change_per_Year",
                                          "Hannah", "2050", "Comments"), 
                            skip = 3,  show_col_types = FALSE) %>%
  filter(profile == (str_extract_all(profile.pattern, "\\(?[0-9,.]+\\)?")[[1]])) %>%
  full_join(profiles.df, by = "profile") %>%
  select(profile, Park, X_BasePoint, Y_BasePoint, season:z) %>%
  drop_na()
complete.profile$year <- factor(complete.profile$year, levels =  c("97", "98", "99","00", "01", "02", "03",
                                                     "04", "05", "06", "07", "08", "09", "10",
                                                     "11", "12", "13", "14", "15", "16", "17",
                                                     "18", "19", "20", "21", "22"))


# (ARCHIVE) Extracted xy coordinates from graphical linear regression -----------------------

## Plot a single profile and year with a regression line. 
# single.lm.profile <- ggplot(data = complete.profile %>% filter(year %in% year.pattern),
#        aes(x = x, y = y, group = year)) +
#   facet_wrap(~year) +
#   geom_point() +
#   stat_smooth(method = lm, se = FALSE) +
#   ggtitle(paste("Profile:", profile.pattern, "Year:", year.pattern))
# single.lm.profile
# 
# # Extract xy coordinates for the above graph's recession line
# single.lm.df <- ggplot_build(single.lm.profile)$data[[2]][, c("x","y")]
# 
# # Quick linear model: find y at x position using extracted points
# linear.model <- lm(data = single.lm.df, y ~ x)
# intercept <- coef(linear.model)[1] # Intercept
# slope <- coef(linear.model)[2] # slope
# y_min <- (slope*min(single.lm.df$x)) + intercept
# y_max <- (slope*max(single.lm.df$x)) + intercept
# 
# # Plot the points using the extracted slope
# plot(single.lm.df$x, single.lm.df$y)
# points(x = min(single.lm.df$x), y = y_min, col = "red", pch = 20, cex = 3)
# points(x = max(single.lm.df$x), y = y_max, col = "blue", pch = 20, cex = 3)


# Use grouped linear model for a single profile/year  -----------------------

## Apply linear model to single profile and year
single.lm.df <- complete.profile %>%
  group_by(profile, year) %>%
  do(model = lm(y ~ x, data = .)) %>%
  mutate(intercept = coef(model)[1],
         slope = coef(model)[2]) %>%
  filter(year %in% year.pattern)

single.POI.df <- complete.profile %>%
  left_join(single.lm.df %>% select(-model), by = c("profile", "year")) %>%
  filter(year %in% year.pattern) %>%
  group_by(profile, year) %>%
  mutate(x_min = min(x),
         y_min = (slope*min(x)) + intercept) %>%
  mutate(x_max = max(x),
         y_max = (slope*max(x)) + intercept) %>%
  mutate(x_midpoint = ((x_min + x_max)/2),
         y_midpoint = (((slope*min(x)) + intercept) +
                         ((slope*max(x)) + intercept))/2) %>%
  mutate(x_quartile1 = ((min(x) + ((x_min + x_max)/2))/2),
         y_quartile1 = (((slope*min(x)) + intercept) + 
                          (((slope*min(x)) + intercept) + 
                             ((slope*max(x)) + intercept))/2)/2) %>%
  mutate(x_quartile3 = ((x_midpoint + max(x))/2),
         y_quartile3 = ((((((slope*min(x)) + intercept) + 
                             ((slope*max(x)) + intercept))/2)) + 
                          ((slope*max(x)) + intercept))/2)

single.POI.plot <- ggplot(data = single.POI.df) +
  facet_wrap(~year) +
  geom_point(aes(x = x, y = y), alpha = 0.5) +
  geom_point(aes(x = X_BasePoint, y = Y_BasePoint), color = "black", size = 5) + 
  geom_point(aes(x = x_min, y = y_min), color = "red", size = 3) +
  geom_point(aes(x = x_quartile1, y = y_quartile1), color = "orange", size = 3) +
  geom_point(aes(x = x_midpoint, y = y_midpoint), color = "green", size = 3) +
  geom_point(aes(x = x_quartile3, y = y_quartile3), color = "blue", size = 3) +
  geom_point(aes(x = x_max, y =  y_max), color = "purple", size = 3) +
  ggtitle(paste("Profile:", profile.pattern, "Year:", year.pattern))
single.POI.plot


## Acquire slope and intercept for all profiles/years
complete.lm.df <- complete.profile %>%
  group_by(profile, year) %>%
  do(model = lm(y ~ x, data = .)) %>%
  mutate(intercept = coef(model)[1],
         slope = coef(model)[2]) 

## Calculate min, quartile1, mid, quartile3, and max values for each profile and year
complete.POI.df <- complete.profile %>%
  left_join(complete.lm.df %>% select(-model), by = c("profile", "year")) %>%
  group_by(profile, year) %>%
  mutate(x_min = min(x),
         y_min = (slope*min(x)) + intercept) %>%
  mutate(x_max = max(x),
         y_max = (slope*max(x)) + intercept) %>%
  mutate(x_midpoint = ((x_min + x_max)/2),
         y_midpoint = (((slope*min(x)) + intercept) +
                         ((slope*max(x)) + intercept))/2) %>%
  mutate(x_quartile1 = ((min(x) + ((x_min + x_max)/2))/2),
         y_quartile1 = (((slope*min(x)) + intercept) + 
                          (((slope*min(x)) + intercept) + 
                             ((slope*max(x)) + intercept))/2)/2) %>%
  mutate(x_quartile3 = ((x_midpoint + max(x))/2),
         y_quartile3 = ((((((slope*min(x)) + intercept) + 
                             ((slope*max(x)) + intercept))/2)) + 
                          ((slope*max(x)) + intercept))/2) %>%
  drop_na()

# avgtest <- complete.POI.df %>%
#   select(profile, year, contains("_")) %>%
#   unique() %>%
#   rowwise() %>%
#   mutate(xavg = mean(x_min, x_max, x_quartile1, x_quartile3, x_midpoint)) %>%
#   mutate(yavg = mean(y_min, y_max, y_quartile1, y_quartile3, y_midpoint))

## Calculate euclidean distances between each point of interest and the base point.
euclidean.distances <- complete.POI.df %>% 
  select(profile, Park, year, everything(), -c(x, y, z, season, slope, intercept)) %>%
  unique() %>%
  ungroup() %>%
  mutate(min_euc_BP = sqrt(((X_BasePoint - x_min)^2) + ((Y_BasePoint -  y_min)^2))) %>%
  mutate(q1_euc_BP = sqrt(((X_BasePoint - x_quartile1)^2) + ((Y_BasePoint -  y_quartile1)^2))) %>%
  mutate(mid_euc_BP = sqrt(((X_BasePoint - x_midpoint)^2) + ((Y_BasePoint -  y_midpoint)^2))) %>%
  mutate(q3_euc_BP = sqrt(((X_BasePoint - x_quartile3)^2) + ((Y_BasePoint -  y_quartile3)^2))) %>%
  mutate(max_euc_BP = sqrt(((X_BasePoint - x_max)^2) + ((Y_BasePoint -  y_max)^2)))

ggplot(data = euclidean.distances, aes(x = year, y = min_euc_BP)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.5) +
  ggtitle(paste("Profile", profile.pattern))

## Calculate rates of change  
euclidean.rates <- euclidean.distances %>%
  mutate(min_rate = (min_euc_BP/lag(min_euc_BP) - 1) * 100) %>%
  mutate(min_rate = 100 * (min_euc_BP - lag(min_euc_BP))/lag(min_euc_BP)) %>%
  mutate(q1_rate = 100 * (q1_euc_BP - lag(q1_euc_BP))/lag(q1_euc_BP)) %>%
  mutate(mid_rate = 100 * (mid_euc_BP - lag(mid_euc_BP))/lag(mid_euc_BP)) %>%
  mutate(q3_rate = 100 * (q3_euc_BP - lag(q3_euc_BP))/lag(q3_euc_BP)) %>%
  mutate(max_rate = 100 * (max_euc_BP - lag(max_euc_BP))/lag(max_euc_BP))

## Change rates to dfs for plot
min_rate_df <- euclidean.rates %>%
  select(profile, year, rate = min_rate) %>%
  mutate(position = "min")

q1_rate_df <- euclidean.rates %>%
  select(profile, year, rate = q1_rate) %>%
  mutate(position = "q1")

mid_rate_df <- euclidean.rates %>%
  select(profile, year, rate = mid_rate) %>%
  mutate(position = "mid")

q3_rate_df <- euclidean.rates %>%
  select(profile, year, rate = q3_rate) %>%
  mutate(position = "q3")

max_rate_df <- euclidean.rates %>%
  select(profile, year, rate = max_rate) %>%
  mutate(position = "max")

mean_rate_df <- euclidean.rates %>%
  drop_na() %>%
  group_by(year) %>%
  mutate(mean_rate = mean(min_rate:max_rate)) %>%
  select(profile, year, rate = mean_rate) %>%
  mutate(position = "mean")

## Combine
euc.plot <- min_rate_df %>%
  rbind(q1_rate_df) %>%
  rbind(mid_rate_df) %>%
  rbind(q3_rate_df) %>%
  rbind(max_rate_df) %>%
  rbind(mean_rate_df) 

ggplot(data = euc.plot, aes(x = year, y = rate, fill = position)) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.5) +
  gghighlight(position == "mean") +
  ggtitle(paste("Profile", profile.pattern, "Rate of Change")) 
