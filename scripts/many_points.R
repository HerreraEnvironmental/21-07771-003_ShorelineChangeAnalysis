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

profile.pattern <- "prof_6"
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
         y_midpoint = (((slope*min(x)) + intercept) + ((slope*max(x)) + intercept))/2) %>%
  mutate(x_quartile1 = ((min(x) + ((x_min + x_max)/2))/2),
         y_quartile1 = (((slope*min(x)) + intercept) + (((slope*min(x)) + intercept) + ((slope*max(x)) + intercept))/2)/2) %>%
  
  mutate(x_quartile3 = ((x_midpoint + max(x))/2),
         y_quartile3 = ((((((slope*min(x)) + intercept) + ((slope*max(x)) + intercept))/2)) + ((slope*max(x)) + intercept))/2)

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

# Change rates?...
change <- points.of.interest.df %>% 
  select(profile, Park, year, everything(), -c(x, y, z, season)) %>%
  #select(-x, -y) %>%
  unique() %>%
  group_by(profile) %>% 
  mutate(min_rate = CalculateChangeRates(x_min, y_min)) %>%
  mutate(quart1_rate = CalculateChangeRates(x_quartile1, y_quartile1)) %>%
  mutate(mid_rate = CalculateChangeRates(x_midpoint, y_midpoint)) %>%
  mutate(quart3_rate = CalculateChangeRates(x_quartile3, y_quartile3)) %>%
  mutate(max_rate = CalculateChangeRates(x_max, y_max))


min_df <- MakeRateDF(change, "min")
quart1_df <- MakeRateDF(change, "quart1")
mid_df <- MakeRateDF(change, "mid")
quart3_df <- MakeRateDF(change, "quart3")
max_df <- MakeRateDF(change, "max")
  
change.plot <- min_df %>%
  rbind(quart1_df) %>%
  rbind(mid_df) %>%
  rbind(quart3_df) %>%
  rbind(max_df) 
  

ggplot(change.plot, aes(x=year, y=rate, fill = position)) +
  geom_col(position = "dodge", stat = "identity") +
  ggtitle(paste("Profile", profile.pattern))
