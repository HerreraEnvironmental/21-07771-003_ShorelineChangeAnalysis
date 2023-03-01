## RLionheart
## 21-0771-001
## February 
## Shoreline Conservation Areas, Washington State Parks


CalculateChangeRates <- function(x, y) {
  xval = (x - lag(x))/lag(x)
  yval = (y - lag(y))/lag(y)
  rate = xval/yval
  
  return(rate)
}


## Take n Euclidean points along profiles and use that as the change rate.

profile.pattern <- "prof_6"
year.pattern <- c("00", "17")

source("scripts/load_packages.R")
source("scripts/import_profiles.R")

## Import erosion file for Base Point data
profile.erosion <- read_csv("data_raw/ProfilesForErosion.csv", 
                            col_names = c("profile", "Park", "MHHW",
                                          "X_BasePoint", "Y_BasePoint", 
                                          "Start_Year", "Start_X", "Start_Y", "Start_Dist",
                                          "End_Year", "End_X", "End_Y", "End_Dist",
                                          "Total_Change", "Years", "Change_per_Year",
                                          "Hannah", "2050", "Comments"), 
                            skip = 3,  show_col_types = FALSE) %>%
  filter(profile == (str_extract_all(profile.pattern, "\\(?[0-9,.]+\\)?")[[1]]))

## Combine and add euclidean distance
complete.profile <- profile.erosion %>%
  full_join(profiles.df, by = "profile") %>%
  select(profile, Park, X_BasePoint, Y_BasePoint, season:z) %>%
  group_by(profile, year) %>% # probably not season?
  mutate(x_min = min(x),
         y_min = min(y)) %>%
  mutate(x_max = max(x),
         y_max = max(y)) %>%
  mutate(x_midpoint = ((min(x) + max(x))/2),
         y_midpoint = ((min(y) + max(y))/2)) %>%
  mutate(x_quartile1 = ((min(x) + x_midpoint)/2),
         y_quartile1 = ((min(y) + y_midpoint)/2)) %>%
  mutate(x_quartile3 = ((x_midpoint + max(x))/2),
         y_quartile3 = ((y_midpoint + max(y))/2)) 
complete.profile$year <- factor(complete.profile$year, levels =  c("97", "98", "99","00", "01", "02", "03",
                                                                   "04", "05", "06", "07", "08", "09", "10",
                                                                   "11", "12", "13", "14", "15", "16", "17",
                                                                   "18", "19", "20", "21", "22"))

## Plot
ggplot(data = complete.profile %>% filter(year %in% year.pattern)) +
  facet_wrap(~year) +
  geom_point(aes(x = x, y = y), alpha = 0.5) +
  geom_point(aes(x = x_min, y = y_min), color = "red", size = 3) +
  geom_point(aes(x = x_quartile1, y = y_quartile1), color = "orange", size = 3) +
  geom_point(aes(x = x_midpoint, y = y_midpoint), color = "green", size = 3) +
  geom_point(aes(x = x_quartile3, y = y_quartile3), color = "blue", size = 3) +
  geom_point(aes(x = x_max, y =  y_max), color = "purple", size = 3) +
  ggtitle(paste("Profile:", profile.pattern, "Year:", year.pattern))

# Change rates?...
change <- complete.profile %>% 
  select(profile, Park, year, everything(), -c(x, y, z, season)) %>%
  unique() %>%
  group_by(profile) %>% 
  mutate(min_rate = CalculateChangeRates(x_min, y_min)) %>%
  mutate(quart1_rate = CalculateChangeRates(x_quartile1, y_quartile1)) %>%
  mutate(mid_rate = CalculateChangeRates(x_midpoint, y_midpoint)) %>%
  mutate(quart3_rate = CalculateChangeRates(x_quartile3, y_quartile3)) %>%
  mutate(max_rate = CalculateChangeRates(x_max, y_max))

MakeRateDF <- function(df, description) {
  rate_df <- df %>%
    select(profile, year, rate = paste(description, "_rate", sep = "")) %>%
    mutate(position = description)
  
  return(rate_df)
}

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
