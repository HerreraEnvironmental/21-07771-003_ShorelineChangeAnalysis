## RLionheart
## 21-0771-001
## January 2023
## Shoreline Conservation Areas, Washington State Parks

source("scripts/load_packages.R")

## Would need to interpolate Base Point data from John's CAD to get a complete picture.
## The quality of data is variable- does midpoint euclidean gloss over the data too much?

#profile.pattern <- "prof_6|prof_7|prof_8|prof_9|prof_17|prof_41"
profile.pattern <- "prof"
source("scripts/import_profiles.R")

## Import erosion file for Base Point data
profile.erosion <- read_csv("data_raw/ProfilesForErosion.csv", 
                            col_names = c("profile", "Park", "MHHW",
                                          "X_BasePoint", "Y_BasePoint", 
                                          "Start_Year", "Start_X", "Start_Y", "Start_Dist",
                                          "End_Year", "End_X", "End_Y", "End_Dist",
                                          "Total_Change", "Years", "Change_per_Year",
                                          "Hannah", "2050", "Comments"), 
                            skip = 3, show_col_types = FALSE)

## Combine and add euclidean distance
complete.profile <- profile.erosion %>%
  full_join(profiles.df, by = "profile") %>%
  select(profile, Park, X_BasePoint, Y_BasePoint, season:z) %>%
  group_by(profile, year) %>%
  mutate(x_midpoint = ((min(x) + max(x))/2)) %>%
  mutate(y_midpoint = ((min(y) + max(y))/2))
  
  
## Prelim visual of data vs BasePoint
g <- ggplot(data = complete.profile %>% group_by(profile, year) %>% slice(750)) +
  geom_point(aes(x = x, y = y)) +
  geom_point(aes(x = X_BasePoint, y = Y_BasePoint), color = "red", size = 3) +
  ggtitle(paste("Profile:", profile.pattern))
g

# With color by year and best fit
partial.visual <- complete.profile %>%
  filter(profile %in% c(6))

ggplot(data = partial.visual %>% group_by(profile, year)) +
  geom_point(aes(x = x, y = y), alpha = 0.5) +
  geom_point(aes(x = X_BasePoint, y = Y_BasePoint), color = "red", size = 3) +
  geom_point(aes(x = x_midpoint, y =  y_midpoint), color = "blue", size = 3) +
  ggtitle(paste("Profile:", profile.pattern))

## Euclidean distances
euclidean <- complete.profile %>%
  select(profile, year, X_BasePoint, Y_BasePoint, x_midpoint,  y_midpoint) %>%
  unique() %>%
  group_by(profile) %>%
  mutate(euc_dist_to_BP = sqrt(((X_BasePoint - x_midpoint)^2) + ((Y_BasePoint -  y_midpoint)^2))) %>%
  #mutate(profile_slope = ((Y_BasePoint -  y_midpoint) / (X_BasePoint - x_midpoint))) %>%
  #mutate(slope_dir = ifelse(profile_slope > 0, "positive", "negative"))
euclidean$year <- factor(euclidean$year, levels =  c("97", "98", "99","00", "01", "02", "03",
                                                     "04", "05", "06", "07", "08", "09", "10",
                                                     "11", "12", "13", "14", "15", "16", "17",
                                                     "18", "19", "20", "21", "22"))

## Visualize euclidean distance from average Euclidean distance of each year
ggplot(euclidean %>% drop_na(), 
       aes(year, euc_dist_to_BP)) + #, fill=slope_dir, group = slope_dir)) +
  facet_wrap(~profile, scales = "free") +
  geom_col(position = position_dodge(width = 0.5)) +
  # geom_line(aes(group = slope_dir), position = position_dodge(width = 1),
  #           size = 1, color = "black") +
  geom_smooth(method = "lm", se = TRUE, color="red") +
  theme(axis.text = element_blank()) +
  ggtitle(paste("Profile", profile.pattern, ": Midpoint Euclidean Distance from Base Point"))


#### CHECK OUT LATER
# library(healthyR.ts)
# data_tbl <- ts_to_tbl(AirPassengers) |>
#   select(-index)
# output <- ts_ma_plot(
#   .data = data_tbl,
#   .date_col = date_col,
#   .value_col = value
# )
# output$pgrid
# output$xts_plt

# Negative slope test
# neg <- complete.profile %>%
#   filter(profile == 6 | profile == 41) %>% 
#   select(profile, year, x, y) %>%
#   group_by(profile) %>%
#   mutate(profile_slope = ((max(y) - min(y)) / (max(x) - min(x)))) %>%
#   mutate(slope_dir = ifelse(profile_slope > 0, "positive", "negative"))
# 
# ggplot(neg, aes(x = x, y = y)) +
#   facet_wrap(~ profile, scales = "free") +
#   geom_point() +
#   geom_smooth(method = "lm", se = TRUE, color="red")

                            