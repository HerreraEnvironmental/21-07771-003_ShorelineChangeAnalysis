## RLionheart
## 21-0771-001
## January 2023
## Shoreline Conservation Areas, Washington State Parks

source("scripts/load_packages.R")

## Would need to interpolate Base Point data from John's CAD to get a complete picture.
## The quality of data is variable- does midpoint euclidean gloss over the data too much?

#profile.pattern <- "prof_6|prof_7|prof_8|prof_9|prof_17|prof_41"
profile.pattern <- "prof_6"
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

# With color by year and euclidean midpoint
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
  mutate(profile_slope = ((Y_BasePoint -  y_midpoint) / (X_BasePoint - x_midpoint))) %>%
  mutate(slope_dir = ifelse(profile_slope > 0, "positive", "negative"))
euclidean$year <- factor(euclidean$year, levels =  c("97", "98", "99","00", "01", "02", "03",
                                                     "04", "05", "06", "07", "08", "09", "10",
                                                     "11", "12", "13", "14", "15", "16", "17",
                                                     "18", "19", "20", "21", "22"))

## Visualize euclidean distance from average Euclidean distance of each year
ggplot(euclidean %>% drop_na(), 
       aes(year, euc_dist_to_BP, fill=slope_dir, group = slope_dir)) +
  facet_wrap(~profile, scales = "free") +
  geom_col(position = position_dodge(width = 0.5)) +
  # geom_line(aes(group = slope_dir), position = position_dodge(width = 1),
  #           size = 1, color = "black") +
  geom_smooth(method = "lm", se = TRUE, color="red") +
  theme(axis.text = element_blank()) +
  ggtitle(paste("Profile", profile.pattern, ": Midpoint Euclidean Distance from Base Point"))

## Rate of change
change <- euclidean %>% 
  group_by(profile) %>% 
  mutate(rate = 100 * (euc_dist_to_BP - lag(euc_dist_to_BP))/lag(euc_dist_to_BP)) %>%
  ungroup() %>%
  filter(profile == 41)


ggplot(change, mapping=aes(x=year, y=rate)) +
  geom_col()

## Dumbbell graph... useful?
dumbbell <- complete.profile %>%
  select(-x_midpoint, -y_midpoint) %>%
  filter(year == 97 | year == 18) 
t <- setDT(dumbbell)[ , .SD[which.max(x)], by = profile]
t <- setDT(dumbbell)[ , .SD[which.max(x)], by = profile]
