## RLionheart
## 21-0771-001
## January 2023
## Shoreline Conservation Areas, Washington State Parks


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
  group_by(profile, year)


## Euclidean distances
euclidean <- complete.profile %>%
  mutate(x_midpoint = ((min(x) + max(x))/2)) %>%
  mutate(y_midpoint = ((min(y) + max(y))/2)) %>%
  select(profile, year, X_BasePoint, Y_BasePoint, x_midpoint,  y_midpoint) %>%
  unique() %>%
  arrange(year) %>%
  group_by(profile) %>%
  mutate(euc_dist_to_BP = sqrt(((X_BasePoint - x_midpoint)^2) + ((Y_BasePoint -  y_midpoint)^2))) %>%
  # mutate(test = ifelse(levels(euclidean$year)[1], sqrt(((X_BasePoint - x_midpoint)^2) + ((Y_BasePoint -  y_midpoint)^2)),
  #                                ifelse(year == 21, TRUE, FALSE)))
  mutate(profile_slope = ((Y_BasePoint - y_midpoint) / (X_BasePoint - x_midpoint))) %>%
  #mutate(slope_dir = ifelse(profile_slope > 0, "positive", "negative"))
  mutate(slope_dir = "slope")
euclidean$year <- factor(euclidean$year, levels =  c("97", "98", "99","00", "01", "02", "03",
                                                     "04", "05", "06", "07", "08", "09", "10",
                                                     "11", "12", "13", "14", "15", "16", "17",
                                                     "18", "19", "20", "21", "22"))


## Visualize euclidean distance from average Euclidean distance of each year
midpoint.euc.dist.plot <- ggplot(euclidean %>% drop_na(), 
       aes(year, euc_dist_to_BP, fill=slope_dir, group = slope_dir)) +
  facet_wrap(~profile) +
  geom_col(position = position_dodge(width = 0.5)) +
  geom_line(aes(group = slope_dir), position = position_dodge(width = 1),
            linewidth = 1, color = "black") +
  geom_smooth(method = "lm", se = TRUE, color="blue") +
  theme(axis.text = element_blank()) +
  ggtitle(paste("Profile", profile.pattern, ": Midpoint Euclidean Distance from Base Point"))
midpoint.euc.dist.plot

