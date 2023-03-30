## RLionheart
## 21-0771-001
## January 2023
## Shoreline Conservation Areas, Washington State Parks

profile.pattern <- "prof_22|prof_23"
source("scripts/src/import_profiles.R")

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
  full_join(profiles.df, by = "profile", multiple = "all") %>%
  select(profile, Park, X_BasePoint, Y_BasePoint, season:z) %>%
  group_by(profile, year)


## Euclidean distances
euclidean <- complete.profile %>%
  mutate(x_midpoint = ((min(x) + max(x))/2)) %>%
  mutate(y_midpoint = ((min(y) + max(y))/2)) %>%
  select(profile, year, X_BasePoint, Y_BasePoint, x_midpoint,  y_midpoint) %>%
  unique() %>%
  arrange(profile, year) %>%
  group_by(profile) %>%
  mutate(euc_dist_to_BP = sqrt(((X_BasePoint - x_midpoint)^2) + ((Y_BasePoint -  y_midpoint)^2))) 

myslope <- euclidean %>%
  drop_na() %>%
  filter(rank(year) == 1|rank(year) == max(rank(year))) %>%
  group_by(profile) %>%
  mutate(profile_slope = ifelse(euc_dist_to_BP[year == 21] > euc_dist_to_BP[year == 97],
                                "positive", "negative")) %>%
  select(profile, profile_slope)

euclidean <- euclidean %>%
  left_join(myslope, by = "profile")
euclidean$year <- factor(euclidean$year, levels =  c("97", "98", "99","00", "01", "02", "03",
                                                     "04", "05", "06", "07", "08", "09", "10",
                                                     "11", "12", "13", "14", "15", "16", "17",
                                                     "18", "19", "20", "21", "22"))


## Visualize euclidean distance from average Euclidean distance of each year
midpoint.euc.dist.plot <- ggplot(euclidean %>% drop_na(), 
       aes(year, euc_dist_to_BP, fill=profile_slope, group = profile_slope)) +
  facet_wrap(~profile) +
  geom_col(position = position_dodge(width = 0.5)) +
  geom_line(aes(group = profile_slope), position = position_dodge(width = 1),
            linewidth = 1, color = "black") +
  geom_smooth(method = "lm", se = TRUE, color="blue") +
  theme(axis.text = element_blank()) +
  ggtitle(paste("Profile", profile.pattern, ": Midpoint Euclidean Distance from Base Point"))
midpoint.euc.dist.plot

