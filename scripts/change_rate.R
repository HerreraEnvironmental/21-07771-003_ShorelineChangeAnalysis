## RLionheart
## 21-0771-001
## January 2023
## Shoreline Conservation Areas, Washington State Parks

source("scripts/load_packages.R")

profile.pattern <- "prof_16|prof_17"
source("scripts/import_profiles.R")

## Import erosion file for Base Point data
profile.erosion <- read_csv("data_raw/ProfilesForErosion.csv", 
                            col_names = c("profile", "Park", "MHHW",
                                          "BasePoint_X", "BasePoint_Y", 
                                          "Start_Year", "Start_X", "Start_Y", "Start_Dist",
                                          "End_Year", "End_X", "End_Y", "End_Dist",
                                          "Total_Change", "Years", "Change_per_Year",
                                          "Hannah", "2050", "Comments"), 
                            skip = 3, show_col_types = FALSE)

## Combine and add euclidean distance
complete.profile <- profile.erosion %>%
  full_join(profiles.df, by = "profile") %>%
  select(profile, Park, BasePoint_X, BasePoint_Y, season:z) %>%
  rowwise() %>%
  mutate(euc_dist = sqrt(((BasePoint_X - x)^2) + ((BasePoint_Y - y)^2))) %>%
  group_by(profile, year, season) %>% # can probably drop season
  mutate(X_midpoint = ((min(x) + max(x))/2)) %>%
  mutate(Y_midpoint = ((min(y) + max(y))/2)) %>%
  mutate(euc_avg = mean(euc_dist))

## Prelim visual of data vs BasePoint
g <- ggplot(data = complete.profile) +
  geom_point(aes(x = x, y = y, color = profile)) +
  geom_point(aes(x = BasePoint_X, y = BasePoint_Y), color = "black", size = 3) +
  ggtitle(paste("Profile:", profile.pattern))
g

# With color by year and best fit
partial.visual <- complete.profile %>%
  filter(profile %in% c(16, 17))

ggplot(data = partial.visual) +
  geom_point(aes(x = x, y = y), alpha = 0.2) +
  geom_point(aes(x = BasePoint_X, y = BasePoint_Y), color = "black", size = 3) +
  geom_point(aes(x = X_midpoint, y = Y_midpoint), color = "blue", size = 3) +
  #geom_smooth(method=lm, se = FALSE, aes(x = x, y = y, color = year, weight = 2)) +
  ggtitle(paste("Profile:", profile.pattern))

## Euclidean distances
euclidean <- partial.visual %>%
  ungroup() %>%
  select(profile, year, BasePoint_X, BasePoint_Y, X_midpoint, Y_midpoint) %>%
  group_by(profile, year) %>%
  mutate(X_midpoint = mean(X_midpoint),
         Y_midpoint = mean(Y_midpoint)) %>%
  unique() %>%
  mutate(euc_dist = sqrt(((BasePoint_X - X_midpoint)^2) + ((BasePoint_Y - Y_midpoint)^2)))
euclidean$year <- factor(euclidean$year, levels =  c("97", "98", "99","00", "01", "02", "03",
                                                     "04", "05", "06", "07", "08", "09", "10",
                                                     "11", "12", "13", "14", "15", "16", "17",
                                                     "18", "19", "20", "21", "22"))

## Visualize euclidean distance from average Euclidean distance of each year
ggplot(euclidean, aes(year, euc_dist)) +
  facet_wrap(~profile) +
  geom_bar(stat = "identity") +
  geom_smooth(method = "lm", weight = 3) +
  theme(legend.position = "none") +
  ggtitle(paste("Profile", profile.pattern, ": Midpoint Euclidean Distance from Base Point"))

ggplot(euclidean, aes(year, euc_dist, group = profile, color = factor(profile))) +
  geom_point() +
  geom_line()

                            