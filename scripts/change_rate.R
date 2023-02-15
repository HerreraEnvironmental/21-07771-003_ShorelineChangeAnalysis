## RLionheart
## 21-0771-001
## January 2023
## Shoreline Conservation Areas, Washington State Parks

source("scripts/load_packages.R")

profile.pattern <- "prof_6|prof_7|prof_8|prof_9"
source("scripts/import_profiles.R")

## Import erosion file for Base Point data
profile.erosion <- read_csv("data_raw/ProfilesForErosion.csv", 
                            col_names = c("profile", "Park", "MHHW",
                                          "BasePoint_X", "BasePoint_Y", 
                                          "Start_Year", "Start_X", "Start_Y", "Start_Dist",
                                          "End_Year", "End_X", "End_Y", "End_Dist",
                                          "Total_Change", "Years", "Change_per_Year",
                                          "Hannah", "2050", "Comments"), skip = 3, show_col_types = FALSE)

## Combine and add euclidean distance
complete.profile <- profile.erosion %>%
  full_join(profiles.df, by = "profile") %>%
  rowwise() %>%
  mutate(euc_dist = sqrt(((BasePoint_X - x)^2) + ((BasePoint_Y - y)^2))) %>%
  group_by(year) %>%
  mutate(euc_avg = mean(euc_dist))

## Prelim visual of data vs BasePoint
complete.visual <- complete.profile %>%
  filter(profile %in% c(6:9))
ggplot(data = complete.visual) +
  geom_point(aes(x = x, y = y, color = profile)) +
  geom_point(aes(x = BasePoint_X, y = BasePoint_Y), color = "black", size = 3) +
  ggtitle(paste("Profile:", profile.pattern))


# With color by year and best fit
partial.visual <- complete.visual %>%
  filter(profile == 6 & year %in% c(97)) # Add 18 %>%

ggplot(data = partial.visual) +
  geom_point(aes(x = x, y = y), alpha = 0.3) +
  geom_point(aes(x = BasePoint_X, y = BasePoint_Y), color = "black", size = 3) +
  geom_smooth(method=lm, se = FALSE, aes(x = x, y = y, color = year, weight = 2))


## Visualize euclidean distance from average Euclidean distance
miniframe <- complete.profile %>% 
  select(profile, year, euc_avg) %>%
  unique() %>%
  drop_na()
miniframe$year <- factor(miniframe$year, levels =  c("97", "98", "99","00", "01", "02", "03",
                                                     "04", "05", "06", "07", "08", "09", "10",
                                                     "11", "12", "13", "14", "15", "16", "17",
                                                     "18", "19", "20", "21", "22"))

ggplot(miniframe, aes(year, euc_avg)) +
  facet_wrap(~profile) +
  geom_bar(stat = "identity") +
  geom_smooth(method = "lm", weight = 3) +
  theme(legend.position = "none") +
  ggtitle("Average Euclidean Distance from Base Point")



                            