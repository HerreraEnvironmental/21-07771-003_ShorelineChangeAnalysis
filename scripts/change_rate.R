library(tidyverse)

source("scripts/import_profiles.R") # Imports prof.df


profile.erosion <- read_csv("data_raw/ProfilesForErosion.csv", 
                            col_names = c("profile", "Park", "MHHW",
                                          "BasePoint_X", "BasePoint_Y", 
                                          "Start_Year", "Start_X", "Start_Y", "Start_Dist",
                                          "End_Year", "End_X", "End_Y", "End_Dist",
                                          "Total_Change", "Years", "Change_per_Year",
                                          "Hannah", "2050", "Comments"), skip = 3, show_col_types = FALSE) %>%
  filter(profile %in% c(17))

complete.profile <- profile.erosion %>%
  left_join(prof.df, by = "profile") %>%
 # filter(year %in% c("97", "00", "05", "10", "18")) %>%
  rowwise() %>%
  mutate(euc_dist = sqrt(((BasePoint_X - x)^2) + ((BasePoint_Y - y)^2))) %>%
  group_by(year) %>%
  mutate(euc_avg = mean(euc_dist))
  

# With color by season
ggplot(data = complete.profile) +
  geom_point(aes(x = x, y = y), alpha = 0.1) +
  geom_point(aes(x = BasePoint_X, y = BasePoint_Y), color = "black", size = 5) +
  geom_smooth(method=lm, se = FALSE, aes(x = x, y = y, color = year, weight = 2)) 
  
miniframe <- complete.profile %>% select(profile, year, euc_avg) %>% unique()  
miniframe$year <- factor(miniframe$year, levels =  c("97", "98", "99","00", "01", "02", "03",
                                                     "04", "05", "06", "07", "08", "09", "10",
                                                     "11", "12", "13", "14", "15", "16", "17",
                                                     "18", "19", "20", "21", "22"))

ggplot(miniframe, aes(year, euc_avg)) +
  geom_col()



                            