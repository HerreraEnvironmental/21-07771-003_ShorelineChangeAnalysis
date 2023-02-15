library(tidyverse)

source("scripts/import_profiles.R") # Imports prof.df


profile.erosion <- read_csv("data_raw/ProfilesForErosion.csv", 
                            col_names = c("profile", "Park", "MHHW",
                                          "BasePoint_X", "BasePoint_Y", 
                                          "Start_Year", "Start_X", "Start_Y", "Start_Dist",
                                          "End_Year", "End_X", "End_Y", "End_Dist",
                                          "Total_Change", "Years", "Change_per_Year",
                                          "Hannah", "2050", "Comments"), skip = 3, show_col_types = FALSE) %>%
  filter(profile %in% c(6))

complete.profile <- profile.erosion %>%
  left_join(prof.df, by = "profile") %>%
  rowwise() %>%
  mutate(euc_dist = sqrt((BasePoint_X - x)^2) + ((BasePoint_Y - y)^2))
  

# Visual layout of BP, Start, End
ggplot(data = complete.profile) +
  geom_point(aes(x = BasePoint_X, y = BasePoint_Y), color = "black", size = 2) +
  geom_point(aes(x = x, y = y), color = "red") 

ggplot(data = complete.profile) +
  geom_line(aes(x = euc_dist, y = z))


# Make some distance matrices!
mydist <- complete.profile %>%
  select(profile, BasePoint_X, BasePoint_Y, x, y) %>%
  rowwise() %>%
  mutate(euc_dist = sqrt((BasePoint_X - x)^2) + ((BasePoint_Y - y)^2))

                            