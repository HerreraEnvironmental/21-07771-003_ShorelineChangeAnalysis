## RLionheart
## 21-0771-001
## January 2023
## Shoreline Conservation Areas, Washington State Parks

#source("scripts/load_packages.R")

## Would need to interpolate Base Point data from John's CAD to get a complete picture.
## The quality of data is variable- does midpoint euclidean gloss over the data too much?

#profile.pattern <- "prof_6|prof_7|prof_8|prof_9|prof_17|prof_41"
# profile.pattern <- "prof_6"
# source("scripts/import_profiles.R")

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
all.basepoint.plot <- ggplot(data = complete.profile %>% group_by(profile, year) %>% slice(750)) +
  geom_point(aes(x = x, y = y)) +
  geom_point(aes(x = X_BasePoint, y = Y_BasePoint), color = "red", size = 3) +
  ggtitle(paste("Profile:", profile.pattern))
all.basepoint.plot

# Zoom in on a specific profile to see what's happening
partial.visual <- complete.profile %>%
  filter(profile %in% c(6) & year == "98")

single.basepoint.plot <- ggplot(data = partial.visual %>% group_by(profile, year)) +
  geom_point(aes(x = x, y = y), alpha = 0.5) +
  geom_point(aes(x = X_BasePoint, y = Y_BasePoint), color = "red", size = 3) +
  ggtitle("Profile 6")
single.basepoint.plot
