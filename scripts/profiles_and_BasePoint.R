## RLionheart
## 21-0771-001
## January 2023
## Shoreline Conservation Areas, Washington State Parks

#source("scripts/load_packages.R")

## Would need to interpolate Base Point data from John's CAD to get a complete picture.
## The quality of data is variable- does midpoint euclidean gloss over the data too much?

profile.pattern <- "prof_6|prof_7|prof_8|prof_9"
source("scripts/src/import_profiles2.R")
source("scripts/src/assign_profile_parks.R")


## Combine and add euclidean distance
profile.midpoints <- complete.profile %>%
  select(profile, BasePoint_X, BasePoint_Y, season:z) %>%
  group_by(profile, year) %>%
  mutate(x_midpoint = ((min(x) + max(x))/2)) %>%
  mutate(y_midpoint = ((min(y) + max(y))/2))
  
  
## Prelim visual of data vs BasePoint
all.basepoint.plot <- ggplot(data = profile.midpoints %>% group_by(profile, year) %>% slice(750)) +
  geom_point(aes(x = x, y = y)) +
  geom_point(aes(x = BasePoint_X, y = BasePoint_Y), color = "red", size = 3) +
  geom_line(aes(x = BasePoint_X, y = BasePoint_Y), color = "red", linewidth = 1) +
  ggtitle(paste("Profile:", profile.pattern))
all.basepoint.plot

# Zoom in on a specific profile to see what's happening
partial.visual <- profile.midpoints %>%
  filter(profile %in% c(6) & year == "98")

single.basepoint.plot <- ggplot(data = partial.visual %>% group_by(profile, year)) +
  geom_point(aes(x = x, y = y), alpha = 0.5) +
  geom_point(aes(x = BasePoint_X, y = BasePoint_Y), color = "red", size = 3) +
  ggtitle("Profile 6")
single.basepoint.plot

