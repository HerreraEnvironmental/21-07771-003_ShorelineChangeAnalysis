## Dumbbell chart using geographic profiles, averaged to five years? 
## So 4 graphs showing geographic change over time, profile on y axis, 
## shoreline change on x axis


profile.pattern <- "prof"
year.pattern <- c("00", "01", "02", "03", "04", "05")

source("scripts/src/load_packages.R")
source("scripts/src/import_profiles.R")



## Import erosion file for Base Point data
complete.profile <- read_csv("data_raw/ProfilesForErosion.csv", 
                             col_names = c("profile", "Park", "MHHW",
                                           "X_BasePoint", "Y_BasePoint", 
                                           "Start_Year", "Start_X", "Start_Y", "Start_Dist",
                                           "End_Year", "End_X", "End_Y", "End_Dist",
                                           "Total_Change", "Years", "Change_per_Year",
                                           "Hannah", "2050", "Comments"), 
                             skip = 3,  show_col_types = FALSE) %>%
  full_join(profiles.df, by = "profile", multiple = "all") %>%
  select(profile, Park, X_BasePoint, Y_BasePoint, season:z) %>%
  drop_na()

## Geographic locations
profile.erosion <- read_csv("data_raw/ProfilesForErosion.csv", 
                            col_names = c("profile", "Park"), 
                            col_select = (1:2),
                            skip = 3, show_col_types = FALSE)
profile.OBA <- read_csv("data_raw/OBAProfiles.csv", 
                        col_names = c("OBA", "profile", "Notes"), 
                        col_select = c("profile", "OBA", "Notes"),
                        skip = 1, show_col_types = FALSE) %>%
  separate_longer_delim(profile, ",") %>%
  mutate(profile = as.numeric(gsub(" ", "", profile)))

complete.geo.profiles <- profile.OBA %>% 
  full_join(profile.erosion, by = "profile") %>%
  arrange(profile)


## Apply linear model 
linear.model.df <- complete.profile %>%
  filter(year %in% year.pattern) %>%
  group_by(profile, year) %>%
  do(model = lm(y ~ x, data = .)) %>%
  mutate(intercept = coef(model)[1],
         slope = coef(model)[2]) 

quartile.df <- complete.profile %>%
  left_join(linear.model.df %>% select(-model), by = c("profile", "year")) %>%
  filter(year %in% year.pattern) %>%
  group_by(profile, year) %>%
  mutate(x_west_west = min(x),
         y_west_west = (slope*min(x)) + intercept) %>%
  mutate(x_east_east = max(x),
         y_east_east = (slope*max(x)) + intercept) %>%
  mutate(x_midpoint = ((x_west_west + x_east_east)/2),
         y_midpoint = (((slope*min(x)) + intercept) +
                         ((slope*max(x)) + intercept))/2) %>%
  mutate(x_west = ((min(x) + ((x_west_west + x_east_east)/2))/2),
         y_west = (((slope*min(x)) + intercept) + 
                     (((slope*min(x)) + intercept) + 
                        ((slope*max(x)) + intercept))/2)/2) %>%
  mutate(x_east = ((x_midpoint + max(x))/2),
         y_east = ((((((slope*min(x)) + intercept) + 
                        ((slope*max(x)) + intercept))/2)) + 
                     ((slope*max(x)) + intercept))/2) %>%
  unique()

quartiles.plot <- ggplot(data = quartile.df %>% filter(year %in% year.pattern)) +
  facet_wrap(~year) +
  geom_point(aes(x = x, y = y), alpha = 0.5) +
  geom_point(aes(x = X_BasePoint, y = Y_BasePoint), color = "red", size = 5) + 
  geom_point(aes(x = x_west_west, y = y_west_west), color = "darkred", size = 3) +
  geom_point(aes(x = x_west, y = y_west), color = "orange", size = 3) +
  geom_point(aes(x = x_midpoint, y = y_midpoint), color = "green", size = 3) +
  geom_point(aes(x = x_east, y = y_east), color = "blue", size = 3) +
  geom_point(aes(x = x_east_east, y =  y_east_east), color = "purple", size = 3) +
  ggtitle(paste("Profile:", profile.pattern, "Year:", year.pattern))
#quartiles.plot

## Calculate euclidean distances between each point of interest and the base point.
euclidean.distances <- quartile.df %>% 
  select(profile, Park, year, everything(), -c(x, y, z, season, slope, intercept)) %>%
  unique() %>%
  ungroup() %>%
  mutate(west_west_dist = sqrt(((X_BasePoint - x_west_west)^2) + ((Y_BasePoint -  y_west_west)^2))) %>%
  mutate(west_dist = sqrt(((X_BasePoint - x_west)^2) + ((Y_BasePoint -  y_west)^2))) %>%
  mutate(midpoint_dist = sqrt(((X_BasePoint - x_midpoint)^2) + ((Y_BasePoint -  y_midpoint)^2))) %>%
  mutate(east_dist = sqrt(((X_BasePoint - x_east)^2) + ((Y_BasePoint -  y_east)^2))) %>%
  mutate(east_east_dist = sqrt(((X_BasePoint - x_east_east)^2) + ((Y_BasePoint -  y_east_east)^2))) %>%
  group_by(profile) %>%
  mutate(west_west_dist_avg = mean(west_west_dist),
         east_east_dist_avg = mean(east_east_dist))


## Begin dumbbell plot work
dumbbell.df <- euclidean.distances %>%
  # select(profile, Park, year, west_west_dist, east_east_dist) %>% ## non averaged
  select(profile, Park, west_west_dist_avg, east_east_dist_avg) %>% ## averaged %>%
  unique() %>%
  mutate(segment_dist = east_east_dist_avg - west_west_dist_avg) %>%
  pivot_longer(cols = c(east_east_dist_avg, west_west_dist_avg)) %>% 
  rename(position = name,
         euclidean_distance = value)

landward.point <- dumbbell.df %>%
  filter(position == "east_east_dist_avg")
seaward.point <- dumbbell.df %>%
  filter(position == "west_west_dist_avg")
diff <- dumbbell.df %>% 
  mutate(x_pos = euclidean_distance + (segment_dist/2)) %>%
  filter(position == "west_west_dist_avg")

## Create plot
dumbbell.plot <- ggplot(dumbbell.df)+
  geom_segment(data = landward.point,
               aes(x = euclidean_distance, y = profile,
                   yend = seaward.point$profile, 
                   xend = seaward.point$euclidean_distance),
               color = "#aeb6bf",
               linewidth = 4.5,
               alpha = .5) +
  geom_point(aes(x = euclidean_distance, y = profile, color = position), 
             size = 4, show.legend = TRUE) +
  geom_text(data = diff, aes(label = paste("Location: ", Park), 
                x = x_pos, y = profile),
            color = "#4a4e4d",
            size = 2.5) +
  scale_y_continuous(trans = "reverse") +
  ggtitle(paste("Landward and Seaward Point Migration: Year", year.pattern))
dumbbell.plot
