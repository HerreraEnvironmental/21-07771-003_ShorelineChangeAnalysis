## Dumbbell chart using geographic profiles, averaged to five years? 
## So 4 graphs showing geographic change over time, profile on y axis, 
## shoreline change on x axis


profile.pattern <- "prof_6|prof_7|prof_8|prof_9"
year.pattern <- c("00")

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
  filter(profile == (str_extract_all(profile.pattern, "\\(?[0-9,.]+\\)?")[[1]])) %>%
  full_join(profiles.df, by = "profile", multiple = "all") %>%
  select(profile, Park, X_BasePoint, Y_BasePoint, season:z) %>%
  drop_na()

## Apply linear model 
linear.model.df <- complete.profile %>%
  filter(year == year.pattern) %>%
  group_by(profile, year) %>%
  do(model = lm(y ~ x, data = .)) %>%
  mutate(intercept = coef(model)[1],
         slope = coef(model)[2]) 

quartile.df <- complete.profile %>%
  left_join(linear.model.df %>% select(-model), by = c("profile", "year")) %>%
  filter(year == year.pattern) %>%
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
quartiles.plot

## Calculate euclidean distances between each point of interest and the base point.
euclidean.distances <- quartile.df %>% 
  select(profile, Park, year, everything(), -c(x, y, z, season, slope, intercept)) %>%
  unique() %>%
  ungroup() %>%
  mutate(west_west_dist = sqrt(((X_BasePoint - x_west_west)^2) + ((Y_BasePoint -  y_west_west)^2))) %>%
  mutate(west_dist = sqrt(((X_BasePoint - x_west)^2) + ((Y_BasePoint -  y_west)^2))) %>%
  mutate(midpoint_dist = sqrt(((X_BasePoint - x_midpoint)^2) + ((Y_BasePoint -  y_midpoint)^2))) %>%
  mutate(east_dist = sqrt(((X_BasePoint - x_east)^2) + ((Y_BasePoint -  y_east)^2))) %>%
  mutate(east_east_dist = sqrt(((X_BasePoint - x_east_east)^2) + ((Y_BasePoint -  y_east_east)^2)))


## Begin dumbbell plot work
dumbbell.df <- euclidean.distances %>%
  select(profile, Park, year, west_west_dist, east_east_dist) %>% 
  mutate(segment_dist = east_east_dist - west_west_dist) %>%
  pivot_longer(cols = c(east_east_dist, west_west_dist)) %>% 
  rename(position = name,
         euclidean_distance = value)

landward.point <- dumbbell.df %>%
  filter(position == "east_east_dist")
seaward.point <- dumbbell.df %>%
  filter(position == "west_west_dist")

dumbbell.plot <- ggplot(dumbbell.df)+
  geom_segment(data = landward.point,
               aes(x = euclidean_distance, y = profile,
                   yend = seaward.point$profile, xend = seaward.point$euclidean_distance), #use the $ operator to fetch data from our "Females" tibble
               color = "#aeb6bf",
               size = 4.5, #Note that I sized the segment to fit the points
               alpha = .5) +
  
  geom_point(aes(x = euclidean_distance, y = profile, color = position), size = 4, show.legend = TRUE)+
  ggtitle("Landward and Seaward Point Migration")
dumbbell.plot
