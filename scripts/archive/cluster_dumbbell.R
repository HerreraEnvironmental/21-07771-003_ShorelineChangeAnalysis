# Visualize as a dumbbell plot --------------------------------------------------------

profile.pattern <- "prof"
year.pattern <- c("20")

source("scripts/src/load_packages.R")
source("scripts/src/assign_profile_parks.R")


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
  unique()


## Calculate euclidean distances between each point of interest and the base point.
euclidean.distances <- quartile.df %>% 
  select(profile, Park, year, everything(), -c(x, y, z, season, slope, intercept)) %>%
  unique() %>%
  ungroup() %>%
  mutate(west_west_dist = sqrt(((BasePoint_X - x_west_west)^2) + ((BasePoint_Y -  y_west_west)^2))) %>%
  mutate(east_east_dist = sqrt(((BasePoint_X - x_east_east)^2) + ((BasePoint_Y -  y_east_east)^2))) %>%
  group_by(profile) 


## Begin dumbbell plot work
dumbbell.df <- euclidean.distances %>%
  select(profile, Park, year, west_west_dist, east_east_dist) %>%
  unique() %>%
  mutate(segment_dist = east_east_dist - west_west_dist) %>%
  pivot_longer(cols = c(east_east_dist, west_west_dist)) %>% 
  rename(position = name,
         euclidean_distance = value) %>%
  left_join(df.clustered %>% 
              select(-Park) %>% 
              mutate(profile = as.numeric(profile)), by = "profile") 

landward.point <- dumbbell.df %>%
  filter(position == "east_east_dist")
seaward.point <- dumbbell.df %>%
  filter(position == "west_west_dist")
diff <- dumbbell.df %>% 
  mutate(x_pos = euclidean_distance + (segment_dist/2)) %>%
  filter(position == "west_west_dist")

## Create plot
dumbbell.plot <- ggplot(dumbbell.df) +
  geom_segment(data = landward.point,
               aes(x = euclidean_distance, y = profile,
                   yend = seaward.point$profile, 
                   xend = seaward.point$euclidean_distance),
               linewidth = 4.5,
               alpha = .2) +
  geom_point(aes(x = euclidean_distance, y = profile, color = factor(cluster_id)), 
             size = 4, show.legend = TRUE) +
  geom_text(data = diff, aes(label = paste("Location: ", Park, profile), 
                             x = x_pos, y = profile),
            size = 2.5) +
  scale_y_continuous(trans = "reverse") +
  scale_x_continuous(trans = "reverse") +
  ggtitle(paste("Cluster group"))
dumbbell.plot
