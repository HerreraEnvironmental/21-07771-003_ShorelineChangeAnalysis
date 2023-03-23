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
  mutate(x_min = min(x),
         y_min = (slope*min(x)) + intercept) %>%
  mutate(x_max = max(x),
         y_max = (slope*max(x)) + intercept) %>%
  mutate(x_midpoint = ((x_min + x_max)/2),
         y_midpoint = (((slope*min(x)) + intercept) +
                         ((slope*max(x)) + intercept))/2) %>%
  mutate(x_quartile1 = ((min(x) + ((x_min + x_max)/2))/2),
         y_quartile1 = (((slope*min(x)) + intercept) + 
                          (((slope*min(x)) + intercept) + 
                             ((slope*max(x)) + intercept))/2)/2) %>%
  mutate(x_quartile3 = ((x_midpoint + max(x))/2),
         y_quartile3 = ((((((slope*min(x)) + intercept) + 
                             ((slope*max(x)) + intercept))/2)) + 
                          ((slope*max(x)) + intercept))/2) %>%
  unique()

quartiles.plot <- ggplot(data = quartile.df %>% filter(year %in% year.pattern)) +
  facet_wrap(~year) +
  geom_point(aes(x = x, y = y), alpha = 0.5) +
  geom_point(aes(x = X_BasePoint, y = Y_BasePoint), color = "red", size = 5) + 
  geom_point(aes(x = x_min, y = y_min), color = "darkred", size = 3) +
  geom_point(aes(x = x_quartile1, y = y_quartile1), color = "orange", size = 3) +
  geom_point(aes(x = x_midpoint, y = y_midpoint), color = "green", size = 3) +
  geom_point(aes(x = x_quartile3, y = y_quartile3), color = "blue", size = 3) +
  geom_point(aes(x = x_max, y =  y_max), color = "purple", size = 3) +
  ggtitle(paste("Profile:", profile.pattern, "Year:", year.pattern))
quartiles.plot

## Calculate euclidean distances between each point of interest and the base point.
euclidean.distances <- quartile.df %>% 
  select(profile, Park, year, everything(), -c(x, y, z, season, slope, intercept)) %>%
  unique() %>%
  ungroup() %>%
  mutate(min_euc_BP = sqrt(((X_BasePoint - x_min)^2) + ((Y_BasePoint -  y_min)^2))) %>%
  mutate(q1_euc_BP = sqrt(((X_BasePoint - x_quartile1)^2) + ((Y_BasePoint -  y_quartile1)^2))) %>%
  mutate(mid_euc_BP = sqrt(((X_BasePoint - x_midpoint)^2) + ((Y_BasePoint -  y_midpoint)^2))) %>%
  mutate(q3_euc_BP = sqrt(((X_BasePoint - x_quartile3)^2) + ((Y_BasePoint -  y_quartile3)^2))) %>%
  mutate(max_euc_BP = sqrt(((X_BasePoint - x_max)^2) + ((Y_BasePoint -  y_max)^2)))

# sea.dist.plot <- ggplot(data = euclidean.distances, aes(x = year, y = min_euc_BP)) +
#   facet_wrap(~profile) +
#   geom_bar(position = "dodge", stat = "identity", alpha = 0.5) +
#   ggtitle(paste("Profile", profile.pattern, "Seaward Euclidean Distance to BasePoint"))
# sea.dist.plot
# 
# land.dist.plot <- ggplot(data = euclidean.distances, aes(x = year, y = max_euc_BP)) +
#   facet_wrap(~profile) +
#   geom_bar(position = "dodge", stat = "identity", alpha = 0.5) +
#   ggtitle(paste("Profile", profile.pattern, "Landward Euclidean Distance to BasePoint"))
# land.dist.plot


## Begin dumbbell plot work
dumbbell.df <- euclidean.distances %>%
  select(profile, Park, year, min_euc_BP, max_euc_BP) %>% 
  mutate(diff = max_euc_BP - min_euc_BP) %>%
  pivot_longer(cols = c(max_euc_BP, min_euc_BP)) %>% #get into long format
  rename(Position = name, #rename columns
         Distance = value)

Max <- dumbbell.df %>%
  filter(Position == "max_euc_BP")
Min <- dumbbell.df %>%
  filter(Position == "min_euc_BP")
head(Min)

p <- ggplot(dumbbell.df)+
  
  geom_segment(data = Max,
               aes(x = Distance, y = profile,
                   yend = Min$profile, xend = Min$Distance), #use the $ operator to fetch data from our "Females" tibble
               color = "#aeb6bf",
               size = 4.5, #Note that I sized the segment to fit the points
               alpha = .5) +
  
  geom_point(aes(x = Distance, y = profile, color = Position), size = 4, show.legend = TRUE)+
  
  ggtitle("Min and Max profile distances")
p