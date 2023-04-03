## RLionheart
## P21-0771-001
## January 2023
## Shoreline Conservation Areas, Washington State Parks


source("scripts/src/load_packages.R")
source("scripts/src/import_profiles.R")
source("scripts/src/assign_profile_parks.R")

## Take a look only at a single profile
single.profile <- profiles.df %>%
  filter(year %in% year.pattern)

## Linear Regression
single.linear.plot <- ggplot(data = single.profile, aes(x = x, y = y, group = year)) +
  geom_point() +
  geom_smooth(method = "lm", linewidth = 1) +
  theme(axis.text = element_blank()) +
  ggtitle(paste("Profile:", profile.pattern, "Year:", year.pattern))
single.linear.plot


# Use grouped linear model for a single profile/year  -----------------------

## Apply linear model to single profile and year
single.lm.df <- complete.profile %>%
  group_by(profile, year) %>%
  do(model = lm(y ~ x, data = .)) %>%
  mutate(intercept = coef(model)[1],
         slope = coef(model)[2]) %>%
  filter(year %in% year.pattern)

single.POI.df <- complete.profile %>%
  left_join(single.lm.df %>% select(-model), by = c("profile", "year")) %>%
  filter(year %in% year.pattern) %>%
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
                          ((slope*max(x)) + intercept))/2)

single.quartiles.plot <- ggplot(data = single.POI.df) +
  facet_wrap(~year) +
  geom_point(aes(x = x, y = y), alpha = 0.5) +
  geom_point(aes(x = BasePoint_X, y = BasePoint_Y), color = "red", size = 5) + 
  geom_point(aes(x = x_min, y = y_min), color = "darkred", size = 3) +
  geom_point(aes(x = x_quartile1, y = y_quartile1), color = "orange", size = 3) +
  geom_point(aes(x = x_midpoint, y = y_midpoint), color = "green", size = 3) +
  geom_point(aes(x = x_quartile3, y = y_quartile3), color = "blue", size = 3) +
  geom_point(aes(x = x_max, y =  y_max), color = "purple", size = 3) +
  theme(axis.text = element_blank()) +
  ggtitle(paste("Profile:", profile.pattern, "Year:", year.pattern))
single.quartiles.plot
