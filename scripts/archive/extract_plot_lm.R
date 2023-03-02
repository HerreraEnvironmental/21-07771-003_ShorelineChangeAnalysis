## RLionheart
## 21-0771-001
## February 2023
## Shoreline Conservation Areas, Washington State Parks


## Take quartile points along profiles and use euclidean distance to BP as the change rate.

profile.pattern <- "prof_17"
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
  full_join(profiles.df, by = "profile") %>%
  select(profile, Park, X_BasePoint, Y_BasePoint, season:z) %>%
  drop_na()
complete.profile$year <- factor(complete.profile$year, levels =  c("97", "98", "99","00", "01", "02", "03",
                                                                   "04", "05", "06", "07", "08", "09", "10",
                                                                   "11", "12", "13", "14", "15", "16", "17",
                                                                   "18", "19", "20", "21", "22"))


# (ARCHIVE) Extracted xy coordinates from graphical linear regression -----------------------

## Plot a single profile and year with a regression line. 
single.lm.profile <- ggplot(data = complete.profile %>% filter(year %in% year.pattern),
       aes(x = x, y = y, group = year)) +
  facet_wrap(~year) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  ggtitle(paste("Profile:", profile.pattern, "Year:", year.pattern))
single.lm.profile

# Extract xy coordinates for the above graph's recession line
single.lm.df <- ggplot_build(single.lm.profile)$data[[2]][, c("x","y")]

# Quick linear model: find y at x position using extracted points
linear.model <- lm(data = single.lm.df, y ~ x)
intercept <- coef(linear.model)[1] # Intercept
slope <- coef(linear.model)[2] # slope
y_min <- (slope*min(single.lm.df$x)) + intercept
y_max <- (slope*max(single.lm.df$x)) + intercept

# Plot the points using the extracted slope
plot(single.lm.df$x, single.lm.df$y)
points(x = min(single.lm.df$x), y = y_min, col = "red", pch = 20, cex = 3)
points(x = max(single.lm.df$x), y = y_max, col = "blue", pch = 20, cex = 3)