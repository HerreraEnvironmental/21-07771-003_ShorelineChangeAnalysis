## RLionheart
## P21-0771-001
## January 2023
## Shoreline Conservation Areas, Washington State Parks

profile.pattern <- "prof_22"
year.pattern <- c("00")

source("scripts/src/load_packages.R")
source("scripts/src/import_profiles.R")

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
  filter(year %in% year.pattern) %>%
  select(profile, Park, X_BasePoint, Y_BasePoint, season:z) %>%
  drop_na()
complete.profile$year <- factor(complete.profile$year, levels =  c("97", "98", "99","00", "01", "02", "03",
                                                                   "04", "05", "06", "07", "08", "09", "10",
                                                                   "11", "12", "13", "14", "15", "16", "17",
                                                                   "18", "19", "20", "21", "22"))


## Try with quartiles
boxplot.quartiles <- complete.profile %>%
  select(profile, year, x, y, z) %>%
  group_by(profile, year) %>%
  do(model =  as.data.frame(boxplot(.[3:5])$stats)) %>%
  select(profile, year, model) %>% 
  unnest(model) %>%
  group_by(profile, year) %>%
  mutate(quartile = row_number()) %>%
  rename(x = V1, y = V2, z = V3)

boxplot.quartiles$quartile[boxplot.quartiles$quartile=="1"]<-"min"
boxplot.quartiles$quartile[boxplot.quartiles$quartile=="2"]<-"quartile1"
boxplot.quartiles$quartile[boxplot.quartiles$quartile=="3"]<-"median"
boxplot.quartiles$quartile[boxplot.quartiles$quartile=="4"]<-"quartile3"
boxplot.quartiles$quartile[boxplot.quartiles$quartile=="5"]<-"max"


maybe.pivot <- boxplot.quartiles %>%
  pivot_wider(names_from = quartile, values_from = c(x, y, z)) %>%
  left_join(complete.profile %>% 
              select(profile, X_BasePoint, Y_BasePoint, x, y) %>%
              unique(), multiple = "all")


# single.quartiles.plot <- ggplot(data = maybe.pivot) +
#   geom_point(aes(x = x, y = y), alpha = 0.5) +
#   geom_point(aes(x = X_BasePoint, y = Y_BasePoint), color = "red", size = 5) + 
#   geom_point(aes(x = x_min, y = y_min), color = "darkred", size = 3) +
#   geom_point(aes(x = x_quartile1, y = y_quartile1), color = "orange", size = 3) +
#   geom_point(aes(x = x_median, y = y_median), color = "green", size = 3) +
#   geom_point(aes(x = x_quartile3, y = y_quartile3), color = "blue", size = 3) +
#   geom_point(aes(x = x_max, y =  y_max), color = "purple", size = 3) +
#   theme(axis.text = element_blank()) +
#   ggtitle(paste("Profile:", profile.pattern, "Year:", year.pattern))
# single.quartiles.plot

## test
single.quartiles.plot <- ggplot(data = maybe.pivot) +
  facet_wrap(~profile) +
  geom_point(aes(x = x, y = y), alpha = 0.5) +
  geom_point(aes(x = X_BasePoint, y = Y_BasePoint), color = "red", size = 5) + 
  geom_point(aes(x = x_min, y = y_min), color = "darkred", size = 3) +
  geom_point(aes(x = x_quartile1, y = y_quartile1), color = "orange", size = 3) +
  geom_point(aes(x = x_median, y = y_median), color = "green", size = 3) +
  geom_point(aes(x = x_quartile3, y = y_quartile3), color = "blue", size = 3) +
  geom_point(aes(x = x_max, y =  y_max), color = "purple", size = 3) +
  theme(axis.text = element_blank()) +
  ggtitle(paste("Profile:", profile.pattern, "Year:", year.pattern))
single.quartiles.plot
