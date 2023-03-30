## RLionheart
## 21-0771-001
## February 2023
## Shoreline Conservation Areas, Washington State Parks

## Take quartile points along profiles and use euclidean distance to BP as the change rate.

profile.pattern <- "prof_22|prof_16"

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
  filter(profile %in% as.numeric(str_extract_all(profile.pattern, "\\(?[0-9,.]+\\)?")[[1]])) %>%
  full_join(profiles.df, by = "profile", multiple = "all") %>%
  select(profile, Park, X_BasePoint, Y_BasePoint, season:z) %>%
  drop_na()
complete.profile$year <- factor(complete.profile$year, levels =  c("97", "98", "99","00", "01", "02", "03",
                                                                   "04", "05", "06", "07", "08", "09", "10",
                                                                   "11", "12", "13", "14", "15", "16", "17",
                                                                   "18", "19", "20", "21", "22"))
## Acquire slope and intercept for all profiles/years
complete.lm.df <- complete.profile %>%
  group_by(profile, year) %>%
  do(model = lm(y ~ x, data = .)) %>%
  mutate(intercept = coef(model)[1],
         slope = coef(model)[2]) 

## Calculate min, quartile1, median, quartile3, and max values for each profile and year
quartiles.df <- complete.profile %>%
  left_join(complete.lm.df %>% select(-model), by = c("profile", "year")) %>%
  group_by(profile, year) %>%
  mutate(x_min = min(x),
         y_min = (slope*min(x)) + intercept) %>%
  mutate(x_max = max(x),
         y_max = (slope*max(x)) + intercept) %>%
  mutate(x_median = ((x_min + x_max)/2),
         y_median = (((slope*min(x)) + intercept) +
                         ((slope*max(x)) + intercept))/2) %>%
  mutate(x_quartile1 = ((min(x) + ((x_min + x_max)/2))/2),
         y_quartile1 = (((slope*min(x)) + intercept) + 
                          (((slope*min(x)) + intercept) + 
                             ((slope*max(x)) + intercept))/2)/2) %>%
  mutate(x_quartile3 = ((x_median + max(x))/2),
         y_quartile3 = ((((((slope*min(x)) + intercept) + 
                             ((slope*max(x)) + intercept))/2)) + 
                          ((slope*max(x)) + intercept))/2) %>%
  drop_na()

## Calculate euclidean distances between each quartile and the base point.
euc.quartile.distances <- quartiles.df %>% 
  select(profile, Park, year, everything(), -c(x, y, z, season, slope, intercept)) %>%
  unique() %>%
  arrange(profile, year) %>%
  rowwise() %>%
  mutate(min_dist_to_BP = sqrt(((X_BasePoint - x_min)^2) + ((Y_BasePoint -  y_min)^2))) %>%
  mutate(q1_dist_to_BP = sqrt(((X_BasePoint - x_quartile1)^2) + ((Y_BasePoint -  y_quartile1)^2))) %>%
  mutate(med_dist_to_BP = sqrt(((X_BasePoint - x_median)^2) + ((Y_BasePoint -  y_median)^2))) %>%
  mutate(q3_dist_to_BP = sqrt(((X_BasePoint - x_quartile3)^2) + ((Y_BasePoint -  y_quartile3)^2))) %>%
  mutate(max_dist_to_BP = sqrt(((X_BasePoint - x_max)^2) + ((Y_BasePoint -  y_max)^2)))

## Visualize the spatial migration minimum (furthest seaward) point over time;
## increasing means accretion, decreasing means erosion.
seaward.point.plot <- ggplot(data = euc.quartile.distances, aes(x = year, y = min_dist_to_BP)) +
  facet_wrap(~profile) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.5) +
  ggtitle("Accretion and Erosion of Seaward Point")
seaward.point.plot

## Visualize each quartile's migration
all.quartiles <- euc.quartile.distances %>%
  select(profile:year, contains("dist")) %>%
  pivot_longer(cols = contains("dist"))

all.quartiles.plot <- ggplot(t, aes(fill=name, y=value, x=year)) + 
  geom_bar(position="dodge", stat="identity")
all.quartiles.plot

## Calculate rates of change  
euclidean.rates <- euc.quartile.distances %>%
  select(profile:year, contains("dist")) %>%
  arrange(profile, year) %>%
  group_by(profile) %>%
  mutate(min_rate = (min_dist_to_BP/lag(min_dist_to_BP) - 1) * 100) %>%
  mutate(q1_rate = (q1_dist_to_BP/lag(q1_dist_to_BP) - 1) * 100) %>%
  mutate(med_rate = (med_dist_to_BP/lag(med_dist_to_BP) - 1) * 100) %>%
  mutate(q3_rate = (q3_dist_to_BP/lag(q3_dist_to_BP) - 1) * 100) %>%
  mutate(max_rate = (max_dist_to_BP/lag(max_dist_to_BP) - 1) * 100)

t <- euc.quartile.distances %>%
  select(profile:year, contains("dist")) %>%
  arrange(profile, year) %>%
  group_by(profile) %>%
  mutate(across(min_dist_to_BP:max_dist_to_BP, ~ ((.x/lag(.x) - 1) * 100), .names = "rate_{.col}"))

## Change rates to dfs for plot
min_rate_df <- euclidean.rates %>%
  select(profile, year, rate = min_rate) %>%
  mutate(position = "min")

q1_rate_df <- euclidean.rates %>%
  select(profile, year, rate = q1_rate) %>%
  mutate(position = "q1")

med_rate_df <- euclidean.rates %>%
  select(profile, year, rate = med_rate) %>%
  mutate(position = "mid")

q3_rate_df <- euclidean.rates %>%
  select(profile, year, rate = q3_rate) %>%
  mutate(position = "q3")

max_rate_df <- euclidean.rates %>%
  select(profile, year, rate = max_rate) %>%
  mutate(position = "max")

mean_rate_df <- euclidean.rates %>%
  drop_na() %>%
  #group_by(year) %>%
  rowwise() %>%
  mutate(mean_rate = mean(min_rate:max_rate)) %>%
  select(profile, year, rate = mean_rate) %>%
  mutate(position = "mean")

## Combine
quartile.rates <- min_rate_df %>%
  rbind(q1_rate_df) %>%
  rbind(med_rate_df) %>%
  rbind(q3_rate_df) %>%
  rbind(max_rate_df) %>%
  rbind(mean_rate_df) 

quartile.ROC.plot <- ggplot(data = quartile.rates, aes(x = year, y = rate, fill = position)) +
  facet_wrap(~profile) +
  geom_bar(position = "dodge", stat = "identity") +
  gghighlight(position == "mean") +
  ggtitle(paste("Profile", profile.pattern, "Rate of Change")) 
quartile.ROC.plot

