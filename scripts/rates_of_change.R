## RLionheart
## 21-0771-001
## February 2023
## Shoreline Conservation Areas, Washington State Parks

## Take quartile points along profiles and use euclidean distance to BP as the change rate.

#profile.pattern <- "prof"

source("scripts/src/load_packages.R")
#source("scripts/src/import_profiles.R")
source("scripts/src/assign_profile_parks.R")


## Acquire slope and intercept for all profiles/years
complete.lm.df <- complete.profile %>%
  group_by(profile, year) %>%
  do(model = lm(y ~ x, data = .)) %>%
  mutate(intercept = coef(model)[1],
         slope = coef(model)[2],
         rsq = summary(model)$r.squared) 

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
  mutate(min_dist_to_BP = sqrt(((BasePoint_X - x_min)^2) + ((BasePoint_Y -  y_min)^2))) %>%
  mutate(q1_dist_to_BP = sqrt(((BasePoint_X - x_quartile1)^2) + ((BasePoint_Y -  y_quartile1)^2))) %>%
  mutate(med_dist_to_BP = sqrt(((BasePoint_X - x_median)^2) + ((BasePoint_Y -  y_median)^2))) %>%
  mutate(q3_dist_to_BP = sqrt(((BasePoint_X - x_quartile3)^2) + ((BasePoint_Y -  y_quartile3)^2))) %>%
  mutate(max_dist_to_BP = sqrt(((BasePoint_X - x_max)^2) + ((BasePoint_Y -  y_max)^2)))

## Visualize the spatial migration minimum (furthest seaward) point over time;
## increasing means accretion, decreasing means erosion.
# seaward.point.plot <- ggplot(data = euc.quartile.distances, aes(x = year, y = min_dist_to_BP)) +
#   facet_wrap(~profile) +
#   geom_bar(position = "dodge", stat = "identity", alpha = 0.5) +
#   ggtitle("Accretion and Erosion of Seaward Point")
# seaward.point.plot

## Visualize each quartile's migration
# Messy, so commented out for now
# all.quartiles <- euc.quartile.distances %>%
#   select(profile:year, contains("dist")) %>%
#   pivot_longer(cols = contains("dist"))
# 
# all.quartiles.plot <- ggplot(all.quartiles, aes(fill=name, y=value, x=year)) + 
#   geom_bar(position="dodge", stat="identity")
# all.quartiles.plot

## Calculate rates of change by quartile
euclidean.rates <- euc.quartile.distances %>%
  select(profile:year, contains("dist")) %>%
  arrange(profile, year) %>%
  group_by(profile) %>%
  mutate(across(min_dist_to_BP:max_dist_to_BP, ~ ((.x/lag(.x) - 1) * 100), .names = "rate_{.col}"))

quartile.rates <- euclidean.rates %>%
  select(profile:year, contains("rate")) %>%
  pivot_longer(cols = contains("rate"), 
               names_to = "quartile", values_to = "rate_of_change") 

## Add in the average rate of all quartiles.
mean.rate.df <- euclidean.rates %>%
  drop_na() %>%
  group_by(profile, year) %>%
  mutate(mean_rate = mean(rate_min_dist_to_BP:rate_max_dist_to_BP)) %>%
  select(profile, Park, year, rate_of_change = mean_rate) %>%
  mutate(quartile = "mean")

## Combine for a complete df of quartile rates with mean
all.quartile.rates <- quartile.rates %>%
  rbind(mean.rate.df) %>%
  arrange(profile, year) %>%
  mutate(profile_direction = ifelse(rate_of_change > 0, "Accretion", "Erosion"))

## Plot each rate of change for each profile
profile.ROC.plot <- ggplot(data = all.quartile.rates %>% drop_na(),  
                           aes(x = year, y = rate_of_change, fill = profile_direction)) +
  facet_wrap(~profile) +
  geom_bar(position = "dodge", stat = "identity", width = 1, color = "black") +
  scale_fill_manual(values=c("#04A1FF", "tomato2")) +
  theme(axis.text.x = element_blank()) +
  ggtitle("Individual Profile Rates of Change") 
profile.ROC.plot

## Group the rates by park

park.quartile.rates <- quartile.rates %>%
  rbind(mean.rate.df) %>%
  arrange(profile, year) %>%
  mutate(profile_direction = ifelse(rate_of_change > 0, "Accretion", "Erosion")) %>%
  separate(Park, into = c("Park", "Region"), sep = ",")


region.ROC.plot <- ggplot(data = park.quartile.rates %>% drop_na(),  
                            aes(x = year, y = rate_of_change, fill = profile_direction)) +
  facet_wrap(~Region, scales = "free") +
  geom_bar(position = "dodge", stat = "identity", width = 1, color = "black") +
  scale_fill_manual(values=c("#04A1FF", "tomato2")) +
  theme(axis.text.x = element_blank()) +
  ggtitle("Combined Rates of Change per Region") 
region.ROC.plot





## Annualized rates (not different from quartile rates)
# annualized.ROC <- euc.quartile.distances %>%
#   select(profile:year, med_dist_to_BP) %>%
#   arrange(profile, year) %>% 
#   group_by(profile) %>%
#   mutate(change=(med_dist_to_BP-lag(med_dist_to_BP,1))/lag(med_dist_to_BP,1)*100)


