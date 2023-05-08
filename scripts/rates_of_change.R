## RLionheart
## 21-0771-001
## February 2023
## Shoreline Conservation Areas, Washington State Parks

## Take quartile points along profile linear models,
## and use euclidean distance to BasePoint as the change rate.

profile.pattern <- "prof"

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

write.csv(euc.quartile.distances %>% select(profile:year, contains("dist")), "data_secondary/profiles_with_quartile_distance.csv", 
          row.names = FALSE)

## Calculate rates of change by quartile
quartile.rates <- euc.quartile.distances %>%
  select(profile:year, contains("dist")) %>%
  group_by(profile) %>%
  arrange(profile, year) %>%
  mutate(across(min_dist_to_BP:max_dist_to_BP, ~ (100 * (.x - dplyr::lag(.x))/lag(.x)), .names = "yearly_ROC_{.col}"))# %>%
  # ungroup() %>%
  # group_by(profile) %>%
  # mutate(across(min_dist_to_BP:max_dist_to_BP, ~ (100 * ((.x[year==last(year)] - .x[year==first(year)])/.x[year==first(year)])),
  #               .names = "total_ROC_{.col}"))

quartile.rates.long <- quartile.rates %>%
  select(profile:year, contains("ROC")) %>%
  pivot_longer(cols = contains("ROC"), 
               names_to = "quartile", values_to = "rate_of_change") 

## Add in the average rate of all quartiles.
mean.rate.df <- quartile.rates %>%
  drop_na() %>%
  group_by(profile, year) %>%
  mutate(mean_yearly_rate = mean(yearly_ROC_min_dist_to_BP:yearly_ROC_max_dist_to_BP)) %>%
  #mutate(mean_total_rate = mean(total_ROC_min_dist_to_BP:total_ROC_max_dist_to_BP)) %>%
  select(profile, Park, year, rate_of_change = mean_yearly_rate) %>%#, total_rate_of_change = mean_total_rate) %>%
  mutate(quartile = "mean")

## Combine for a complete df of quartile rates with mean
all.quartile.rates <- quartile.rates.long %>%
  rbind(mean.rate.df) %>%
  arrange(profile, year) %>%
  mutate(profile_direction = ifelse(rate_of_change > 0, "Accretion", "Erosion")) %>%
  mutate(profile = factor(profile, levels = c("1", "2", "3", "4", "5", "6", "7",
                                              "8", "9", "10", "48", "11", "12",
                                              "13", "14", "15", "16", "17", "18",
                                              "19", "20", "21", "22", "23", "24",
                                              "25", "26", "27", "28", "29", "30",
                                              "31", "32", "33", "34", "35", "36",
                                              "37", "49", "38", "50", "39", "40",
                                              "51", "52", "41", "53", "54", "42",
                                              "43", "44", "45", "46", "47")))

## Write csv with all quartile rates
write.csv(all.quartile.rates, "data_secondary/profiles_with_quartROC.csv", row.names = FALSE)

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
# park.quartile.rates <- quartile.rates.long %>%
#   rbind(mean.rate.df) %>%
#   arrange(profile, year) %>%
#   mutate(profile_direction = ifelse(rate_of_change > 0, "Accretion", "Erosion")) %>%
#   separate(Park, into = c("Park", "Region"), sep = ",")

## Write ROC by NANOOS-defined region
#write.csv(park.quartile.rates, "data_secondary/NANOOSRegions_with_quartROC.csv", row.names = FALSE)

## Plot each rate of change for the three NANOOS-defined regions
# region.ROC.plot <- ggplot(data = park.quartile.rates %>% drop_na(),
#                             aes(x = year, y = rate_of_change, fill = profile_direction)) +
#   facet_wrap(~Region, scales = "free") +
#   geom_bar(position = "dodge", stat = "identity", width = 1, color = "black") +
#   scale_fill_manual(values=c("#04A1FF", "tomato2")) +
#   theme(axis.text.x = element_blank()) +
#   ggtitle("Combined Rates of Change per Region")
# region.ROC.plot


# Annualized ROC ----------------------------------------------------------
## Use the median of each transect and find the euclidean distance to the basepoint.
## Calculate growth (change) rate for each year and then average that for each profile.
annualized.rate <- quartiles.df %>%
  select(profile, Park, year, contains("BasePoint"), contains("median")) %>%
  unique() %>%
  arrange(profile, year) %>%
  rowwise() %>%
  mutate(med_dist_to_BP = sqrt(((BasePoint_X - x_median)^2) + ((BasePoint_Y -  y_median)^2))) %>%
  select(-c(BasePoint_X:y_median)) %>%
  group_by(profile) %>%
  mutate(dummy_year = row_number()) %>%
  mutate(diff_year = dummy_year - lag(dummy_year),  # Difference in time (just in case there are gaps)
         diff_growth = med_dist_to_BP - lag(med_dist_to_BP)) %>% # Difference in route between years
  mutate(rate_percent = (diff_growth / diff_year)/lag(med_dist_to_BP) * 100) %>% # growth rate in percent
  mutate(avg_annual_rate = mean(rate_percent, na.rm = TRUE)) # average percent growth rate


annualized.median.plot <- ggplot(data = annualized.rate %>% drop_na(),
                          aes(x = year, y = rate_percent)) +
  facet_wrap(~profile, scales = "free") +
  geom_bar(position = "dodge", stat = "identity", width = 1, color = "black") +
  scale_fill_manual(values=c("#04A1FF", "tomato2")) +
  theme(axis.text.x = element_blank()) +
  ggtitle("Annualized Rates of Change per Profile")
annualized.median.plot

## Write csv with annualized median rates per plot
write.csv(annualized.rate %>% select(profile, Park, year, rate_percent, avg_annual_rate),
          "data_secondary/profiles_with_annualROC.csv", row.names = FALSE)


