## RLionheart
## 21-0771-002
## May 2023
## Shoreline Conservation Areas, Washington State Parks

profile.pattern <- "prof"
source("scripts/src/load_packages.R")
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


## Calculate rates of change by quartile
quartile.rates <- euc.quartile.distances %>%
  select(profile:year, contains("dist")) %>%
  group_by(profile) %>%
  arrange(profile, year) %>%
  mutate(across(min_dist_to_BP:max_dist_to_BP, ~ (100 * (.x - dplyr::lag(.x))/lag(.x)), .names = "yearly_ROC_{.col}"))# %>%


quartile.rates.long <- quartile.rates %>%
  select(profile:year, contains("ROC")) %>%
  pivot_longer(cols = contains("ROC"), 
               names_to = "quartile", values_to = "rate_of_change") 

## Add in the average rate of all quartiles.
mean.rate.df <- quartile.rates %>%
  drop_na() %>%
  group_by(profile, year) %>%
  mutate(mean_yearly_rate = mean(yearly_ROC_min_dist_to_BP:yearly_ROC_max_dist_to_BP)) %>%
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
                                              "43", "44", "45", "46", "47"))) %>%
  filter(quartile == "mean")


## Individual plots
all.quartile.rates$year <- as.Date(all.quartile.rates$year, format="%y")
all.quartile.rates$profile <- factor(all.quartile.rates$profile , levels = 
                                       c("1", "2", "3", "4", "5", "6", "7",
                                         "8", "9", "10", "48", "11", "12",
                                         "13", "14", "15", "16", "17", "18",
                                         "19", "20", "21", "22", "23", "24",
                                         "25", "26", "27", "28", "29", "30",
                                         "31", "32", "33", "34", "35", "36",
                                         "37", "49", "38", "50", "39", "40",
                                         "51", "52", "41", "53", "54", "42",
                                         "43", "44", "45", "46", "47"))

levels = c("1", "2", "3", "4", "5", "6", "7",
           "8", "9", "10", "48", "11", "12",
           "13", "14", "15", "16", "17", "18",
           "19", "20", "21", "22", "23", "24",
           "25", "26", "27", "28", "29", "30",
           "31", "32", "33", "34", "35", "36",
           "37", "49", "38", "50", "39", "40",
           "51", "52", "41", "53", "54", "42",
           "43", "44", "45", "46", "47")

## Plot each rate of change for each profile
individual.ROC <- all.quartile.rates %>%
  group_by(profile) %>%
  group_map(
    .f = ~ggplot(data = .x %>% drop_na(),  
                 aes(x = year, y = rate_of_change, fill = profile_direction)) +
      geom_col(position = position_dodge(width = 1)) +
      scale_fill_manual(values=c("Accretion" = "#04A1FF",
                                 "Erosion" = "tomato2")) +
      xlab("Year") +
      ylab("Average of the Quartile Rate of Change (m/year)") +
      ggtitle(paste("Profile Average of Quartile Change Rate: Profile", .y$profile))
    )
  
names(individual.ROC) <- sort(unique(all.quartile.rates$profile))
lapply(names(individual.ROC), 
       function(x) ggsave(filename = paste("figures/ROC_plots/ROC_plot_", 
                                           x, ".png", sep = ""), plot = individual.ROC[[x]]))
  
