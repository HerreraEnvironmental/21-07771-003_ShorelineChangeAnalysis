## RLionheart
## 21-0771-002
## May 2023
## Shoreline Conservation Areas, Washington State Parks

profile.pattern <- "prof"
source("scripts/src/load_packages.R")
source("scripts/src/assign_profile_parks.R")


## Euclidean distances
euclidean <- complete.profile %>%
  group_by(profile, year) %>%
  mutate(x_midpoint = ((min(x) + max(x))/2)) %>%
  mutate(y_midpoint = ((min(y) + max(y))/2)) %>%
  select(profile, Park, year, BasePoint_X, BasePoint_Y, x_midpoint,  y_midpoint) %>%
  unique() %>%
  arrange(profile, year) %>%
  group_by(profile) %>%
  mutate(euc_dist_to_BP = sqrt(((BasePoint_X - x_midpoint)^2) + ((BasePoint_Y -  y_midpoint)^2))) %>%
  drop_na() %>%
  mutate(net_profile_slope = ifelse(euc_dist_to_BP[which.min(year)] < euc_dist_to_BP[which.max(year)],
                                    "Accretion", "Erosion")) %>%
  mutate(profile = factor(profile, levels = c("1", "2", "3", "4", "5", "6", "7",
                                              "8", "9", "10", "48", "11", "12",
                                              "13", "14", "15", "16", "17", "18",
                                              "19", "20", "21", "22", "23", "24",
                                              "25", "26", "27", "28", "29", "30",
                                              "31", "32", "33", "34", "35", "36",
                                              "37", "49", "38", "50", "39", "40",
                                              "51", "52", "41", "53", "54", "42",
                                              "43", "44", "45", "46", "47")))


## Extract equation parameters
equation.details <- euclidean %>%
  select(profile, Park, year, euc_dist_to_BP) %>%
  unique() %>%
  drop_na() %>%
  group_by(profile) %>%
  mutate(dummy_year = row_number()) %>%
  do(model = lm(euc_dist_to_BP ~ dummy_year, data = .)) %>%
  mutate(intercept = coef(model)[1],
         slope = coef(model)[2],
         rsq = summary(model)$r.squared,
         se = summary(model)$sigma,
         pvalue = glance(model)$p.value) %>%
  mutate(profile_direction = ifelse(slope > 0, "Accretion", "Erosion")) %>%
  select(profile, slope, rsq, se, pvalue) %>%
  mutate(profile = factor(profile, levels = c("1", "2", "3", "4", "5", "6", "7",
                                              "8", "9", "10", "48", "11", "12",
                                              "13", "14", "15", "16", "17", "18",
                                              "19", "20", "21", "22", "23", "24",
                                              "25", "26", "27", "28", "29", "30",
                                              "31", "32", "33", "34", "35", "36",
                                              "37", "49", "38", "50", "39", "40",
                                              "51", "52", "41", "53", "54", "42",
                                              "43", "44", "45", "46", "47")))



t <- euclidean %>%
  left_join(equation.details, by = "profile") %>%
  mutate(shoreline_profile = ifelse(pvalue < 0.05 & slope > 0, "Significant Accretion",
                                    ifelse(pvalue > 0.05 & slope > 0, "Non Significant Accretion", 
                                           ifelse(pvalue < 0.05 & slope < 0, "Significant Erosion", 
                                                  ifelse(pvalue > 0.05 & slope < 0, "Non Significant Erosion", "Other"))))) %>%
  drop_na() %>%
  select(profile, year, euc_dist_to_BP, shoreline_profile)

t$year <- as.Date(t$year, format="%y")
t$profile <- factor(t$profile, levels = c("1", "2", "3", "4", "5", "6", "7",
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

toplot <- t %>%
  group_by(profile) %>%
  group_map(
    .f = ~ggplot(.x, aes(x = year, y = euc_dist_to_BP, fill = shoreline_profile)) +
      geom_col(position = position_dodge(width = 1)) +
      scale_fill_manual(values = c("Non Significant Accretion" = "grey55",
                                   "Non Significant Erosion" = "grey54",
                                   "Significant Accretion" = "#04A1FF",
                                   "Significant Erosion" = "tomato2")) +
      geom_smooth(method = "lm", se = TRUE, color = "black") +
      xlab("Year") +
      ylab("Distance in meters from BasePoint") +
      guides(fill = guide_legend(title="")) +
      ggtitle(paste("Net Accretion or Erosion per Profile", .y$profile)))

names(toplot) <- sort(unique(t$profile))
lapply(names(toplot), 
       function(x) ggsave(filename = paste("figures/midpoint_plots/midpoint_plot_", 
                                           x, ".png", sep = ""), plot = toplot[[x]]))

