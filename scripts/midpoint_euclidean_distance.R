## RLionheart
## 21-0771-001
## January 2023
## Shoreline Conservation Areas, Washington State Parks

#profile.pattern <- "prof"
#source("scripts/src/import_profiles.R")
#profiles.df <- read.csv("data_secondary/all_imported_profiles.csv") 
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
                                "Accretion", "Erosion"))


### Extract equation parameters
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
  select(profile, slope, rsq, se, pvalue)

toplot <- euclidean %>%
  left_join(equation.details, by = "profile") %>%
  mutate(shoreline_profile = ifelse(pvalue < 0.05 & slope > 0, "Significant Accretion",
                          ifelse(pvalue > 0.05 & slope > 0, "Non Significant Accretion", 
                                 ifelse(pvalue < 0.05 & slope < 0, "Significant Erosion", 
                                        ifelse(pvalue > 0.05 & slope < 0, "Non Significant Erosion", "Other")))))


## Visualize euclidean distance from average Euclidean distance of each year
midpoint.euc.dist.plot <- ggplot(toplot %>% drop_na(), 
                                 aes(year, euc_dist_to_BP, 
                                     fill = shoreline_profile, group = shoreline_profile)) +
  scale_fill_manual(values=c("grey55", "grey54", "#04A1FF", "tomato2", "grey")) +
  #facet_grid(rows = vars(profile)) +
  facet_wrap(~profile) +
  geom_col(position = position_dodge(width = 1)) +
  geom_line(aes(group = shoreline_profile), position = position_dodge(width = 1),
            linewidth = 1, color = "black") +
  geom_smooth(method = "lm", se = TRUE, color="black") +
  xlab("Year") +
  ylab("Distance in m from BasePoint") + 
  theme(axis.text.x = element_blank()) +
  guides(fill=guide_legend(title="")) +
  ggtitle("Net Accretion or Erosion per Profile")
midpoint.euc.dist.plot


## Pearson's correlation 
pearson.correlation <- toplot %>%
  group_by(profile) %>%
  mutate(dummy_year = row_number()) %>%
  group_by(profile) %>%
  summarize(cor=cor(dummy_year, euc_dist_to_BP)) %>%
  mutate(relationship = ifelse(cor > 0 & cor < 1, TRUE, FALSE)) 
write.csv(pearson.correlation, "data_secondary/pearson_correlation.csv", row.names = FALSE)

## Table of results
results.table <- toplot %>%
  select(profile, Park, shoreline_profile) %>%
  unique() %>%
  left_join(equation.details, by = "profile")


## Download equations for WCEHA comparison
write.csv(results.table, "data_secondary/midpoint_profile_results.csv", row.names = FALSE)

## Download for cluster
# cluster <- euclidean %>%
#   drop_na() %>%
#   select(profile:year, euc_dist_to_BP) %>%
#   unique() 
# write.csv(cluster, "data_secondary/profiles_to_cluster.csv", row.names = FALSE)
