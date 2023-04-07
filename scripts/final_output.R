## Creating the final output



distance <- read.csv("data_secondary/profiles_with_midpoint_distance.csv")

significance <- read.csv("data_secondary/profiles_with_equations.csv")

washington <- read.csv("data_secondary/profiles_with_WCEHA.csv")

change <- read.csv("data_secondary/profiles_with_annualROC.csv")

cluster <- read.csv("data_secondary/profiles_with_clusters.csv")


final <- distance %>%
  left_join(significance %>% select(profile, shoreline_profile), by = "profile") %>%
  left_join(washington %>% select(profile, WCEHA, conflict), by = "profile") %>%
  left_join(change %>% select(profile, year, rate_percent), by = c("profile", "year")) %>%
  left_join(cluster %>% select(-Park), by = "profile")
# 
# Designation of accretion/erosion and accompanying significance
# 
# Rates of change of each
# 
# Subreach delineated by clustering
# 
# Inflection point if I have time
