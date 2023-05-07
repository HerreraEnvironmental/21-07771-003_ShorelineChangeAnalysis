original <- read.csv("archive/data_secondary_OG/profiles_with_clusters.csv")

redo <- read.csv("data_secondary/REDOprofiles_with_clusters.csv")


t <- original %>% left_join(redo) %>%
  filter(profile %in% c("11", "40"))
