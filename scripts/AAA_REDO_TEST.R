original <- read.csv("archive/data_secondary_OG/profiles_with_clusters.csv")

redo <- read.csv("data_secondary/REDOprofiles_with_clusters.csv")

setdiff(original, redo)


t <- original %>% rbind(redo) %>%
  filter(profile %in% c("11", "40"))
