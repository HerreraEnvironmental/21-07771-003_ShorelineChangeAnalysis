
## Clustering delineation
original <- read.csv("archive/data_secondary_OG/profiles_with_clusters.csv")

redo <- read.csv("data_secondary/profiles_with_clusters.csv")

setdiff(original, redo)

clustering.conflicts <- original %>% rbind(redo) %>%
  filter(profile %in% c("11", "40"))

## Change rate calculation
original.bb <- read.csv("archive/data_secondary_OG/reach_characterization_change_rates.csv") %>%
  select(reach_delineation, mean_reach_change_rate) %>%
  mutate(version = "original")

redo.bb <- read.csv("data_secondary/reach_characterization_change_rates.csv") %>%
  select(reach_delineation, mean_reach_change_rate) %>%
  mutate(version = "redo")


ROC.conflicts <- original.bb %>%
  rbind(redo.bb) %>%
  group_by(reach_delineation) %>%
  mutate(dValue = mean_reach_change_rate[version == "original"] - 
           mean_reach_change_rate[version == "redo"])

ggplot(ROC.conflicts, aes(fill=version, y=mean_reach_change_rate, x=reach_delineation)) + 
  geom_bar(position="dodge", stat="identity")
