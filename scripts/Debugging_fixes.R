library(tidyverse)


## Description of error:
# R import/export bug when handling transfer between factor and numerical classes.
# Profiles have been classified as a factor type due to non-sequential geographic layout,
# produced problems in the re-import step for all profiles. Problem resulted in 

## Example:
year.bug <- read.csv("archive/data_secondary_OG/profiles_with_midpoint_distance.csv")
year.fixed <- read.csv("data_secondary/profiles_with_midpoint_distance.csv",
                       stringsAsFactors = TRUE)

year.change.bug <- read.csv("archive/data_secondary_OG/profiles_with_annualROC.csv")
year.change.fixed <- read.csv("data_secondary/profiles_with_annualROC.csv")

## Clustering delineation
original <- read.csv("archive/data_secondary_OG/profiles_with_clusters.csv")

redo <- read.csv("data_secondary/profiles_with_clusters.csv")

setdiff(original, redo)

clustering.conflicts <- original %>% rbind(redo) %>%
  filter(profile %in% c("11", "40"))

## Change rate calculation
original.final <- read.csv("archive/data_secondary_OG/reach_characterization_change_rates.csv") %>%
  select(reach_delineation, mean_reach_change_rate) %>%
  mutate(version = "original")

redo.final <- read.csv("data_secondary/reach_characterization_change_rates.csv") %>%
  select(reach_delineation, mean_reach_change_rate) %>%
  mutate(version = "redo")


ROC.conflicts <- original.final %>%
  rbind(redo.final) %>%
  group_by(reach_delineation) %>%
  mutate(dValue = mean_reach_change_rate[version == "original"] - 
           mean_reach_change_rate[version == "redo"])

ggplot(ROC.conflicts, aes(fill=version, y=mean_reach_change_rate, x=reach_delineation)) + 
  geom_bar(position="dodge", stat="identity")

  