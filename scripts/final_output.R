## Creating the final output



distance <- read.csv("data_secondary/profiles_with_midpoint_distance.csv")

significance <- read.csv("data_secondary/profiles_with_equations.csv")

washington <- read.csv("data_secondary/profiles_with_WCEHA.csv")

change <- read.csv("data_secondary/profiles_with_annualROC.csv")

cluster <- read.csv("data_secondary/profiles_with_clusters.csv")

## Adjusting reach by agreed-upon delineation, 4/7/23 meeting with Andrea and Ian David
agreed.delineations <- read.csv("data_raw/SCA_Shorezone_with_Profiles_20230407.csv") %>%
  separate_rows(Profiles, sep = ",") %>%
  mutate_if(is.character, str_trim) %>%
  rename(profile = Profiles,
         final_delineation = ReachType) %>%
  select(profile, final_delineation) %>%
  mutate(profile = as.numeric(profile))


## Construct final df
final <- distance %>%
  left_join(significance %>% select(profile, shoreline_profile), by = "profile") %>%
  left_join(washington %>% select(profile, WCEHA, conflict), by = "profile") %>%
  left_join(change %>% select(profile, year, rate_percent), by = c("profile", "year")) %>%
  left_join(cluster %>% select(-Park), by = "profile") %>%
  left_join(agreed.delineations, by = "profile")

t <- final %>% 
  unique() %>%
  group_by(final_delineation) %>%
  mutate(mean_percent_change = mean(rate_percent, na.rm = TRUE)) %>%
  select(profile, shoreline_profile, WCEHA, mean_percent_change, cluster_id:final_delineation) %>%
  unique() %>%
  mutate(final_delineation_character = case_when(
    (final_delineation == "A") ~ "North Beach Accretionay Zone. Overall statistically significant accretion.",
    (final_delineation == "B1") ~ "North Beach Transition Zone. Marks a point of transition between overall accretion and a shift towards lower rates of accretion.",
    (final_delineation == "B2") ~ "North Jetty Transition Zone. An area of shifting erosion and accretion, influenced by the shoreline armor at the entrance to North Bay, and defies broad characterization due to many environmental and manmade factors.",
    (final_delineation == "B3") ~ "Westport North Transition Zone. Highly built environment, frequently nourished by the Army Corps of Engineers and influenced by sediment input and the Westport Jetty.",
    (final_delineation == "B4") ~ "Westport South Zone. Another dynamic environment, marked by shoreline development influencing wave action and sediment transport.",
    (final_delineation == "C") ~ "South Beach Transition Zone. Breakpoint between Westport and the highly dynamic zone at the entrance to Willapa Bay.",
    (final_delineation == "D1") ~ "Washaway Beach Dynamic Zone. Highly dynamic, highly volatile and uncertain area of rapid erosion at the south end of the entrance to Willapa Bay, and rapid accretion to the north."))
