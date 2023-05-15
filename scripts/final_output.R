## Creating the final output

## Produced from midpoint_euclidean_distance.R
distance <- read.csv("data_secondary/profiles_with_midpoint_distance.csv")

## Produced from midpoint_euclidean_distance.R
significance <- read.csv("data_secondary/profiles_with_equations.csv")

## Produced from WCEHA_validation.R
washington <- read.csv("data_secondary/profiles_with_WCEHA.csv")

## Produced from rates_of_change.R
change <- read.csv("data_secondary/profiles_with_annualROC.csv")

## Produced from cluster_analysis.R
cluster <- read.csv("data_secondary/profiles_with_clusters.csv")

## Adjusting reach by agreed-upon delineation, 4/7/23 meeting with Andrea and Ian David
agreed.delineations <- read.csv("data_raw/SCA_Shorezone_with_Profiles_20230407.csv") %>%
  separate_rows(Profiles, sep = ",") %>%
  mutate_if(is.character, str_trim) %>%
  rename(profile = Profiles,
         reach_delineation = ReachType) %>%
  select(profile, reach_delineation) %>%
  mutate(profile = as.numeric(profile))


## Construct final df
semifinal <- distance %>%
  left_join(significance %>% select(profile, shoreline_profile), by = "profile") %>%
  left_join(washington %>% select(profile, WCEHA, conflict), by = "profile") %>%
  left_join(change %>% select(profile, year, rate_percent, avg_annual_rate), by = c("profile", "year")) %>%
  left_join(cluster %>% select(-Park), by = "profile") %>%
  left_join(agreed.delineations, by = "profile")
  

final <- semifinal %>% 
  unique() %>%
  group_by(reach_delineation) %>%
  mutate(mean_reach_change_rate = mean(rate_percent, na.rm = TRUE)) %>%
  select(profile, Park, shoreline_profile, WCEHA, mean_reach_change_rate, reach_delineation) %>%
  unique() %>%
  mutate(profile = factor(profile, levels = c("1", "2", "3", "4", "5", "6", "7",
                                              "8", "9", "10", "48", "11", "12",
                                              "13", "14", "15", "16", "17", "18",
                                              "19", "20", "21", "22", "23", "24",
                                              "25", "26", "27", "28", "29", "30",
                                              "31", "32", "33", "34", "35", "36",
                                              "37", "49", "38", "50", "39", "40",
                                              "51", "52", "41", "53", "54", "42",
                                              "43", "44", "45", "46", "47"))) %>%
  arrange(profile) %>%
  mutate(temp = case_when(
    (reach_delineation == "A") ~ "North Beach Accretion Zone. Relatively stable and steady accretion.",
    (reach_delineation == "B1") ~ "North Beach Transition Zone. Marks a transition between steady accretion decreased rates of accertion.",
    (reach_delineation == "B2") ~ "North Jetty Dynamic Zone. An area of shifting erosion and accretion, influenced by the shoreline armor at the entrance to North Bay, and defies broad characterization due to many environmental and manmade factors.",
    (reach_delineation == "B3") ~ "Westport North Dynamic Zone. Highly built environment, frequently nourished by the Army Corps of Engineers and influenced by sediment input and the Westport Jetty.",
    (reach_delineation == "B4") ~ "Westport South Dynamic Zone. Another dynamic environment, marked by shoreline development influencing wave action and sediment transport.",
    (reach_delineation == "C") ~ "South Beach Transition Zone. Breakpoint between Westport and the highly dynamic zone at the entrance to Willapa Bay.",
    (reach_delineation == "D1") ~ "Washaway Beach Dynamic Zone. Highly dynamic, highly volatile and uncertain area of rapid erosion at the south end of the entrance to Willapa Bay accompanied by rapid accretion to the north.",
    (reach_delineation == "D2") ~ "Leadbetter Accretion Zone. Strong rates of accretion and buildout of beaches.",
    (reach_delineation == "E") ~ "Long Beach North Accretion Zone. Characterized by moderate, stable accretion.",
    (reach_delineation == "F") ~ "Long Beach Mid Accretion Zone. Small zone of increased accretion.",
    (reach_delineation == "G") ~ "Long Beach OBA Influence Zone. Another small subreach of decreased rates of accretion, potentially influenced by the presence of a heavily used OBA near a suburb of Klipsan beach.",
    (reach_delineation == "H") ~ "Long Beach South Accretion Zone. Steady, relatively high levels of accertion present through this area.",
    (reach_delineation == "I") ~ "Long Beach South Mid Zone. Small subreach of decreased accertion rates as erosion begins.",
    (reach_delineation == "J") ~ "Long Beach South Transition Zone. Area of fluctuation but overall erosion, potentially stabilized by the North Head Lighthouse outcropping.",
    (reach_delineation == "M") ~ "Long Beach South Erosion Zone. Highly unstable area of strong erosion.")) %>%
  separate(col = temp, sep = "[.]", into = c("zone_name", "zone_description")) 

final[is.na(final)] = "Oregon"

write.csv(final, "data_secondary/final_subreach_characterization.csv", row.names = FALSE)

## Drop all extra columns 
barebones.final <- final %>%
  select(Reach = reach_delineation, Change_Rate = mean_reach_change_rate,
         Zone = zone_name, Description = zone_description) %>%
  filter(!Reach == "Oregon") %>%
  mutate(across(Change_Rate, round, 2)) %>%
  unique()

write.csv(barebones.final, "data_secondary/reach_characterization_change_rates.csv", row.names = FALSE)


