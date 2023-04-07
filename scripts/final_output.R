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
         second_delineation = ReachType) %>%
  select(profile, second_delineation) %>%
  mutate(profile = as.numeric(profile))


## Construct final df
semifinal <- distance %>%
  left_join(significance %>% select(profile, shoreline_profile), by = "profile") %>%
  left_join(washington %>% select(profile, WCEHA, conflict), by = "profile") %>%
  left_join(change %>% select(profile, year, rate_percent), by = c("profile", "year")) %>%
  left_join(cluster %>% select(-Park), by = "profile") %>%
  left_join(agreed.delineations, by = "profile")
  

final <- semifinal %>% 
  unique() %>%
  group_by(second_delineation) %>%
  mutate(mean_percent_change = mean(rate_percent, na.rm = TRUE)) %>%
  select(profile, Park, shoreline_profile, WCEHA, mean_percent_change, second_delineation) %>%
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
    (second_delineation == "A") ~ "North Beach Accretion Zone. Relatively stable and steady accretion.",
    (second_delineation == "B1") ~ "North Beach Transition Zone. Marks a transition between steady accretion decreased rates of accertion.",
    (second_delineation == "B2") ~ "North Jetty Dynamic Zone. An area of shifting erosion and accretion, influenced by the shoreline armor at the entrance to North Bay, and defies broad characterization due to many environmental and manmade factors.",
    (second_delineation == "B3") ~ "Westport North Dynamic Zone. Highly built environment, frequently nourished by the Army Corps of Engineers and influenced by sediment input and the Westport Jetty.",
    (second_delineation == "B4") ~ "Westport South Dynamic Zone. Another dynamic environment, marked by shoreline development influencing wave action and sediment transport.",
    (second_delineation == "C") ~ "South Beach Transition Zone. Breakpoint between Westport and the highly dynamic zone at the entrance to Willapa Bay.",
    (second_delineation == "D1") ~ "Washaway Beach Dynamic Zone. Highly dynamic, highly volatile and uncertain area of rapid erosion at the south end of the entrance to Willapa Bay accompanied by rapid accretion to the north.",
    (second_delineation == "D2") ~ "Leadbetter Accretion Zone. Strong rates of accretion and buildout of beaches.",
    (second_delineation == "E") ~ "Long Beach North Accretion Zone. Characterized by moderate, stable accretion.",
    (second_delineation == "F") ~ "Long Beach Mid Accretion Zone. Small zone of increased accretion.",
    (second_delineation == "G") ~ "Long Beach OBA Influence Zone. Another small subreach of decreased rates of accretion, potentially influenced by the presence of a heavily used OBA near a suburb of Klipsan beach.",
    (second_delineation == "H") ~ "Long Beach South Accretion Zone. Steady, relatively high levels of accertion present through this area.",
    (second_delineation == "I") ~ "Long Beach South Mid Zone. Small subreach of decreased accertion rates as erosion begins.",
    (second_delineation == "J") ~ "Long Beach South Transition Zone. Area of fluctuation but overall erosion, potentially stabilized by the North Head Lighthouse outcropping.",
    (second_delineation == "M") ~ "Long Beach South Erosion Zone. Highly unstable area of strong erosion.")) %>%
  separate(col = temp, sep = "[.]", into = c("left", "right")) 

final[is.na(final)] = "Oregon"

write.csv(final, "data_secondary/final_subreach_characterization.csv", row.names = FALSE)
