## Source the data from the Washington Coastal Erosion Hazard Assessment

source("scripts/src/load_packages.R")
wash.data <- read_csv("data_raw/NANOOS_WCEHA_validation.csv",
                      col_names = c("profile", "WCEHA", "location", "notes"),
                      skip = 1,
                      show_col_types = FALSE)

## Drop unnecessary notes 
wash.data$WCEHA <- gsub(", below noise level", "", wash.data$WCEHA)
wash.data$WCEHA <- gsub(", no change rates available", "", wash.data$WCEHA)

nanoos.data <- read_csv("data_secondary/profiles_with_equations.csv",
                        show_col_types = FALSE)

all.data <- nanoos.data %>%
  left_join(wash.data, by = "profile") %>%
  select(profile, Park, shoreline_profile, WCEHA, -c(slope:pvalue, notes)) %>%
  mutate(shoreline_profile = ifelse(shoreline_profile == "Significant Accretion"| shoreline_profile == "Non Significant Accretion",
                                    "Accreted", "Erosion")) %>%
  mutate(conflict = ifelse(shoreline_profile != WCEHA, "flag", NA))

write.csv(all.data, "data_secondary/profiles_with_WCEHA.csv", row.names = FALSE)

