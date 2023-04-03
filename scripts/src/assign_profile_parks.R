## This script imports the Profiles for Erosion csv and connects the imported
## xyz profiles .out files to the BasePoint information, and each profile's 
## known geographic location. 

## In order to run this file, scripts/src/import_profiles.R must already have 
## bun run.

## Import erosion file for Base Point data
profile.erosion <- read_csv("data_raw/ProfilesForErosion.csv", 
                            col_names = c("profile", "Park", "MHHW",
                                          "BasePoint_X", "BasePoint_Y", 
                                          "Start_Year", "Start_X", "Start_Y", "Start_Dist",
                                          "End_Year", "End_X", "End_Y", "End_Dist",
                                          "Total_Change", "Years", "Change_per_Year",
                                          "Hannah", "2050", "Comments"), 
                            skip = 3, show_col_types = FALSE) %>%
  filter(profile %in% str_extract_all(profile.pattern,"\\(?[0-9,.]+\\)?")[[1]])

## Combine
complete.profile <- profile.erosion %>%
  full_join(profiles.df, by = "profile", multiple = "all") %>%
  select(profile, Park, MHHW, BasePoint_X, BasePoint_Y, season:z) 
