## This script imports the Profiles for Erosion csv and connects the imported
## xyz profiles .out files to the BasePoint information, and each profile's 
## known geographic location. 

## In order to run this file, scripts/src/import_profiles.R must already have 
## been run.

## Import erosion file for Base Point data
profile.erosion <- read_csv("data_raw/ProfilesForErosion.csv", 
                            col_names = c("profile", "Park", "MHHW",
                                          "BasePoint_X", "BasePoint_Y", 
                                          "Start_Year", "Start_X", "Start_Y", "Start_Dist",
                                          "End_Year", "End_X", "End_Y", "End_Dist",
                                          "Total_Change", "Years", "Change_per_Year",
                                          "Hannah", "2050", "Comments"), 
                            skip = 3, show_col_types = FALSE) %>%
  slice(-c(25, 26, 27))
  
  
if (str_detect(profile.pattern, "\\(?[0-9,.]+\\)?")) {
  profile.erosion <- profile.erosion %>%
    filter(profile %in% str_extract_all(profile.pattern,"\\(?[0-9,.]+\\)?")[[1]])
} else {
  profiles.df <- read.csv("data_secondary/all_imported_profiles.csv")
}


## Combine
complete.profile <- profile.erosion %>%
  full_join(profiles.df, by = "profile", multiple = "all") %>%
  select(profile, Park, MHHW, BasePoint_X, BasePoint_Y, season:z) %>%
  mutate(year = factor(year, levels =  c("97", "98", "99","00", "01", "02", "03",
                                         "04", "05", "06", "07", "08", "09", "10",
                                         "11", "12", "13", "14", "15", "16", "17",
                                         "18", "19", "20", "21", "22")))
