
## RLionheart
## 21-0771-001
## February 2023
## Shoreline Conservation Areas, Washington State Parks


profile.pattern <- "prof_17|prof_16|prof_22"

source("scripts/src/load_packages.R")
source("scripts/src/import_profiles.R")

## Import erosion file for Base Point data
complete.profile <- read_csv("data_raw/ProfilesForErosion.csv", 
                             col_names = c("profile", "Park", "MHHW",
                                           "X_BasePoint", "Y_BasePoint", 
                                           "Start_Year", "Start_X", "Start_Y", "Start_Dist",
                                           "End_Year", "End_X", "End_Y", "End_Dist",
                                           "Total_Change", "Years", "Change_per_Year",
                                           "Hannah", "2050", "Comments"), 
                             skip = 3,  show_col_types = FALSE) %>%
  filter(profile %in% str_extract_all(profile.pattern, "\\(?[0-9,.]+\\)?")[[1]]) %>%
  full_join(profiles.df, by = "profile", multiple = "all") %>%
  select(profile, Park, X_BasePoint, Y_BasePoint, season:z) %>%
  drop_na()
complete.profile$year <- factor(complete.profile$year, levels =  c("97", "98", "99","00", "01", "02", "03",
                                                                   "04", "05", "06", "07", "08", "09", "10",
                                                                   "11", "12", "13", "14", "15", "16", "17",
                                                                   "18", "19", "20", "21", "22"))

toplot <- complete.profile %>%
  select(profile, year, x, y, z) %>%
  group_by(profile, year) %>%
  do(model =  as.data.frame(boxplot(.[3:5])$stats)) %>%
  select(profile, year, model) %>% 
  unnest(model) %>%
  group_by(profile, year) %>%
  mutate(quartile = row_number()) %>%
  rename(x = V1, y = V2, z = V3)

toplot$quartile[toplot$quartile=="1"]<-"Min"
toplot$quartile[toplot$quartile=="2"]<-"Q2"
toplot$quartile[toplot$quartile=="3"]<-"Med"
toplot$quartile[toplot$quartile=="4"]<-"Q3"
toplot$quartile[toplot$quartile=="5"]<-"Max"


