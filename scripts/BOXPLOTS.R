
## RLionheart
## 21-0771-001
## February 2023
## Shoreline Conservation Areas, Washington State Parks


profile.pattern <- "prof_17"
year.pattern <- c("00")

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
  filter(profile == (str_extract_all(profile.pattern, "\\(?[0-9,.]+\\)?")[[1]])) %>%
  full_join(profiles.df, by = "profile") %>%
  select(profile, Park, X_BasePoint, Y_BasePoint, season:z) %>%
  drop_na()
complete.profile$year <- factor(complete.profile$year, levels =  c("97", "98", "99","00", "01", "02", "03",
                                                                   "04", "05", "06", "07", "08", "09", "10",
                                                                   "11", "12", "13", "14", "15", "16", "17",
                                                                   "18", "19", "20", "21", "22"))

toplot <- complete.profile %>%
  filter(year %in% year.pattern) %>%
  group_by(profile, year)


boxplot(toplot[7:9])

Summary<-boxplot(toplot[7:9])$stats
colnames(Summary)<-c("x","y","z")
rownames(Summary)<-c("Min","First Quartile","Median","Third Quartile","Maximum")
quartiles <- as.data.frame(Summary)

## Visual
boxplot(y~x,data=toplot, notch = FALSE, main=paste("Profile:", profile.pattern),
        xlab="x", ylab="y")
