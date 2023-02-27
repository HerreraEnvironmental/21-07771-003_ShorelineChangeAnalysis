## RLionheart
## 21-0771-001
## February 2023
## Shoreline Conservation Areas, Washington State Parks

source("scripts/load_packages.R")

## Where does the "shore" line cross the MHHW plane? 

#profile.pattern <- "prof_6|prof_7|prof_8|prof_9|prof_17|prof_41"
profile.pattern <- "prof_6"
source("scripts/import_profiles.R")

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

## Combine and add euclidean distance
complete.profile <- profile.erosion %>%
  full_join(profiles.df, by = "profile") %>%
  select(profile, Park, MHHW, BasePoint_X, BasePoint_Y, season:z)


## Plot
marker <- list(color = ~year, showscale = TRUE,
               size = 2, shape = 1)

profileplot <- plot_ly(complete.profile, x = ~x, y = ~y, z = ~z,
      marker = marker) %>%
  add_markers() %>%
  add_mesh(x = ~x, y = ~y, z = ~MHHW, data = complete.profile, opacity = 0.3) %>%
  layout(
    scene = list(xaxis = list(title = "x"),
                 yaxis = list(title = "y"),
                 zaxis = list(title = "z")),
    title = list(text = paste("Profile:", profile.pattern, "Years: 1997-2022"), y = 0.9),
    legend = levels(year))

profileplot

