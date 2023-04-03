## RLionheart
## 21-0771-001
## February 2023
## Shoreline Conservation Areas, Washington State Parks


## Where does the "shore" line cross the MHHW plane? 

# -------------------------------------------------------------------------

# year.pattern <- c("00")
# profile.pattern <- "prof_6"
source("scripts/src/load_packages.R")
source("scripts/src/import_profiles.R")
source("scripts/src/assign_profile_parks.R")

# ## Import erosion file for Base Point data
# profile.erosion <- read_csv("data_raw/ProfilesForErosion.csv", 
#                             col_names = c("profile", "Park", "MHHW",
#                                           "BasePoint_X", "BasePoint_Y", 
#                                           "Start_Year", "Start_X", "Start_Y", "Start_Dist",
#                                           "End_Year", "End_X", "End_Y", "End_Dist",
#                                           "Total_Change", "Years", "Change_per_Year",
#                                           "Hannah", "2050", "Comments"), 
#                             skip = 3, show_col_types = FALSE) %>%
#   filter(profile %in% str_extract_all(profile.pattern,"\\(?[0-9,.]+\\)?")[[1]])
# 
# ## Combine
# complete.profile <- profile.erosion %>%
#   full_join(profiles.df, by = "profile") %>%
#   select(profile, Park, MHHW, BasePoint_X, BasePoint_Y, season:z) 

# Complete profile and MHHW plot -------------------------------------------------
marker <- list(color = ~year, showscale = TRUE,
               size = 2, shape = 1)

complete.profile.plot <- plot_ly(complete.profile %>% drop_na(), x = ~x, y = ~y, z = ~z,
                       marker = marker, hoverinfo = "text", 
                       text = ~paste('</br> Year: ', year)) %>%
  add_markers() %>%
  add_mesh(complete.profile, x = ~x, y = ~y, z = ~MHHW, opacity = 0.5) %>%
  layout(
    scene = list(xaxis = list(title = "x"),
                 yaxis = list(title = "y"),
                 zaxis = list(title = "z")),
    title = list(text = paste("Profile:", profile.pattern, "Years:"), y = 0.9),
    legend = levels(year))

complete.profile.plot
