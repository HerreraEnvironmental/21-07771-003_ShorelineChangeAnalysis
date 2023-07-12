## RLionheart
## 21-0771-003
## Shoreline Conservation Areas, Washington State Parks


## Where does the "shore" line cross the MHHW plane? 

# -------------------------------------------------------------------------
## Modify the year and profile patterns to change selection
# year.pattern <- c("00")
# profile.pattern <- "prof_6"

## Source the required scripts
source("scripts/src/load_packages.R")
source("scripts/src/import_profiles.R")
source("scripts/src/assign_profile_parks.R")

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
    scene = list(xaxis = element_blank()),
    title = list(text = paste("Profile:", profile.pattern), y = 0.9),
    showlegend = FALSE)
complete.profile.plot
