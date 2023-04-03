## RLionheart
## 21-0771-001
## February 2023
## Shoreline Conservation Areas, Washington State Parks


## Where does the "shore" line cross the MHHW plane? 

# -------------------------------------------------------------------------

profile.pattern <- "prof_22"
year.pattern <- c("00")

source("scripts/src/load_packages.R")
source("scripts/src/import_profiles.R")
source("scripts/src/assign_profile_parks.R")

# Original single plot for reference  -------------------------------------------------

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

# Where does each profile cross the MHHW? -------------------------------------------------
MHHW <- complete.profile %>%
  group_by(year) %>%
  filter(z == MHHW)

marker <- list(showscale = FALSE,
               size = 5, shape = 1)

MHHW.to.BasePoint <- plot_ly(MHHW %>% drop_na(), x = ~x, y = ~y, z = ~z,
                             marker = marker, hoverinfo = "text", 
                             text = ~paste('</br> Year: ', year)) %>%
  add_markers() %>%
  add_markers(x = ~BasePoint_X, y = ~BasePoint_Y, z = ~MHHW) %>%
  add_mesh(complete.profile, x = ~x, y = ~y, z = ~MHHW, opacity = 0.5) %>%
  layout(
    scene = list(xaxis = list(title = "x"),
                 yaxis = list(title = "y"),
                 zaxis = list(title = "z")),
    title = list(text = paste("Profile:", profile.pattern, "Years:"), y = 0.9),
    showlegend = FALSE)

MHHW.to.BasePoint


