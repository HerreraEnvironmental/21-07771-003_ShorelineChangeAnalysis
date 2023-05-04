## RLionheart
## P21-0771-001
## March 2023
## Shoreline Conservation Areas, Washington State Parks


#profile.pattern <- "prof_22"

# Import all files --------------------------------------------------
source("scripts/src/import_profiles.R")


## Isolate one season, one year
prof.data <- profiles.df %>%
  drop_na() %>%
  mutate(year = factor(year, levels =  c("97", "98", "99","00", "01", "02", "03",
                                         "04", "05", "06", "07", "08", "09", "10",
                                         "11", "12", "13", "14", "15", "16", "17",
                                         "18", "19", "20", "21", "22"))) %>%
  filter(season == "f")

## Attempt to redo

my.data <- prof.data %>%
  select(-profile, -season, -y) %>%
  group_by(year) 
m <- list(
  l = 20,
  r = 20,
  b = 100,
  t = 100,
  pad = 4
)

profile.timeseries.fig <- plot_ly(my.data, x = ~x, y = ~as.numeric(year), z = ~z, 
                    type = "scatter3d", mode = "lines",
                    width = 3, color=~year) %>%
  layout(
    scene = list(xaxis = list(title = "Position", showticklabels = FALSE),
                 yaxis = list(title = "Year"),
                 zaxis = list(title = "Elevation")),
    title = list(text = paste("Profile:", profile.pattern), y = 0.95),
    legend = levels(year),
    showlegend = FALSE,
    autosize = F, width = 750, height = 750, margin = m)

