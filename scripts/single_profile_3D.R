## RLionheart
## 21-0771-003
## Shoreline Conservation Areas, Washington State Parks

## Uncomment the profile.pattern and year.pattern variables 
## to select and render a single year.
# profile.pattern <- "prof_2_"
# year.pattern <- c("00")


# Import all files --------------------------------------------------
source("scripts/src/import_profiles.R")


## Isolate one season, one year
prof.data <- profiles.df %>%
  drop_na() %>%
  mutate(year = factor(year, levels =  c("97", "98", "99","00", "01", "02", "03",
                                         "04", "05", "06", "07", "08", "09", "10",
                                         "11", "12", "13", "14", "15", "16", "17",
                                         "18", "19", "20", "21", "22"))) %>%
  filter(year %in% year.pattern)


## Plot
marker <- list(size = 3, shape = 1)

single.profile.plot <- plot_ly(prof.data, x = ~x, y = ~y, z = ~z,
                       color = ~season, marker = marker, hoverinfo = "text", 
                      text = ~paste("</br> Season: ", season)) %>%
  add_markers() %>%
  layout(
    scene = list(xaxis = list(title = "x"),
                 yaxis = list(title = "y"),
                 zaxis = list(title = "z")),
    title = list(text = paste("Profile:", sub("^[^_]*_", "", profile.pattern),
                              "Year:", year.pattern), y = 0.9),
    legend = levels(year))

single.profile.plot

