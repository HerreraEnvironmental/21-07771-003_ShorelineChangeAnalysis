## RLionheart
## P21-0771-001
## January 2023
## Shoreline Conservation Areas, Washington State Parks


#profile.pattern <- "prof_6"
#year.pattern <- c("00")
#source("scripts/src/import_profiles.R")


# Import all files --------------------------------------------------


## Isolate one season, one year
prof.data <- profiles.df %>%
  drop_na() %>%
  mutate(year = factor(year, levels =  c("97", "98", "99","00", "01", "02", "03",
                                         "04", "05", "06", "07", "08", "09", "10",
                                         "11", "12", "13", "14", "15", "16", "17",
                                         "18", "19", "20", "21", "22"))) %>%
  filter(year %in% year.pattern)


## Plot
marker <- list(size = 2, shape = 1)

profileplot <- plot_ly(prof.data, x = ~x, y = ~y, z = ~z,
                       color = ~year, marker = marker, hoverinfo = "text", 
                      text = ~paste("</br> Season: ", season)) %>%
  add_markers() %>%
  layout(
    scene = list(xaxis = list(title = "y"),
                 yaxis = list(title = "y"),
                 zaxis = list(title = "z")),
    title = list(text = paste("Profile:", profile.pattern, "Years:", year.pattern), y = 0.9),
    legend = levels(year))

profileplot

