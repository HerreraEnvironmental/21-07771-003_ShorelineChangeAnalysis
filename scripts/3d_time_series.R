## RLionheart
## P21-0771-001
## March 2023
## Shoreline Conservation Areas, Washington State Parks


profile.pattern <- "prof_16"
source("scripts/src/import_profiles.R")


# Import all files --------------------------------------------------


## Isolate one season, one year
prof.data <- profiles.df %>%
  drop_na() %>%
  mutate(year = factor(year, levels =  c("98", "99","00", "01", "02", "03",
                                         "04", "05", "06", "07", "08", "09", "10",
                                         "11", "12", "13", "14", "15", "16", "17",
                                         "18", "19", "20", "21", "22"))) %>%
  filter(season == "f") #%>%
  #filter(!year %in% c("98", "99"))


## Plot
marker <- list(size = 3, shape = 1)

profileplot <- plot_ly(prof.data, x = ~x, y = ~y, z = ~z,
                       color = ~year, marker = marker, hoverinfo = "text", 
                       text = ~paste("</br> Year: ", year)) %>%
  add_markers() %>%
  layout(
    scene = list(xaxis = list(title = "x"),
                 yaxis = list(title = "y"),
                 zaxis = list(title = "z")),
    title = list(text = paste("Profile:", profile.pattern, "Years:"), y = 0.9),
    legend = levels(year))

profileplot


## Attempt to redo

my.data <- prof.data %>%
  select(-profile, -season, -y) %>%
  rename(y = year)

my.data$y <- factor(my.data$y, levels =  c("98", "99","00", "01", "02", "03",
                                           "04", "05", "06", "07", "08", "09", "10",
                                           "11", "12", "13", "14", "15", "16", "17",
                                           "18", "19", "20", "21", "22"))


prof.fig <- plot_ly(my.data, x = ~x, y = ~y, z = ~z, 
                  type = "scatter3d", mode = "lines", 
                  color=~year.list)
suppressWarnings(print(prof.fig))
