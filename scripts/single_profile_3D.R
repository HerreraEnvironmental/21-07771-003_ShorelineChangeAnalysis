## RLionheart
## P21-0771-001
## January 2023
## Shoreline Conservation Areas, Washington State Parks

source("scripts/load_packages.R")


## Shoreline profiles in x y z format (easting northing elevation). 
## Named prof_X_ttYY.out where X is the profile number, tt is a season code (e.g. f = fall) and YY is the year. 
## The vertical datum is NAVD88.
## The horizontal is WA State Plane South. All of the units are in meters.

## At least some of these are null and will be removed and noted
## We have two duplicates, see accompanying script "duplicate_test.R"
## Check on non-conforming files: beachface? dunes?


# Import all files --------------------------------------------------
source("scripts/import_profiles.R")

## Isolate one season, one year
prof.data <- profiles.df %>%
  drop_na() %>%
  mutate(year = factor(year, levels =  c("97", "98", "99","00", "01", "02", "03",
                                         "04", "05", "06", "07", "08", "09", "10",
                                         "11", "12", "13", "14", "15", "16", "17",
                                         "18", "19", "20", "21", "22")))


## Plot
marker <- list(color = ~year, showscale = TRUE,
               size = 2, shape = 1)

profileplot <- plot_ly(prof.data, x = ~x, y = ~y, z = ~z,
                       marker = marker) %>%
  add_markers() %>%
  layout(
    scene = list(xaxis = list(title = "x"),
                 yaxis = list(title = "y"),
                 zaxis = list(title = "z")),
    title = list(text = paste("Profile:", profile.pattern, "Years: 1997-2022"), y = 0.9),
    legend = levels(year))

profileplot

