## RLionheart
## P21-0771-001
## Shoreline Conservation Areas, Washington State Parks

library(data.table)
library(plotly)
library(tidyverse)


## Shoreline profiles in x y z format (easting northing elevation). 
## Named prof_X_ttYY.out where X is the profile number, tt is a season code (e.g. f = fall) and YY is the year. 

## At least some of these are null and will be removed and noted
## We have two duplicates, see accompanying script "duplicate_test.R"
## Check on non-conforming files: beachface? dunes?

# Set pattern to select files from directory
profile.pattern <- regex("prof_7", ignore_case = TRUE)

# Import all files --------------------------------------------------
all.dfs <- list.files(path = "data_raw/all_prof_xyz_s97-s22/", pattern=profile.pattern)
print(paste("Total number of files:", length(all.dfs)))

dfs.list <- suppressWarnings(lapply(paste("data_raw/all_prof_xyz_s97-s22/", all.dfs, sep = ""), 
                   read.table, header = FALSE, col.names = c("x", "y", "z")))
names(dfs.list) = all.dfs

## Remove empty dataframes from list according to row number (1 or fewer means empty)
dfs.filtered <- dfs.list[sapply(dfs.list, nrow) > 1]
removed.profiles <- dfs.list[!(dfs.list %in% dfs.filtered)]
print(paste("Number of removed (NULL) files:", length(names(removed.profiles))))

## Isolate and bind profiles
prof.df <- rbindlist(dfs.filtered, idcol = TRUE, fill = FALSE) %>%
  separate_wider_delim(.id, "_", names = c("drop", "profile", "season"), too_many = "drop") %>%
  separate_wider_delim(season, ".", names = c("season", "out"), too_few = "align_end") %>%
  separate(season, 
           into = c("season", "year"), 
           sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  select(profile:year, x:z)

## Isolate one season, one year
prof.data <- prof.df %>%
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

