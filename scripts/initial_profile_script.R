## RLionheart
## P21-0771-001
## Shoreline Conservation Areas, Washington State Parks

library(data.table)
library(plotly)
library(tidyverse)


## Shoreline profiles in x y z format (easting northing elevation). 
## Named prof_X_ttYY.out where X is the profile number, tt is a season code (e.g. f = fall) and YY is the year. 

## At least some of these are null and will be removed (should be noted?)
## We have two duplicates, see accompanying script 

profile.pattern <- regex("*.out")

# Import all files --------------------------------------------------
all.dfs <- list.files(path = "data_raw/all_prof_xyz_s97-s22/", pattern=profile.pattern)
print(paste("Total number of files:", length(all.dfs)))

dfs.list <- lapply(paste("data_raw/all_prof_xyz_s97-s22/", all.dfs, sep = ""), 
                   read.table, header = FALSE, col.names = c("x", "y", "z"))
names(dfs.list) = all.dfs

## Remove empty dataframes from list according to row number (1 or fewer means empty)
dfs.filtered <- dfs.list[sapply(dfs.list, nrow) > 1]
removed.profiles <- dfs.list[!(dfs.list %in% dfs.filtered)]
print(names(removed.profiles))

## Isolate profile 17 (find better selection than below)
prof17 <- rbindlist(dfs.filtered[29:39], idcol = TRUE, fill = FALSE) %>%
  separate_wider_delim(.id, "_", names = c("drop", "profile", "season"), too_many = "drop") %>%
  separate_wider_delim(season, ".", names = c("season", "out")) %>%
  separate(season, 
           into = c("season", "year"), 
           sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  select(profile:year, x:z)

## Isolate one season, one year
prof17.fall19 <- prof17 %>%
  filter(season == "f" & year == "19") %>%
  mutate(year = as.numeric(year))


## Plot
marker <- list(color = ~year, colorscale = c('#FFE1A1', '#683531'),
               showscale = TRUE)

p <- plot_ly(prof17.fall19, x = ~x, y = ~y, z = ~z, marker = marker) %>%
  add_markers() %>%
  layout(
    scene = list(xaxis = list(title = "x"),
                 yaxis = list(title = "y"),
                 zaxis = list(title = "z"))
  )

p

## Duplicating John's graph
prof22s18 <- read.table("data_raw/crlc_prof_xyz_out_files_s97-sp19/prof_22_s18.out",
                        header = FALSE, col.names = c("x", "y", "z"))


## Plot
marker2 <- list(size = 2, color = "orange")

p2 <- plot_ly(prof22s18, x = ~x, y = ~y, z = ~z, name = "spring18", marker = marker2) %>%
  add_markers() %>%
  layout(
    scene = list(xaxis = list(title = "x"),
                 yaxis = list(title = "y"),
                 zaxis = list(title = "z")),
    title = ("Profile 22, Spring '18")
  )


p2
