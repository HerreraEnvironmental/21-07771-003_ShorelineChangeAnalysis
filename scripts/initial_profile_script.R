## RLionheart
## P21-0771-001
## Shoreline Conservation Areas, Washington State Parks

library(car)
library(data.table)
library(plotly)
library(rgl)
library(scatterplot3d)
library(tidyverse)


## Shoreline profiles in x y z format (easting northing elevation). 
## Named prof_X_ttYY.out where X is the profile number, tt is a season code (e.g. f = fall) and YY is the year. 

## At least some of these are null and will be removed.
## We have two duplicates, see accompanying script 


# Import all files --------------------------------------------------
all.dfs <- list.files(path = "data_raw/crlc_prof_xyz_out_files_sp19-s22/", pattern="*.out")
print(paste("Total number of files:", length(all.dfs)))

dfs.list <- lapply(paste("data_raw/crlc_prof_xyz_out_files_sp19-s22/", all.dfs, sep = ""), 
                   read.table, header = FALSE, col.names = c("x", "y", "z"))
names(dfs.list) = all.dfs

## Remove empty dataframes from list (need to figure out how to include these)
dfs.filtered <- dfs.list[sapply(dfs.list, nrow) > 1]

## Do a test run: how has profile 13 changed over time?
prof13 <- dfs.filtered[1:12] # fix this
prof13 <- rbindlist(prof13, idcol = TRUE, fill = FALSE)
prof13.df <- prof13 %>%
  separate_wider_delim(.id, "_", names = c("drop", "profile", "season")) %>%
  separate_wider_delim(season, ".", names = c("season", "out")) %>%
  separate(season, 
           into = c("season", "year"), 
           sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  select(profile:year, x:z)

## First plot
scatterplot3d(x = prof13.df$x, y = prof13.df$y, z = prof13.df$z)

## 
marker <- list(color = ~year, colorscale = c('#FFE1A1', '#683531'), 
               showscale = TRUE)
# Create the plot
p <- plot_ly(prof13.df, x = ~x, y = ~y, z = ~z, marker = marker) %>%
  add_markers() %>%
  layout(
    scene = list(xaxis = list(title = "x"),
                 yaxis = list(title = "y"),
                 zaxis = list(title = "accretion"))
  )

p
