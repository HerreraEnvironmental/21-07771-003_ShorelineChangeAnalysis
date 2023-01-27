## RLionheart
## P21-0771-001
## Shoreline Conservation Areas, Washington State Parks

library(car)
library(data.table)
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
prof13 <- dfs.filtered[1:12]
prof13.df <- rbindlist(prof13, idcol = TRUE, fill = FALSE)


oneprofile <- read.table("data_raw/crlc_prof_xyz_out_files_sp19-s22/prof_13_f19.out",
                         col.names = c("x", "y", "z"))


scatter3d(oneprofile)













