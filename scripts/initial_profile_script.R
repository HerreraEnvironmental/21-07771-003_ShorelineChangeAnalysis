## RLionheart
## P21-0771-001
## Shoreline Conservation Areas, Washington State Parks

library(readr)
library(tidyverse)

## Shoreline profiles in x y z format (easting northing elevation). 
## Named prof_X_ttYY.out where X is the profile number, tt is a season code (e.g. f = fall) and YY is the year. 

## At least some of these are null and will be removed.
## We have two duplicates, see accompanying script 



# Import all files --------------------------------------------------
all.dfs <- list.files(path = "data_raw/crlc_prof_xyz_out_files_sp19-s22/", pattern="*.out")
print(paste("Total number of files:", length(all.dfs)))

dfs.list <- lapply(paste("data_raw/crlc_prof_xyz_out_files_sp19-s22/", all.dfs, sep = ""), 
                   read.table, col.names = c("x", "y", "z"))



dfs.filtered <- dfs.list[sapply(dfs.list, nrow) > 1]










