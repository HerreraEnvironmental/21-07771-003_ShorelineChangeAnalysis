## RLionheart
## P21-0771-001
## Shoreline Conservation Areas, Washington State Parks

library(readr)

## Shoreline profiles in x y z format (easting northing elevation). 
## Named prof_X_ttYY.out where X is the profile number, tt is a season code (e.g. f = fall) and YY is the year. 

t <- read.fwf(file = "data_raw/crlc_prof_xyz_out_files_s97-sp19/prof_1_f98.out", 
         skip=4, widths=c(12, 7, 4, 9, 4, 9, 4, 9, 4))
