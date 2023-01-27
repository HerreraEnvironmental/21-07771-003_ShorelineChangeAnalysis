## RLionheart
## P21-0771-001
## Shoreline Conservation Areas, Washington State Parks

library(readr)
library(tidyverse)

## Shoreline profiles in x y z format (easting northing elevation). 
## Named prof_X_ttYY.out where X is the profile number, tt is a season code (e.g. f = fall) and YY is the year. 

## At least some of these are null. 
## We have two duplicates, see 

RemoveCsv <- function(full.filepaths) {
  # Gathers all files in given directory and drops the csv extension.
  #
  # Args
  #   full.filepaths: list of files in a directory matching given patterns.
  #
  # Returns
  #   no.path: list of files, with filepath and csv extension removed.
  #
  no.path <- substr(full.filepaths, 1, nchar(full.filepaths)-4)
  no.ID <-   gsub("\\_.*","", no.path)
  
  return(no.path)
}

all.profiles <- list.files("data_raw/")


# Import all files --------------------------------------------------
file.pattern <- "prof_17_f20"
filenames <- RemoveCsv(list.files(path = "C:/Users/rlionheart/HerreraWork/SCA_P21-07771-002/data_raw/crlc_prof_xyz_out_files_s97-sp19/",
                                  pattern = file.pattern))

for (i in filenames) {
  filepath <- file.path("data_raw/crlc_prof_xyz_out_files_s97-sp19", paste(i,".out", sep = ""))
  assign(make.names(i), suppressWarnings(read.table(filepath, header = FALSE, col.names = c("x", "y", "z"))))
}





