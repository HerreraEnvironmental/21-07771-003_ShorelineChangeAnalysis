source("scripts/load_packages.R")

# Set pattern to select files from directory
profile.pattern <- regex("prof_6|prof_7|prof_8", ignore_case = TRUE)

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
  select(profile:year, x:z) %>%
  mutate(profile = as.numeric(profile))
