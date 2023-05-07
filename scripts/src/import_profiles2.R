## Import all files
profile.names <- list.files(path = "data_raw/all_prof_xyz_s97-s22/", 
                            pattern = profile.pattern)

print(paste("Total number of files in profile:", length(profile.names)))

profile.list <- suppressWarnings(lapply(paste("data_raw/all_prof_xyz_s97-s22/", profile.names, sep = ""), 
                                        read.table, header = FALSE, col.names = c("x", "y", "z")))
names(profile.list) = profile.names

## Remove empty dataframes from list according to number of rows (1 or fewer means df is empty)
dfs.filtered <- profile.list[sapply(profile.list, nrow) > 1]
removed.profiles <- profile.list[!(profile.list %in% dfs.filtered)]
print(paste("Number of removed (NULL) files:", length(names(removed.profiles))))
print(paste("Names of removed (NULL) files:", names(removed.profiles)))

## Isolate and bind profiles
profiles.df <- rbindlist(dfs.filtered, idcol = TRUE, fill = FALSE) %>%
  separate_wider_delim(.id, "_", names = c("drop", "profile", "season"), too_many = "drop") %>%
  separate_wider_delim(season, ".", names = c("season", "out"), too_few = "align_end") %>%
  separate(season, 
           into = c("season", "year"), 
           sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  select(profile:year, x:z) %>%
  drop_na() %>%
  mutate(profile = as.numeric(profile)) %>%
  mutate(year = factor(year, levels =  c("97", "98", "99","00", "01", "02", "03",
                                         "04", "05", "06", "07", "08", "09", "10",
                                         "11", "12", "13", "14", "15", "16", "17",
                                         "18", "19", "20", "21", "22")))

#write.csv(profiles.df, "data_secondary/all_imported_profiles.csv", row.names = FALSE)
