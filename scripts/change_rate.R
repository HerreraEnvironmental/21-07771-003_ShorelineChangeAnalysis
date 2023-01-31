

profile.pattern <- c("prof_6_s18|prof_6_s97|prof_7_s18|prof_7_s97|prof_8_s18|prof_8_s97")
profile.pattern <- "prof_6"

# Import all files --------------------------------------------------
all.dfs <- list.files(path = "data_raw/all_prof_xyz_s97-s22/", pattern = profile.pattern)
print(paste("Total number of files:", length(all.dfs)))

dfs.list <- lapply(paste("data_raw/all_prof_xyz_s97-s22/", all.dfs, sep = ""), 
                   read.table, header = FALSE, col.names = c("x", "y", "z"))
names(dfs.list) = all.dfs


mydf <- rbindlist(dfs.list, idcol = TRUE, fill = FALSE) %>%
  separate_wider_delim(.id, "_", names = c("drop", "profile", "season"), too_many = "drop") %>%
  separate_wider_delim(season, ".", names = c("season", "out")) %>%
  separate(season, 
           into = c("season", "year"), 
           sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  select(profile:year, x:z)

experiment <- mydf %>%
  filter(profile == 6 & year %in% c("18", "97"))

prof6 <- rbindlist(dfs.list, idcol = TRUE, fill = FALSE)

plot(prof6$x, prof6$y)
         