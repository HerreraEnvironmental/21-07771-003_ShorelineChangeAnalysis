
profile.pattern <- "prof"

## List and order all files
profile.names <- list.files(path = "data_raw/all_prof_xyz_s97-s22/", pattern = profile.pattern)
full.names <- unique(str_extract(profile.names, "[^_]*_[^_]*"))
numbers.only <- unique(sub("^[^_]*_([^_]*).*", "\\1", profile.names)) %>%
  as.numeric() %>%
  sort()

print(paste("Total number of files:", length(profile.names)))
print("Fullnames of unique profiles:")
print(full.names)
print("Unique numbers of profiles:")
print(numbers.only)


## Import csv with geographic profile locations
## Import erosion file for Base Point data
profile.erosion <- read_csv("data_raw/ProfilesForErosion.csv", 
                            col_names = c("profile", "Park"), 
                            col_select = (1:2),
                            skip = 3, show_col_types = FALSE)
profile.OBA <- read_csv("data_raw/OBAProfiles.csv", 
                        col_names = c("OBA", "profile", "Notes"), 
                        col_select = c("profile", "OBA", "Notes"),
                        skip = 1, show_col_types = FALSE) %>%
  separate_longer_delim(profile, ",") %>%
  mutate(profile = as.numeric(gsub(" ", "", profile)))

complete.geo.profiles <- profile.OBA %>% 
  full_join(profile.erosion, by = "profile") %>%
  arrange(profile)



sequence <- complete.geo.profiles$profile 
seq2 <- min(sequence, na.rm = TRUE):max(sequence, na.rm = TRUE)
missing <- seq2[!seq2 %in% sequence]

print("Profiles without a geographic location included:")
print(missing)


