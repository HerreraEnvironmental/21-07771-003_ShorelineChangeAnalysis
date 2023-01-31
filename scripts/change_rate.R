library(ggmap)
library(data.table)
library(tidyverse)

profile.pattern <- c("prof_6_s18|prof_6_s97|prof_7_s18|prof_7_s97|prof_8_s18|prof_8_s97")
profile.pattern <- "prof_6"

# Import all files --------------------------------------------------
prof.names <- list.files(path = "data_raw/all_prof_xyz_s97-s22/", pattern = profile.pattern)
print(paste("Total number of files:", length(prof.names)))

prof.list <- lapply(paste("data_raw/all_prof_xyz_s97-s22/", prof.names, sep = ""), 
                   read.table, header = FALSE, col.names = c("x", "y", "z"))
names(prof.list) = prof.names

## Remember to handle the beachface/dune stuff.
prof.df <- rbindlist(prof.list, idcol = TRUE, fill = FALSE) %>%
  separate_wider_delim(.id, "_", names = c("drop", "profile", "season"), too_many = "merge") %>%
  separate_wider_delim(season, ".", names = c("season", "out"), too_many = "merge") %>%
  separate(season, 
           into = c("season", "year"), 
           sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  mutate(description = if_else(str_detect(year, "_"), TRUE, FALSE, NULL)) %>%
  filter(description == FALSE) %>% ## TODO: temp fix
  select(profile:year, x:z)


## Take a look only at profile 6
prof6 <- prof.df %>%
  filter(profile == 6 & year %in% c("18"))

ggplot(data = prof6, aes(x = x, y = y, group = season)) +
  geom_point(aes(color = season)) +
  geom_smooth(method = "lm", formula = y~x, color = "red")

## Visual layout
PugetSound <- c(left = -125, bottom = 47, right = -110, top = 49)
get_stamenmap(PugetSound, zoom = 5, maptype = "toner-lite") %>%
  ggmap()


b <- bbox(prof6)
b[1, ] <- (b[1, ] - mean(b[1, ])) * 1.05 + mean(b[1, ])
b[2, ] <- (b[2, ] - mean(b[2, ])) * 1.05 + mean(b[2, ])
# scale longitude and latitude (increase bb by 5% for plot) replace 1.05
# with 1.xx for an xx% increase in the plot size
ggplot(prof.df, aes(x, y, group = year, fill = year)) + 
  geom_polygon() +
  #coord_equal() +
  labs(x = "Easting", y = "Northing", fill = "year") +
  ggtitle("Profile 6")
