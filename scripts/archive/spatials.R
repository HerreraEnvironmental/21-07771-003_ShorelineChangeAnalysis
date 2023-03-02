# Polygon + spatial geoplots ------------------------------------------------------------
ggplot(profiles.df, aes(x, y, group = year, fill = year)) +
  geom_polygon() +
  labs(x = "Easting", y = "Northing", fill = "year") +
  ggtitle(paste("Profile:", profile.pattern, "Year:", year.pattern))

# Visual ------------------------------------------------------------
PugetSound <- c(left = -124.5, bottom = 46, right = -123.5, top = 47.75)
get_stamenmap(PugetSound, zoom = 10, maptype = "terrain-background") %>%
  ggmap()
