## RLionheart
## P21-0771-001
## January 2023
## Shoreline Conservation Areas, Washington State Parks

source("scripts/load_packages.R")

## Set profile pattern to import files
profile.pattern <- regex("prof_6", ignore_case = TRUE)
year.pattern <- "97"

## Import files
source("scripts/import_profiles.R")


## Take a look only at a single profile
single.profile <- profiles.df %>%
  filter(year %in% c("97"))

## Linear Regression
ggplot(data = single.profile, aes(x = x, y = y, group = year)) +
  geom_point() +
  geom_smooth(method = "lm", linewidth = 1) +
  theme(axis.text = element_blank()) +
  ggtitle(paste("Profile:", profile.pattern, "Year:", year.pattern))

plot_ly(x=fitted_values$x, y=fitted_values$y, z=fitted_values$z, type="scatter3d", mode="markers")




# 3D Regression Line #2 ------------------------------------------------------
N <- nrow(single.profile) 

mean_profile <- apply(single.profile[, 4:6], 2, mean)
pca_profile  <- princomp(single.profile[, 4:6]) 
vector_profile <- pca_profile$loadings[, 1]

profile_fit <- matrix(rep(mean_profile, each = N), ncol=3) + 
  pca_profile$score[, 1] %*% t(vector_profile) 
fitted_values <- as_tibble(profile_fit)

t_ends <- c(min(pca_profile$score[,1]) - 0.2, max(pca_profile$score[,1]) + 0.2)  
endpoints <- rbind(mean_profile + t_ends[1]*vector_profile, mean_profile + t_ends[2]*vector_profile)

profile3D <- scatterplot3d(single.profile[, 4:6], pch=20, main = "Profile 6, 1997")
profile3D$points3d(endpoints, type="l", col="blue", lwd=1)

for (i in 1:N){
  profile3D$points3d(rbind(single.profile[, 4:6][i, ], profile_fit[i, ]), type="l", col="green3", lty=2)
} 


# Polygon + spatial geoplots ------------------------------------------------------------
ggplot(profiles.df, aes(x, y, group = year, fill = year)) + 
  geom_polygon() +
  labs(x = "Easting", y = "Northing", fill = "year") +
  ggtitle(paste("Profile:", profile.pattern, "Year:", year.pattern))


# Visual ------------------------------------------------------------
# PugetSound <- c(left = -124.5, bottom = 46, right = -123.5, top = 47.75)
# get_stamenmap(PugetSound, zoom = 10, maptype = "terrain-background") %>%
#   ggmap()
