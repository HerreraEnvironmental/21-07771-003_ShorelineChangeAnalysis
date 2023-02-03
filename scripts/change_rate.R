library(ggmap)
library(data.table)
library(rgl)
library(scatterplot3d)
library(tidyverse)

#profile.pattern <- c("prof_6_s18|prof_6_s97|prof_7_s18|prof_7_s97|prof_8_s18|prof_8_s97")
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
  filter(profile == 6 & year %in% c("97", "18"))

ggplot(data = prof6, aes(x = x, y = y, color = year)) +
 # geom_point() +
  geom_smooth(method = "lm", linewidth = 1) +
  theme(axis.text = element_blank())

##testd TODO START HERE
x97 <- prof6[1:57, 4]
y97 <- prof6[1:57, 4]
df97 <- prof6 %>%
  filter(year == 97) %>%
  select(x97 = x, y97 = y, z97 = z)

df <- cbind(df18, df97)
ggplot() +
  geom_jitter(df18, aes(x18, y18), colour = "red") +
  geom_smooth(aes(x18, y19, col = "red"), method = "lm", se = FALSE) +
  geom_jitter(aes(x2,y2),colour="green")+geom_smooth(aes(x2,y2,col="green"),method="lm",se=FALSE)


# 3D Regression Line #2 ------------------------------------------------------
N <- nrow(prof6) 

mean_profile <- apply(prof6[, 4:6], 2, mean)
pca_profile  <- princomp(prof6[, 4:6]) 
vector_profile <- pca_profile$loadings[, 1] # why 1?

profile_fit <- matrix(rep(mean_profile, each = N), ncol=3) + 
  pca_profile$score[, 1] %*% t(vector_profile) 

t_ends <- c(min(pca_profile$score[,1]) - 0.2, max(pca_profile$score[,1]) + 0.2)  
endpoints <- rbind(mean_profile + t_ends[1]*vector_profile, mean_profile + t_ends[2]*vector_profile)

profile3D <- scatterplot3d(prof6[, 4:6], pch=20, main = "Profile 6, 1997")
profile3D$points3d(endpoints, type="l", col="blue", lwd=1)

for (i in 1:N){
  profile3D$points3d(rbind(prof6[, 4:6][i, ], profile_fit[i, ]), type="l", col="green3", lty=2)
} 


# Plotly 3D attempt -------------------------------------------------------


plot_ly() %>%
  add_trace(x = danc.x, 
            y = danc.y,
            z = danc.z, 
            type = "scatter3d", 
            mode = "markers",
            color = coh$dance,
            colors = c("gray70", '#6d98f3'),
            opacity = 1) %>%
  add_trace(x = danc.x[which(coh$dance == 1)], 
            y = danc.y[which(coh$dance == 1)],
            z = danc.z[which(coh$dance == 1)], 
            type = "scatter3d",
            mode = "markers",
            marker = list(color = "black", symbol = "circle-open")) %>%
  add_trace(x = l1.x,
            y = l1.y,
            z = l1.z,
            type = "scatter3d",
            mode = "lines",
            line = list(color = "black", width = 5, dash = 'dash')) %>%
  layout(title = '"...used for dancing"',
         scene = list(xaxis = list(title = 'India', range = c(1,6), ticktype = "array", tickvals = ticks),
                      yaxis = list(title = 'World', range = c(1,6), ticktype = "array", tickvals = ticks),
                      zaxis = list(title = 'USA', range = c(1,6), ticktype = "array", tickvals = ticks),
                      camera = list(eye = list(x

## Jon's Profile 6
JonProf6 <- data.frame(x = c(218818.1, 218543.0, 218450.8),
                       y = c(218527.6, 218477.1, 218466.0),
                       row.names = c("basepoint", "start", "end"))

ggplot(JonProf6, aes(x, y), group=location) +
  geom_point() +
  geom_text(label=rownames(JonProf6))

# Polygon + spatial geoplots ------------------------------------------------------------
ggplot(prof.df, aes(x, y, group = year, fill = year)) + 
  geom_polygon() +
  labs(x = "Easting", y = "Northing", fill = "year") +
  ggtitle("Profile 6")


## Visual layout of PS, for later
PugetSound <- c(left = -124.5, bottom = 46, right = -123.5, top = 47.75)
get_stamenmap(PugetSound, zoom = 10, maptype = "terrain-background") %>%
  ggmap()