## First go at the cluster

library(tidyverse)
library(cluster)
library(factoextra)


## Load and prepare data
df <- read_csv("data_secondary/profiles_to_cluster.csv", show_col_types = FALSE) %>%
  select(-1) %>%
  mutate(year = factor(year, levels =  c("97", "98", "99","00", "01", "02", "03",
                                         "04", "05", "06", "07", "08", "09", "10", 
                                         "11", "12", "13", "14", "15", "16", "17",
                                         "18", "19", "20", "21", "22"))) %>%
  select(profile:year, euc_dist_to_BP) %>%
  unique() %>%
  pivot_wider(names_from = c(Park, profile), values_from = euc_dist_to_BP) %>%
  column_to_rownames(var = "year") %>%
  t()

## Clustering cannot be performed on missing data. NA data needs to be removed.
## Since the two variables do not have the same units, one may have more weight.
## Scale the data to compare variables independent of units.

df.drop <- df[-c(21, 22, 23, 24),-c(24,25,26)]
#df.drop <- na.omit(df.drop)
df.scaled <- scale(df.drop)


## Thought process: go with unsupervised clustering. We want the patterns to be found.
## Relevant types of unsupervised clustering: k-mean and hierarchical. 
## Kmeans requires some advance knowledge of the number of clusters k, 
## while HCA seeks to build a hierarchy of clusters without a predetermined k. 
## Because we do not know how similar the transects will be, and we are trying
## to determine the sub reaches, we will use HCA for this analysis. The number of classes
## is not specified in advance; hierarchical clustering will determine that.  
## Our variables are distance to baseline (euclidean midpoint movement over time),
## and year. 


# HCA ---------------------------------------------------------------------
# Hierarchical agglomerative clustering

## 1. Put every point in its own cluster.
## 2. Merge two points that are closest to each other based on "distance" from
## calculated distance matrix. This removes one cluster.
## - Measure cluster distance by single linkage: compute minimum distance before merging. 
## - TODO: Confirm this method
## 3. Recalculate all distances with the new number of clusters and save the distances
## in a new distance matrix.
## 4. Continue this until all clusters have been merged.

## Create the distance matrix. 
## This calculates the Euclidean distance between each pair of points
df.dist <- dist(df.scaled)

## Within the distance matrix, start comparing distance between individual clusters.
## The smallest distance between two points corresponds to the first height of the dendogram, 
## and are closest together in the matrix. Theses two points are combined to one group, 
## Now there are new distances between the newly created group and all the rest of the groups.
## The process is repeated until there are no more groups to create. 
## Now a dendogram can be drawn using the history of point combination,
## showing the sequence of combinations of the clusters. 
## The distances of merge between clusters, called heights, are illustrated on the y-axis.

df.hclust <- hclust(df.dist, method = "single")

## Determine optimal number of clusters from dendogram, using largest height difference.
plot(df.hclust)

## Difficult to see , so let's do a barplot
barplot(df.hclust$height,
        names.arg = (nrow(df.scaled) - 1):1 
)

plot(df.hclust)
rect.hclust(df.hclust,
            k = 12, # k is used to specify the number of clusters, taken from previous steps.
            border = "blue")

  
# kmeans Cluster analysis --------------------------------------------------------
## The below kmeans analysis 
res.dist <- get_dist(df.scaled, stand = TRUE, method = "euclidean")

fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
fviz_nbclust(df.scaled, kmeans, method = "gap_stat")


km.res <- kmeans(df.scaled, 3, nstart = 25)
kmeans.plot <- fviz_cluster(km.res, data = df.scaled,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_light())
kmeans.plot

# Another hierarchical cluster --------------------------------------------------------
## Hclustering again, but this time using a variance minimizing method
## rather than a distance-based, neighboring method.
res.hc <- df.scaled %>%
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2") ## TODO: Think about hclust methods...

kmeans.dendogram <- fviz_dend(res.hc, k = 4, ## Assigning 4 clusters based on viewing the graph.
          cex = 0.5,
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "tomato1"),
          color_labels_by_k = TRUE,
          rect = TRUE)
kmeans.dendogram


# Assign clusters based on the last HCA --------------------------------------------------------

df.clustered <- cutree(res.hc, k = 4) %>%
  as.data.frame() %>%
  rownames_to_column(var = "Park_profile") %>%
  separate(Park_profile, into = c("Park", "profile"), sep = "_") %>%
  rename(cluster_id = 3)


# Visualize as a dummbell plot --------------------------------------------------------


profile.pattern <- "prof"
year.pattern <- c("20")

source("scripts/src/load_packages.R")
source("scripts/src/import_profiles.R")


## Import erosion file for Base Point data
complete.profile <- read_csv("data_raw/ProfilesForErosion.csv", 
                             col_names = c("profile", "Park", "MHHW",
                                           "X_BasePoint", "Y_BasePoint", 
                                           "Start_Year", "Start_X", "Start_Y", "Start_Dist",
                                           "End_Year", "End_X", "End_Y", "End_Dist",
                                           "Total_Change", "Years", "Change_per_Year",
                                           "Hannah", "2050", "Comments"), 
                             skip = 3,  show_col_types = FALSE) %>%
  full_join(profiles.df, by = "profile", multiple = "all") %>%
  select(profile, Park, X_BasePoint, Y_BasePoint, season:z) %>%
  drop_na()

## Geographic locations
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


## Apply linear model 
linear.model.df <- complete.profile %>%
  filter(year %in% year.pattern) %>%
  group_by(profile, year) %>%
  do(model = lm(y ~ x, data = .)) %>%
  mutate(intercept = coef(model)[1],
         slope = coef(model)[2]) 

quartile.df <- complete.profile %>%
  left_join(linear.model.df %>% select(-model), by = c("profile", "year")) %>%
  filter(year %in% year.pattern) %>%
  group_by(profile, year) %>%
  mutate(x_west_west = min(x),
         y_west_west = (slope*min(x)) + intercept) %>%
  mutate(x_east_east = max(x),
         y_east_east = (slope*max(x)) + intercept) %>%
  unique()


## Calculate euclidean distances between each point of interest and the base point.
euclidean.distances <- quartile.df %>% 
  select(profile, Park, year, everything(), -c(x, y, z, season, slope, intercept)) %>%
  unique() %>%
  ungroup() %>%
  mutate(west_west_dist = sqrt(((X_BasePoint - x_west_west)^2) + ((Y_BasePoint -  y_west_west)^2))) %>%
  mutate(east_east_dist = sqrt(((X_BasePoint - x_east_east)^2) + ((Y_BasePoint -  y_east_east)^2))) %>%
  group_by(profile) 


## Begin dumbbell plot work
dumbbell.df <- euclidean.distances %>%
  select(profile, Park, year, west_west_dist, east_east_dist) %>%
  unique() %>%
  mutate(segment_dist = east_east_dist - west_west_dist) %>%
  pivot_longer(cols = c(east_east_dist, west_west_dist)) %>% 
  rename(position = name,
         euclidean_distance = value) %>%
  left_join(df.clustered %>% 
              select(-Park) %>% 
              mutate(profile = as.numeric(profile)), by = "profile") 

landward.point <- dumbbell.df %>%
  filter(position == "east_east_dist")
seaward.point <- dumbbell.df %>%
  filter(position == "west_west_dist")
diff <- dumbbell.df %>% 
  mutate(x_pos = euclidean_distance + (segment_dist/2)) %>%
  filter(position == "west_west_dist")

## Create plot
dumbbell.plot <- ggplot(dumbbell.df) +
  geom_segment(data = landward.point,
               aes(x = euclidean_distance, y = profile,
                   yend = seaward.point$profile, 
                   xend = seaward.point$euclidean_distance),
               linewidth = 4.5,
               alpha = .2) +
  geom_point(aes(x = euclidean_distance, y = profile, color = factor(cluster_id)), 
             size = 4, show.legend = TRUE) +
  geom_text(data = diff, aes(label = paste("Location: ", Park, profile), 
                             x = x_pos, y = profile),
            size = 2.5) +
  scale_y_continuous(trans = "reverse") +
  scale_x_continuous(trans = "reverse") +
  ggtitle(paste("Cluster group"))
dumbbell.plot
