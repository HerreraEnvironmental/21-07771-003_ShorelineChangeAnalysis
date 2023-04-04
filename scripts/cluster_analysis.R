## Clustering analysis to determine subreaches

## Load and prepare data.
## Import midpoint euclidean data and arrange so that rows are observations
## and columsn are variables.
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
df.drop <- df[-c(21, 22, 23, 24),-c(24,25,26)]

## Since the two variables do not have the same units, one may have more weight.
## Scale the data to compare variables independent of units.
df.scaled <- scale(df.drop)

# HCA ---------------------------------------------------------------------
# Hierarchical agglomerative clustering


## Create the distance matrix. 
## This calculates the Euclidean distance between each pair of points
df.dist <- dist(df.scaled)

## Apply HCA using "single" distance method
df.hclust <- hclust(df.dist, method = "single")

## Determine optimal number of clusters from dendogram, using largest height difference.
plot(df.hclust, main = "Dendogram using 'single' distance method")

## Difficult to see , so let's do a barplot 
## where the columns correspond to the height of the dendogram.
diff(df.hclust$height)
barplot(df.hclust$height,
        names.arg = (nrow(df.scaled) - 1):1,
        main = "Barplot of Dendogram Heights")
abline(h = 2.661588, col = "blue")


## Largest, most conservative grouping is 3. Next best is 12.
plot(df.hclust)
rect.hclust(df.hclust,
            k = 3, # k is used to specify the number of clusters, taken from previous steps.
            border = "blue")

  
# kmeans Cluster analysis --------------------------------------------------------
## The below analysis uses a similar method but applies gap statistics to obtain k,
## then clusters from there.

## "Enhanced" distance matrix, still uses euclidean. Identical to dist() when stand = FALSE.
res.dist <- get_dist(df.scaled, stand = FALSE, method = "euclidean")

## Visualize the distance matrix.
fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

## Use the elbow and the silhouette method to determine optimal number of clusters.
## So far, they're both landing at 3.
fviz_nbclust(df.scaled, kmeans, method = "wss")
fviz_nbclust(df.scaled, kmeans, method = "silhouette")

# Visualize the clustering
km.res <- kmeans(df.scaled, 3, nstart = 25)
kmeans.plot <- fviz_cluster(km.res, data = df.scaled,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_light())
kmeans.plot

# Another hierarchical cluster --------------------------------------------------------
## Hclustering again, but this time using a variance minimizing method
## rather than a distance-based, nearest neighboring method.
res.hc <- df.scaled %>%
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2") 

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
  rename(cluster_id = 3) %>%
  select(profile, Park, cluster_id)

