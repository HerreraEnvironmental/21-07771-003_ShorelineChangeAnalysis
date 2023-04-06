## Clustering analysis to determine subreaches

## Load and prepare data.
## Import midpoint euclidean data. Order the profiles by geographic location,
## not by whatever order it was in. 
df <- read_csv("data_secondary/profiles_to_cluster.csv", show_col_types = FALSE) %>%
  mutate(year = factor(year, levels =  c("97", "98", "99","00", "01", "02", "03",
                                         "04", "05", "06", "07", "08", "09", "10", 
                                         "11", "12", "13", "14", "15", "16", "17",
                                         "18", "19", "20", "21", "22"))) %>%
  mutate(profile = factor(profile, levels = c("1", "2", "3", "4", "5", "6", "7",
                                              "8", "9", "10", "48", "11", "12",
                                              "13", "14", "15", "16", "17", "18",
                                              "19", "20", "21", "22", "23", "24",
                                              "25", "26", "27", "28", "29", "30",
                                              "31", "32", "33", "34", "35", "36",
                                              "37", "49", "38", "50", "39", "40",
                                              "51", "52", "41", "53", "54", "42",
                                              "43", "44", "45", "46", "47"))) %>%
  select(profile:year, euc_dist_to_BP) %>%
  unique() %>%
  arrange(profile)


## Clustering is very sensitive to outliers, so we will remove them for now. 
## Profiles 21 - 26 will be considered their own subreach, according to the 
## below boxplot. 
ggplot(df, aes(x = factor(profile), y = euc_dist_to_BP, group = profile)) +
  geom_boxplot(outlier.color = "red") +
  theme(panel.grid = element_line(color = "grey",
                                     size = 0.01))

## Munge data so that rows are observations and columns are variables.
df.arranged <- df %>%
  filter(!profile %in% c("21", "22", "23", "24", "25", "26")) %>%
  pivot_wider(names_from = c(Park, profile), values_from = euc_dist_to_BP) %>%
  column_to_rownames(var = "year") %>%
  t()

## Clustering cannot be performed on missing data. NA data needs to be removed.
## Locate which rows to drop
missing.rows <- which(rownames(df.arranged) %in% c("Haynisisoos Park, North Beach_1",
                                                   "Del Rey Beach Rd OBA_46", 
                                                   "Seaside Beach_47"))

df.drop <- df.arranged[-c(missing.rows), -c(1:3, 5, 14:16)]

## Since the two variables do not have the same units, one may have more weight.
## Scale the data to compare variables independent of units.
df.scaled <- scale(df.drop)

new.rownames <- gsub(".*_","",rownames(df.scaled))
row.names(df.scaled) <- new.rownames

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
barplot(df.hclust$height,
        names.arg = (nrow(df.scaled) - 1):1,
        main = "Barplot of Dendogram Heights, 'single' distance method")
abline(h = 1.5, col = "blue")


## Largest, most conservative grouping is 3. Next best is 12.
plot(df.hclust)
rect.hclust(df.hclust,
            k = 5, # k is used to specify the number of clusters, taken from previous steps.
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
             main = "Kmeans with 3 clusters according to wss and silhouette",
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
          k_colors = c("#04A1FF",'#FFC42E','#3ECDA3','#1D455C'),
          color_labels_by_k = TRUE,
          rect = TRUE)
kmeans.dendogram


# Assign clusters based on the last HCA --------------------------------------------------------
## Add back in profiles 21 - 25
df.clustered <- cutree(res.hc, k = 4) %>%
  as.data.frame() %>%
  rownames_to_column(var = "profile") %>%
  rename(cluster_id = 2) %>%
  full_join(df %>% select(profile, Park, euc_dist_to_BP), 
            by = "profile", multiple = "all") %>%
  select(profile, Park, cluster_id, euc_dist_to_BP) %>%
  unique() %>%
  mutate(profile = factor(profile, levels = c("1", "2", "3", "4", "5", "6", "7",
                                              "8", "9", "10", "48", "11", "12",
                                              "13", "14", "15", "16", "17", "18",
                                              "19", "20", "21", "22", "23", "24",
                                              "25", "26", "27", "28", "29", "30",
                                              "31", "32", "33", "34", "35", "36",
                                              "37", "49", "38", "50", "39", "40",
                                              "51", "52", "41", "53", "54", "42",
                                              "43", "44", "45", "46", "47"))) %>%
  arrange(profile)


cluster.plot <- ggplot(df.clustered) +
  geom_col(aes(euc_dist_to_BP, profile, 
               group = factor(profile), fill = factor(cluster_id)), 
           width = 0.6) +
  scale_fill_manual(values = c("#04A1FF",'#FFC42E','#3ECDA3','#1D455C')) +
  scale_y_discrete(limits = rev, position = "right")

cluster.plot
