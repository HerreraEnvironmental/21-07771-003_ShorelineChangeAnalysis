## First go at the cluster

library(NbClust)
library(tidyverse)

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

## 1. Put every point in its own cluster.
## 2. Merge two points that are closest to each other based on "distance" from
## calculated distance matrix. This removes one cluster.
## - Measure cluster distance by single linkage: compute minimum distance before merging. 
## - TODO: Confirm this method
## 3. Recalculate all distances with the new number of clusters and save the distances
## in a new distance matrix.
## 4. Continue this until all clusters have been merged.


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
            k = 12, # k is used to specify the number of clusters
            border = "blue")

  
# kmeans Cluster analysis --------------------------------------------------------
library("cluster")
library("factoextra")
distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 <- kmeans(distance, centers = 13, nstart = 25)
str(k2)

fviz_cluster(k2, data = distance)


######## First attempt
pairs(mydata[2:5])

plot(mydata$Moclips_6~ mydata$`Grayland Beach_23`, data = mydata)
with(mydata, text(mydata$`Grayland Beach_22` ~ mydata$`Grayland Beach_23`, 
                          labels=mydata$year,pos=4))


z <- mydata[,-c(1,1)]
means <- apply(z,2,mean)
sds <- apply(z,2,sd)
nor <- scale(z,center=means,scale=sds)
distance = dist(nor)

mydata.hclust = hclust(distance)
plot(mydata.hclust)
plot(mydata.hclust,labels=mydata$year,main='Default from hclust')
plot(mydata.hclust,hang=-1, labels=mydata$year,main='Default from hclust')
mydata.hclust<-hclust(distance,method="average") 
plot(mydata.hclust,hang=-1) 

member = cutree(mydata.hclust,3)
table(member)


plot(silhouette(cutree(mydata.hclust,3), distance))

## not working
wss <- (nrow(nor)-1)*sum(apply(nor,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(nor, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

