library(fpc)
library(cluster) 
library(factoextra)
library(dbscan)
library(e1071)
library(ppclust)

# 1) K means
# Scale the data
scaled_unsupervised_numerical_df <- unsupervised_numerical_and_PIN_df_with_outcomes %>%
  select_if(is.numeric) %>% # Scale numerical columns only
  scale() %>% 
  as.data.frame()

# Elbow method to find suitable k
wss <- sapply(1:10, function(k) {
  kmeans_result <- kmeans(scaled_unsupervised_numerical_df, centers = k)
  kmeans_result$tot.withinss
})

plot(1:10, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters (K)", ylab = "Total within-cluster sum of squares (WSS)")

# Silhouette method to find suitable k
silhouette_score <- function(k){
  km <- kmeans(scaled_unsupervised_numerical_df, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(scaled_unsupervised_numerical_df))
  mean(ss[, 3])
}

k <- 2:10

avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)

# K means with 3 clusters (k=3)
km <- kmeans(scaled_unsupervised_numerical_df, centers = 3, nstart =25)
km.clusters <- km$cluster

factoextra::fviz_cluster(list(data=scaled_unsupervised_numerical_df, cluster = km.clusters))

# 2) PCA
res.pca <- FactoMineR::PCA(scaled_unsupervised_numerical_df)
factoextra::fviz_pca_biplot(res.pca,
                            geom.ind = "point",
                            pointshape = 21,
                            pointsize = 2.5,
                            fill.ind = as.factor(unsupervised_numerical_and_PIN_df_with_outcomes$PHQ9_status),
                            col.var = "black",
                            legend.title = list(fill = "Groups"),
                            repel = TRUE,
                            axes = c(1,2)
)+
  ggpubr::fill_palette("jco") +
  ggpubr::color_palette("npg")

# Store Eigen values in dataframe
eigen_values_PCA <- res.pca$eig %>%
  as.data.frame()

# Finding how many PCs needed to preserve 95% of the variance
num_components_95_variance <- min(which(eigen_values_PCA$`cumulative percentage of variance` >= 95))

# 3) Hierarchical Clustering
# Compute the distance matrix
dist_matrix <- dist(scaled_unsupervised_numerical_df)

# Perform agglomerative clustering
hc_result <- hclust(dist_matrix, method = "complete")

# Plot the dendrogram
plot(hc_result, main = "Agglomerative Clustering Dendrogram")

# Determine the number of clusters
k <- 3  # (According to elbow/silhouette method)

# Cut the dendrogram to obtain cluster assignments
hierarchical_clusters <- cutree(hc_result, k)

# Print cluster assignments
table(hierarchical_clusters)

# 4a) DBSCAN
# Compute the k-distance plot and sort distances
distances <- kNNdist(as.matrix(scaled_unsupervised_numerical_df), k = 4) 
sorted_distances <- sort(distances)

# Plot the k-distance graph
plot(sorted_distances, type = "l", xlab = "Points sorted by distance", ylab = "k-distance") # Knee point at k-distance = 6

# Run DBSCAN
dbscan_result <- dbscan(as.matrix(scaled_unsupervised_numerical_df), eps = 6, minPts = 5) 

# Obtain  cluster assignments and plot
db.clusters <- dbscan_result$cluster
factoextra::fviz_cluster(dbscan_result, scaled_unsupervised_numerical_df, geom = "point",
                         ellipse = F, show.clust.cent = F, palette = "jco", ggtheme = theme_classic())

table(db.clusters)

# 5) Fuzzy Clustering
fuzzy_data = as.matrix(scaled_unsupervised_numerical_df)

res.fcm <- ppclust::fcm(fuzzy_data, centers = 3)
fcm.clusters <- res.fcm$cluster

table(fcm.clusters)

factoextra::fviz_cluster(list(data = fuzzy_data, cluster = res.fcm$cluster), geom = "point",
                         ellipse = F, show.clust.cent = F, palette = "jco", ggtheme = theme_classic())
