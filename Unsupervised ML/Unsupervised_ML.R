library(fpc)
library(cluster) 
library(factoextra)
library(dbscan)
library(clusterCrit)
library(e1071)

# 1) K means
scaled_unsupervised_numerical_df <- unsupervised_numerical_df %>% # Scale numerical columns only
  scale() %>% 
  as.data.frame()

# Elbow method
wss <- sapply(1:10, function(k) {
  kmeans_result <- kmeans(scaled_unsupervised_numerical_df, centers = k)
  kmeans_result$tot.withinss
})

plot(1:10, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters (K)", ylab = "Total within-cluster sum of squares (WSS)")

# Silhouette method
silhouette_score <- function(k){
  km <- kmeans(scaled_unsupervised_numerical_df, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(scaled_unsupervised_numerical_df))
  mean(ss[, 3])
}

k <- 2:10

avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)

# 1b) Kmeans_Artur
km <- kmeans(scaled_unsupervised_numerical_df, centers = 3, nstart =25)
km.clusters <- km$cluster

factoextra::fviz_cluster(list(data=scaled_unsupervised_numerical_df, cluster = km.clusters))

# 2a) PCA
scaled_data <- scale(scaled_unsupervised_numerical_df)  # Scale the numeric data
pca_result <- prcomp(scaled_data, scale. = FALSE)  # Apply PCA without scaling

principal_components <- pca_result$x  # Principal components matrix
variance_proportions <- pca_result$sdev^2 / sum(pca_result$sdev^2)  # Variance proportions of each component

# Finding how many PCs needed to preserve 95% of the variance
cumulative_variance <- cumsum(variance_proportions)
num_components <- min(which(cumulative_variance >= 0.95))
num_components

# 2b) PCA (Artur)
res.pca <- FactoMineR::PCA(scaled_unsupervised_numerical_df)
factoextra::fviz_pca_biplot(res.pca,
                            geom.ind = "point",
                            pointshape = 21,
                            pointsize = 2.5,
                            fill,ind = as.factor(outcome$PHQ9),
                            col.var = "black",
                            legend.title = list(fill = "Groups"),
                            repel = TRUE,
                            axes = c(1,2)
)+
  ggpubr::fill_palette("jco")
  ggpubr::color_palette("npg")

# 3) Hierarchical Clustering
# a) Numerical datatset only
# Compute the distance matrix
dist_matrix <- dist(scaled_unsupervised_numerical_df)

# Perform agglomerative clustering
hc_result <- hclust(dist_matrix, method = "complete")

# Plot the dendrogram
plot(hc_result, main = "Agglomerative Clustering Dendrogram")

# Determine the number of clusters
k <- 3  # (According to elbow/silhouette method)

# Cut the dendrogram to obtain cluster assignments
cluster_assignments <- cutree(hc_result, k)

# Print cluster assignments
print(cluster_assignments)

# b) Numerical + categorical


# 4a) DBSCAN
# Compute the k-distance plot
distances <- kNNdist(as.matrix(scaled_unsupervised_numerical_df), k = 4) 

# Sort the distances
sorted_distances <- sort(distances)

# Plot the k-distance graph
plot(sorted_distances, type = "l", xlab = "Points sorted by distance", ylab = "k-distance")
# Knee point at k-distance = 6

# Run DBSCAN
dbscan_result <- dbscan(as.matrix(scaled_unsupervised_numerical_df), eps = 6, minPts = 5) 

# Check the cluster assignments
cluster_assignments <- dbscan_result$cluster
cluster_assignments

# 4b) DBSCAN Artur
db.clusters <- dbscan_result$cluster
factoextra::fviz_cluster(dbscan_result, scaled_unsupervised_numerical_df, geom = "point",
                         ellipse = F, show.clust.cent = F, palette = "jco", ggtheme = theme_classic())

# 5) Fuzzy Clustering
fuzzy_data = as.matrix(scaled_unsupervised_numerical_df)

res.fcm <- ppclust::fcm(fuzzy_data, centers =3)
res.fcm$cluster

factoextra::fviz_cluster(list(data = fuzzy_data, cluster = res.fcm$cluster), geom = "point",
                         ellipse = F, show.clust.cent = F, palette = "jco", ggtheme = theme_classic())
