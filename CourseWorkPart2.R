# Load required libraries
library(stats)
library(NbClust)
library(cluster)
library(factoextra)
library(fpc)

# Perform PCA on the cleaned and scaled data
pca_result <- prcomp(cleaned_scaled_data, center = TRUE, scale = FALSE)

# Print summary of PCA
summary(pca_result)

# Extract eigenvalues and eigenvectors
eigenvalues <- pca_result$sdev^2
eigenvectors <- pca_result$rotation

print(eigenvalues)
print(eigenvectors)

# Calculate cumulative score per principal components (PC)
cumulative_score <- cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2))

# Print cumulative score
print("Cumulative Score per Principal Component:")
print(cumulative_score)

# Determine the number of principal components to keep
# Choose PCs with cumulative score > 85%
for(i in cumulative_score){
  if(i==0.8500){
    num_components<-sum(cumulative_score<=0.85)
  }
  else{
    num_components<-sum(cumulative_score<=0.85)+1
  }
}
print(num_components)

# Create transformed dataset with selected principal components
transformed_data <- as.matrix(cleaned_scaled_data) %*% eigenvectors[, 1:num_components]

# Print the transformed dataset
print("Transformed Dataset with Principal Components:")
print(transformed_data)

# Set seed for reproducibility
set.seed(20)

# 1. NBClust Method
clusterNo=NbClust(transformed_data,distance="euclidean", min.nc=2,max.nc=10,
                  method="kmeans",index="all")
clusterNo=NbClust(transformed_data, distance="manhattan", min.nc=2,max.nc=15,
                  method="kmeans",index="all")
print(clusterNo)

# 2.Elbow Method
fviz_nbclust(transformed_data, kmeans, method = "wss") + labs(subtitle = "Elbow Method")

# 3.Gap Statistics Method
gap_stat <- clusGap(transformed_data, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
plot(gap_stat, main = "Gap Statistics")

# 4.Silhouette Method
fviz_nbclust(cleaned_scaled_data, kmeans, method = 'silhouette')

#perform a kmeans analysis for k=2,3,4
k = 2
transformed_kmeans_2 = kmeans(transformed_data, centers = k)
fviz_cluster(transformed_kmeans_2, data = transformed_data, ellipse.type = "euclid", 
             star.plot = TRUE, repel = TRUE, ggtheme = theme_minimal())

k = 3
transformed_kmeans_3 = kmeans(transformed_data, centers = k)
fviz_cluster(transformed_kmeans_3, data = transformed_data, ellipse.type = "euclid", 
             star.plot = TRUE, repel = TRUE, ggtheme = theme_minimal())

k = 4
transformed_kmeans_4 = kmeans(transformed_data, centers = k)
fviz_cluster(kmeans_4, data = transformed_data, ellipse.type = "euclid", 
             star.plot = TRUE, repel = TRUE, ggtheme = theme_minimal())

# Perform kmeans analysis for optimal k
optimal_k <- 2
kmeans_data <- kmeans(transformed_data, centers = optimal_k)
print(kmeans_data)

# Calculate BSS and WSS
BSS <- kmeans_data$betweenss
TSS <- sum(kmeans_data$tot.withinss) + sum(kmeans_data$betweenss)
WSS <- sum(kmeans_data$tot.withinss)

# Calculate ratio of BSS to TSS
BSS_over_TSS <- BSS / TSS
print("The BSS TSS Ratio")
BSS_over_TSS
#total BSS+ TSS
Total <- BSS+TSS
Total

# Silhouette Analysis
sil <- silhouette(kmeans_data$cluster, daisy(transformed_data))
plot(sil, main = "Silhouette Plot for K-means Clustering",col=1:3,border=NA)
sil_widths <- sil[, "sil_width"]
avg_sil_width <- mean(sil_widths)
print(paste("Average Silhouette Width:", avg_sil_width))

print(kmeans_data$cluster)

# Compute the Euclidean distance matrix
dist_matrix <- dist(transformed_data)

# Calculate the Calinski-Harabasz Index
ch_index <- cluster.stats(dist_matrix, kmeans_data$cluster)$ch

# Print the Calinski-Harabasz Index
print(paste("Calinski-Harabasz Index:", ch_index))




