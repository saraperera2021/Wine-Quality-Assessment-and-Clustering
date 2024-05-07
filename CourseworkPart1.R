# Load necessary libraries
library(readxl)
library(NbClust)
library(cluster)
library(factoextra)

#read the dataset
data<-read_excel("/Users/saraperera/Desktop/ML/Whitewine_v6.xlsx")

# Display structure and column names
head(data)
names(data)

# Extract data for scaling
data_to_scale<-data[,-12]

#scale the features
scaled_data<-scale(data_to_scale)

# Function to remove outliers
remove_outliers <- function(data, threshold = 2.5) {
  z_scores <- scale(data)
  mean_z <- apply(z_scores, 2, mean)
  sd_z <- apply(z_scores, 2, sd)
  outliers <- which(abs(z_scores) > threshold, arr.ind = TRUE)
  cleaned_data <- data
  cleaned_data[outliers] <- NA
  cleaned_data <- na.omit(cleaned_data)
  num_outliers_removed <- length(unique(outliers[, 1]))
  cat("Number of outliers removed:", num_outliers_removed, "\n")
  return(cleaned_data)
}

# Remove outliers from scaled data
cleaned_scaled_data <- remove_outliers(scaled_data)

# Plot box plots before and after outlier removal
par(mfrow=c(3, 4))
for (i in 1:(ncol(scaled_data))) {
  boxplot(scaled_data[, i], main=paste("Before Removal:", colnames(data)[i],
                                       "(Scaled)"), col="blue", border="black")
}

par(mfrow=c(3, 4))
for (i in 1:(ncol(cleaned_scaled_data))) {
  boxplot(cleaned_scaled_data[, i], main=paste("After Removal:", 
                  colnames(data)[i], "(Scaled)"), col="green", border="black")
}

# Set seed for reproducibility
set.seed(20)

# 1. NBClust Method
clusterNo=NbClust(cleaned_scaled_data,distance="euclidean", min.nc=2,max.nc=10,
                  method="kmeans",index="all")
clusterNo=NbClust(cleaned_scaled_data, distance="manhattan", min.nc=2,max.nc=15,
                  method="kmeans",index="all")
print(clusterNo)

# 2.Elbow Method
fviz_nbclust(cleaned_scaled_data, kmeans, method = "wss") + labs(subtitle = "Elbow Method")

# 3.Gap Statistics Method
gap_stat <- clusGap(cleaned_scaled_data, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
plot(gap_stat, main = "Gap Statistics")

# 4.Silhouette Method
fviz_nbclust(cleaned_scaled_data, kmeans, method = 'silhouette')


# Perform kmeans analysis for optimal k
k = 2
kmeans_2 = kmeans(cleaned_scaled_data, centers = k)
fviz_cluster(kmeans_2, data = cleaned_scaled_data, ellipse.type = "euclid", 
             star.plot = TRUE, repel = TRUE, ggtheme = theme_minimal())

k = 3
kmeans_3 = kmeans(cleaned_scaled_data, centers = k)
fviz_cluster(kmeans_3, data = cleaned_scaled_data, ellipse.type = "euclid", 
             star.plot = TRUE, repel = TRUE, ggtheme = theme_minimal())

k = 4
kmeans_4 = kmeans(cleaned_scaled_data, centers = k)
fviz_cluster(kmeans_4, data = cleaned_scaled_data, ellipse.type = "euclid", 
             star.plot = TRUE, repel = TRUE, ggtheme = theme_minimal())


# Perform kmeans analysis for optimal k
optimal_k <- 2
kmeans_data <- kmeans(cleaned_scaled_data, centers = optimal_k)
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
sil <- silhouette(kmeans_data$cluster, daisy(cleaned_scaled_data))
plot(sil, main = "Silhouette Plot for K-means Clustering",col=1:3,border=NA)
sil_widths <- sil[, "sil_width"]
avg_sil_width <- mean(sil_widths)
print(paste("Average Silhouette Width:",avg_sil_width))

