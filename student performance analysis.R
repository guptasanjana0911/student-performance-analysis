#Required packages
#install.packages(c("tidyverse", "factoextra", "cluster", "psych"))
library(tidyverse)
library(factoextra)
library(cluster)
library(psych)

#Load the dataset
data=read.csv("C:/Users/USER/Downloads/student_data.csv")
head(data)

#Select and scale the numeric data for analysis
numeric_data<-data%>% select(where(is.numeric))
scaled_data<-scale(numeric_data)

#correlation matrix
corr_matrix<-cor(scaled_data)
corr_matrix

#Visualizing the correlation matrix
library(corrplot)
corrplot(corr_matrix)

#Applying PCA
pca_model <- prcomp(scaled_data, scale. = TRUE)
summary(pca_model)

# Scree Plot
fviz_eig(pca_model, addlabels = TRUE, main = "Scree Plot")

# PCA Biplot
fviz_pca_biplot(pca_model, repel = TRUE) + ggtitle("PCA Biplot")


# Factor Analysis ---

# Kaiser-Meyer-Olkin and Bartlett's test
KMO(corr_matrix)
cortest.bartlett(corr_matrix, n = nrow(scaled_data))

# Scree plot for Factor Analysis
scree(scaled_data)

# Running Factor Analysis model
fa_model <- factanal(scaled_data, factors = 3, rotation = "varimax")
print(fa_model, digits = 3, cutoff = 0.4)

# Factor diagram
fa.diagram(fa_model$loadings, cex = 0.9)



# --- 2. Cluster Analysis ---
# Determining optimal number of clusters
fviz_nbclust(scaled_data, kmeans, method = "wss")

# K-Means Clustering
set.seed(123)
kmeans_model <- kmeans(scaled_data, centers = 4, nstart = 25)

# Visualizing Clusters
fviz_cluster(kmeans_model, data = scaled_data, main = "Cluster Visualization")

# Hierarchical Clustering
dist_matrix <- dist(scaled_data)
hc_model <- hclust(dist_matrix, method = "ward.D2")
plot(hc_model, main = "Dendrogram")
rect.hclust(hc_model, k = 4, border = "red")
