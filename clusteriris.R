# Load data
install.packages("tidyverse")
install.packages("factoextra")
install.packages("dplyr")
install.packages("KMEANS.KNN")
library(KMEANS.KNN)
library(tidyverse)
library(factoextra)
data("iris")
View(iris)
# Scale the data
df <- scale(iris[1:4])
set.seed(123)
# Compute hierarchical k-means clustering
res.hk <-hkmeans(df, 4)
#1. If k = 4, how many observations are there by cluster?
obs.cluster <-table(res.hk$cluster)
obs.cluster

#2. If k = 4, how many observations are there in the wrong cluster and what are they? 
iris_with_clusters <- iris
iris_with_clusters$Cluster <- res.hk$cluster

# Create a contingency table of species vs clusters
contingency_table <- table(iris_with_clusters$Species, iris_with_clusters$Cluster)
contingency_table

# Determine the majority species in each cluster
library(dplyr)
cluster_species_mapping <- as.data.frame(contingency_table) %>%
  group_by(Var2) %>%
  slice_max(Freq) %>%
  select(Cluster = Var2, Species = Var1)

plot(cluster_species_mapping)

cluster_species_mapping$Cluster <- as.integer(as.character(cluster_species_mapping$Cluster))
# Map each observation to its predicted species based on its cluster
iris_with_clusters <- iris_with_clusters %>%
  left_join(cluster_species_mapping, by = c("Cluster" = "Cluster"))

# Compare the predicted species with the actual species
iris_with_clusters$WrongCluster <- iris_with_clusters$Species != iris_with_clusters$Species.y

# Get the number of observations in the wrong cluster
num_wrong <- sum(iris_with_clusters$WrongCluster)
wrong_observations <- iris_with_clusters[iris_with_clusters$WrongCluster, ]
wrong_observations
num_wrong

# Visualize the tree
hkmeans_tree(res.hk, cex = 0.6)
# or use this
fviz_dend(res.hk, cex = 0.6)

# Visualize the hkmeans final clusters
fviz_cluster(res.hk, frame.type = "norm", frame.level = 0.68)








library("igraph")
set.seed(5665)
fviz_dend(x = res.hk,
          k_colors = c("#A52A2A", "#0000CD", "#00CD00", "#A000CD"),
          color_labels_by_k = TRUE,
          cex = 0.3,
          type = "circular",
          horiz = T,
          ggtheme = theme_minimal(),
          repel = TRUE)


fviz_nbclust(df, FUN=hcut, method = "silhouette", k.max = 10) +
  labs(title = "Número óptimo de clusters")

