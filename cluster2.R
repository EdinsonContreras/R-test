# Load data
#edinson Contreras Cogollo
data(X1_2_DATOS_TALLER)
View(X1_2_DATOS_TALLER)
install.packages("tidyverse")
install.packages("factoextra")
install.packages("KMEANS.KNN")
library(tidyverse)
library(factoextra)
library(KMEANS.KNN)
# Scale the data
df <- scale(X1_2_DATOS_TALLER[,-c(1:24,30:32)],center = TRUE,scale = TRUE)
dg <- scale(X1_2_DATOS_TALLER[,c(25,26,27,28,29,33,35,36,37,38)],center = TRUE,scale = TRUE)
educa = as.data.frame(dg)
row.names(educa)<- make.names(X1_2_DATOS_TALLER$EDU_FATHER,unique = TRUE)


# Compute hierarchical k-means clustering
res.hk <-hkmeans(educa, 10)
table(res.hk$cluster)
?hkmeans

# Elements returned by hkmeans()
names(res.hk)

# Print the results
res.hk


# Visualize the tree
hkmeans_tree(res.hk, cex = 0.2)
# or use this
fviz_dend(res.hk, cex = 0.2)

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


fviz_nbclust(X1_2_DATOS_TALLER, FUNcluster = hkmeans, method = "silhouette", k.max = 5) +
  labs(title = "Número óptimo de clusters")

