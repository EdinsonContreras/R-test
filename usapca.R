# Principal component analysis
# ++++++++++++++++++++++++++++++
install.packages("factoextra")
install.packages("KMEANS.KNN")
install.packages("ggforce")
library(factoextra)
library(KMEANS.KNN)
library(ggforce)

data("USArrests")
res.pca <- prcomp(USArrests,  scale = TRUE)

#1. What is the cumulative variance of the first and second principal components?
vari.exp<- summary(res.pca)$importance[2,]

cumla.vari <- sum(vari.exp[1:2])
cumla.vari
#2. Determine, what are the variables by quadrants?
loadings <- res.pca$rotation[,1:2]
loadings
#3. Analyze the quadrants, what does it mean?
quadrants <- apply(loadings, 1, function(x) {
  if (x[1] > 0 & x[2] > 0) {
    return("Quadrant I")
  } else if (x[1] < 0 & x[2] > 0) {
    return("Quadrant II")
  } else if (x[1] < 0 & x[2] < 0) {
    return("Quadrant III")
  } else {
    return("Quadrant IV")
  }
})
quadrants

#Interpretation of quadrants
#Quadrant I: Variables in this quadrant are positively correlated with both principal components.
#Quadrant II: Variables in this quadrant have a negative correlation with PC1 and a positive correlation with PC2.
#Quadrant III: Variables in this quadrant are negatively correlated with both principal components.
#Quadrant IV: Variables in this quadrant have a positive correlation with PC1 and a negative correlation with PC2.



# Graph of individuals
# +++++++++++++++++++++

# Default plot
# Use repel = TRUE to avoid overplotting (slow if many points)
fviz_pca_ind(res.pca, col.ind = "#00AFBB",
             repel = TRUE)


# 1. Control automatically the color of individuals
# using the "cos2" or the contributions "contrib"
# cos2 = the quality of the individuals on the factor map
# 2. To keep only point or text use geom = "point" or geom = "text".
# 3. Change themes using ggtheme: http://www.sthda.com/english/wiki/ggplot2-themes

fviz_pca_ind(res.pca, col.ind="cos2", geom = "point",
             gradient.cols = c("white", "#2E9FDF", "#FC4E07" ))

# Color individuals by groups, add concentration ellipses
# Change group colors using RColorBrewer color palettes
# Read more: http://www.sthda.com/english/wiki/ggplot2-colors
# Remove labels: label = "none".
fviz_pca_ind(res.pca, label='none',alpha.ind = 1,
             habillage=row.names(USArrests),
             repel = TRUE, 
             # Don't use default Ellipses!!!!
             # addEllipses = TRUE,
             invisible='quali') +
  # ADD ggforce's ellipses
  ggforce::geom_mark_ellipse(aes(fill = Groups,
                                 color = Groups)) +
  theme(legend.position = 'left') +
  coord_equal()



# Select and visualize some individuals (ind) with select.ind argument.
# - ind with cos2 >= 0.96: select.ind = list(cos2 = 0.96)
# - Top 20 ind according to the cos2: select.ind = list(cos2 = 20)
# - Top 20 contributing individuals: select.ind = list(contrib = 20)
# - Select ind by names: select.ind = list(name = c("23", "42", "119") )

# Example: Select the top 40 according to the cos2
fviz_pca_ind(res.pca, select.ind = list(cos2 = 40))


# Graph of variables
# ++++++++++++++++++++++++++++

# Default plot
fviz_pca_var(res.pca, col.var = "steelblue")

# Control variable colors using their contributions
fviz_pca_var(res.pca, col.var = "contrib",
         	gradient.cols = c("white", "blue", "red"),
         	ggtheme = theme_minimal())


# Biplot of individuals and variables
# ++++++++++++++++++++++++++
# Keep only the labels for variables
# Change the color by groups, add ellipses
fviz_pca_biplot(res.pca, label = "var", habillage=row.names(USArrests),
            	addEllipses=TRUE, ellipse.level=0.95,
            	ggtheme = theme_minimal())
