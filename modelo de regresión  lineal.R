## Regresión Lineal _y = mx + b_
install.packages("tidyverse")
library(tidyverse)


grasas <- read.table('http://verso.mat.uam.es/~joser.berrendero/datos/EdadPesoGrasas.txt', header = TRUE)
names(grasas)
# Gráfico de correlación
pairs(grasas)

# Para cuantificar el grado de relación lineal, calculamos la matriz de coeficientes de 
# correlación:

cor(grasas)

# Aplicamos minimos cuadrados

regresion <- lm(grasas ~ edad, data = grasas)
summary(regresion)

# Graficamos

plot(grasas$edad, grasas$grasas, xlab='Edad', ylab='Grasas')
abline(regresion)

anova(regresion)

residuos <- rstandard(regresion)
valores.ajustados <- fitted(regresion)
plot(valores.ajustados, residuos)


qqnorm(residuos)
qqline(residuos)

