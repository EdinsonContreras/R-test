#analisis descriptivo 
library(tidyverse)

View(iris)
iris

attach(iris)
table(Species)
(table(Species)/length(Species))*100 #muestra la tabla de porcentajes
prop.table(table(Species)) #obtinene las funciones relativas
barp<- barplot(table(Species),main="Especies",col=c("darkblue","cyan","yellow"),border ="red",density =80 ,ylab = "Cantidad",xlab = "Especies")
#diagrama de pastel
pastel<- pie(table(Species))
#histograma
hist(Sepal.Length,main = "Longitud de sepalos",col="yellow",xlab="longitud",ylab = "frecuencia")
hist(Sepal.Width,main = "Ancho de sepalos",col="steelblue",xlab="Ancho",ylab = "frecuencia")
hist(Petal.Length,main = "Longitud de petalos",col="magenta",xlab="longitud",ylab = "frecuencia")
hist(Petal.Width,main = "Ancho de petalos",col="steelblue",xlab="ancho",ylab = "frecuencia")

#diagrama de puntos
plot(Sepal.Length,main = "Longitud de sepalos",col="yellow",xlab="longitud",ylab = "frecuencia")

plot(Sepal.Width,main = "Ancho de sepalos",col="steelblue",xlab="Ancho",ylab = "frecuencia")
plot(Petal.Length,main = "Longitud de petalos",col="magenta",xlab="longitud",ylab = "frecuencia")
plot(Petal.Width,main = "Ancho de petalos",col="steelblue",xlab="ancho",ylab = "frecuencia")

#Es para mostra varios graficos
par(mfrow=c(2,2))
#Es para mostrar uno solo grafico
par(mfrow=c(1,1))
#digrama de cajas 
boxplot(Sepal.Length,col = "magenta")
boxplot(Sepal.Width,col="steelblue")
boxplot(Petal.Length,col="magenta")
boxplot(Petal.Width,col="steelblue")

#Es para mostra varios graficos
par(mfrow=c(2,2))
#Es para mostrar uno solo grafico
par(mfrow=c(1,1))
#diagrama de cajas por grupos
boxplot(Sepal.Length~Species,col = "magenta")
boxplot(Sepal.Width~Species,col="steelblue")
boxplot(Petal.Length~Species,col="magenta")
boxplot(Petal.Width~Species,col="steelblue")
#medidas descriptivas
summary(Sepal.Length) #resumen estadistico de los datos
mean(Sepal.Length)# valor promedio
median(Sepal.Length) #mediana de los datos
library(modeest)#libreria para calcular la moda
mlv(Sepal.Length,method = "mfv")# comando para calcular la moda
mlv(Petal.Length,method = "mfv")
var(Sepal.Length)#obtinene la varianza de los datos
sd(Sepal.Length)#obtiene la desviacion estandar
cvsl<- (sd(Sepal.Length)/mean(Sepal.Length))*100
cvsl
cv<- function(dato){cv=(sd(dato)/mean(dato))*100;return(cv)}
cv(Sepal.Length)
cv(Sepal.Width)
cv(Petal.Length)
cv(Petal.Width)
#medidas de forma 
library(moments)

skewness(Sepal.Length)#obtiene el coeficiente de asimetria
skewness(Sepal.Width)
skewness(Petal.Length)
skewness(Petal.Width)
#diagram de coerealcion 
cor(Sepal.Length,Sepal.Width) #obtiene la corelacion entre dos variables
plot(Sepal.Length,Sepal.Width)
cor(iris[-5])
plot(Sepal.Width,Petal.Length)
modelo1<- lm(Sepal.Width~Petal.Length)#obtiene un modelo de regrecion lineal

abline(modelo1)#ajusta la recta 
modelo2<- lm(Petal.Length~Petal.Width)#obtiene un modelo de regrecion lineal
abline(modelo2)#ajustar la recta
help("barplot")
