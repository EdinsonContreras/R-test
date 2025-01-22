#Mirar cual es el folder de trabajo actual y asegurarse que en el est? el documento de la obra
getwd()

#Lee el texto desde el archivo "melville.txt" y lo coloca en un vector
text.v <- scan("C:/Users/Edinson Contreras/Documents/meta.txt", what="character", sep="\n")

#Imprime el texto desde el archivo que ha sido colocado en el vector text.v  
text.v

# Imprime la primera y segunda l?nea del vector text.v
text.v [1]
text.v [2]

# Guarda lal?nea del texto en donde inicia la obra en start.v
start.v <- which(text.v == "One morning, when Gregor Samsa woke from troubled dreams, he found")
start.v

# Guarda lal?nea del texto en donde finaliza la obra en start.v
end.v <- which(text.v == "first to get up and stretch out her young body.")
end.v

# Las siguientes l?neas guardan en variables el texto del metadato de la obra
# El metadato tiene dos partes, una al principio de la obra y una al final
# El metadato del principio de la obra inicia en la l?nea 1 y finaliza en la l?nea anterior a que comience la obra (start.v -1)

start.metadata.v <- text.v[1:start.v -1]
start.metadata.v

# El metadato del final de la obra inicia en la l?nea (end.v+1) y finaliza en la ?ltima de la obra
end.metadata.v <- text.v[(end.v+1):length(text.v)]
end.metadata.v

# Colocamos todo el metadato de la obra en la variable (metadata.v)
metadata.v <- c(start.metadata.v, end.metadata.v)
metadata.v

# Ahora, las lineas de la novela son las que se encuentra entre la l?nea de inicio (start.v) y la de fin de la obra (end.v)
# Colocaremos est?s l?neas en la variable (novel.lines.v)
novel.lines.v <-  text.v[start.v:end.v]
novel.lines.v

class(novel.lines.v)

# La variable novel.lines.v almacena en l?neas la novela, con la funci?n paste se uniran todas las l?neas de la novela, separandolas con un espacio " "
novel.v <- paste(novel.lines.v, collapse=" ")
novel.v

class(novel.v)

# La funcion tolower transforma el texto en min?scula
novel.lower.v <- tolower(novel.v)
novel.lower.v

#strsplit divide la novela en palabras, esta funci?n genera una lista que es un tipo de dato en R
moby.words.l <- strsplit(novel.lower.v, "\\W")
moby.words.l

class(moby.words.l)

#la funci?n unlist transforma la lista en vector, esto es necesario porque hay funciones que aplican a ciertos tipos de datos en R
moby.word.v <- unlist(moby.words.l)
moby.word.v
class(moby.word.v)

#quitar conectores
library(tm)
moby.word.v <- removeWords(moby.word.v, words = stopwords(kind = "en"))
moby.word.v


#Las siguientes l?neas permitiran eliminar los caracteres blancos ""
#Se identifica donde est?n los espacios diferentes a espacios en blanco
not.blanks.v  <-  which(moby.word.v!="")
not.blanks.v

#La obra sera ahora todas las palabras diferentes a los espacios en blanco
moby.word.v <-  moby.word.v[not.blanks.v]
moby.word.v



#Ahora identificaremos las veces que aparece la palabra "whale" en la obra
whales.v <- which(moby.word.v=="gregor")
whale.hits.v <- length(moby.word.v[which(moby.word.v=="gregor")])
whale.hits.v

#Calculamos el n?mero total de palabras en la obra
total.words.v <- length(moby.word.v)
total.words.v

#Calculamos la proporci?n de la palabra whale con respecto al n?mero total de palabras en la obra
whale.hits.v/total.words.v

#La funci?n table calcula la frecuencia de cada palabra
moby.freqs.t <- table(moby.word.v)
moby.freqs.t

#La funci?n sort ordena decrecientemente la frecuencia de la palabra
sorted.moby.freqs.t <- sort(moby.freqs.t , decreasing=T)

#Se imprimer la lista de frecuencia
sorted.moby.freqs.t[1:20]

#Se visualizan las 20 palabras con mayor frecuencia
plot(sorted.moby.freqs.t[1:20],main = "Palabras mas frecuentes")

#Vamos a colocar el termino NA en cada palabra de la novela
w.count.v1<-rep(NA,length(moby.word.v))
length(w.count.v1)
w.count.v1

#Vamos a colocar el n?mero -1 cada vez que aparezca la palabra whale en el texto
w.count.v1[whales.v]<-1
w.count.v1

#Dibujamos el gr?fico de dispersi?n de la palabra whale en la novela
plot(w.count.v1, main = "Dispersion", xlab = "Novel time", ylab = "gregor", type = "h", ylim=c(1,0), yaxt = "n")

ahab.v <- which(moby.word.v=="room")
ahab.hits.v <- length(moby.word.v[which(moby.word.v=="room")])
ahab.hits.v/total.words.v

a.count.v2 <- rep(NA,length(moby.word.v))
a.count.v2[ahab.v]<-1
plot(a.count.v2, main = "Dispersion", xlab = "Novel time", ylab = "ahab", type = "h", ylim=c(1,0), yaxt = "n")

whales.v
ahab.v



plot(w.count.v1, col= "royalblue", xlab = "Línea de tiempo", ylab = "", type
     = "h", ylim=c(1,0), yaxt = "n")

par(new=TRUE)
plot(a.count.v2, col="red1" , xlab = "Línea de tiempo", ylab = "", type =
       "h", ylim=c(1,0), yaxt = "n")

title(main="Apariciones de gregor y room")
mtext("room", side = 4, col="red1")
mtext("gregor", side = 2, col="royalblue")




whales.ahabs.m <-cbind(whales.v, ahab.v)
dim(whales.ahabs.m)

whales.ahabs.m
colnames(whales.ahabs.m)<-c("gregor","room")

whales.ahabs.m
class(whales.ahabs.m)

barplot(whales.ahabs.m, main = "Bar plot gregor y room", beside = TRUE, col = c("darkgrey", "darkblue"))



