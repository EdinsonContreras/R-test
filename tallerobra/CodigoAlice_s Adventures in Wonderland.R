#Mirar cual es el folder de trabajo actual y asegurarse que en el est? el documento de la obra
getwd()

#Lee el texto desde el archivo "melville.txt" y lo coloca en un vector
text.v <- scan("C:/Users/Edinson Contreras/Documents/alicia.txt", what="character", sep="\n")

#Imprime el texto desde el archivo que ha sido colocado en el vector text.v  
text.v

# Imprime la primera y segunda l?nea del vector text.v
text.v [1]
text.v [2]

# Guarda lal?nea del texto en donde inicia la obra en start.v
start.v <- which(text.v == "CHAPTER I.", "Down the Rabbit-Hole")
start.v

# Guarda lal?nea del texto en donde finaliza la obra en start.v
end.v <- which(text.v == "THE END")
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
#quitar conectores
library(tm)
novel.lines.v<- removeWords(novel.lines.v, words = stopwords(kind = "en"))
novel.lines.v


chap.positions.v <- grep("^CHAPTER", novel.lines.v)
chap.positions.v

novel.lines.v[chap.positions.v]
novel.lines.v<-c(novel.lines.v,"END")

last.position.v<-length(novel.lines.v)
last.position.v

chap.positions.v<-c(chap.positions.v,last.position.v)
chap.positions.v

for (i in 1:length(chap.positions.v)){
  print (chap.positions.v[i])
  
}

for (i in 1:length(chap.positions.v)){
  print(paste("Chapter",i,"comienza en la posicion",chap.positions.v[i]),sep = "")
  
}
chapter.raws.l <- list()
chapter.freqs.l<- list()

for (i in 1:length(chap.positions.v)){
  if (i != length(chap.positions.v)){
    chapter.title <- novel.lines.v [chap.positions.v[i]]
    startc <- chap.positions.v[i]+1
    endc <- chap.positions.v[i+1]-1
    chapter.lines.v <- novel.lines.v[startc:endc]
    chapter.words.v <- tolower(paste(chapter.lines.v, collapse = ""))
    chapter.words.l <- strsplit(chapter.words.v, "\\W")
    chapter.word.v <- unlist(chapter.words.l)
    chapter.word.v <- chapter.word.v [which(chapter.word.v!="")]
    chapter.freqs.t<-table(chapter.word.v)
    chapter.raws.l[[chapter.title]]<- chapter.freqs.t
    chapter.freqs.t.rel <- 100*(chapter.freqs.t/sum(chapter.freqs.t))
    chapter.freqs.l[[chapter.title]]<-chapter.freqs.t.rel
  }
  
}


chapter.raws.l[[2]]
chapter.raws.l[[2]]


chapter.freqs.l[[1]]["alice"]
chapter.freqs.l[[2]]["alice"]

alice.l <- lapply(chapter.freqs.l,'[','alice')
alice.l

# rbind ->Take a sequence of vector, matrix or data-frame arguments and combine by columns or rows, respectively
alice.m <- do.call(rbind,alice.l)
alice.m
dim(alice.m)

rabbit.l<- lapply(chapter.freqs.l,'[','rabbit')
rabbit.m <- do.call(rbind,rabbit.l)
rabbit.m 

alice.v <- alice.m[,1]
rabbit.v <- rabbit.m[,1]
alice.v
rabbit.v 

barplot(alice.v, col= "royalblue", xlab = "Línea de tiempo", ylab = "",cex.axis=0.5, cex.names=0.5)

par(new=TRUE)

barplot(rabbit.v, col="red1" , xlab = "Línea de tiempo", ylab = "",density=c(30) , angle=c(45),cex.axis=0.5, cex.names=0.5)

title(main="Apariciones de alice y rabbit")
mtext("rabbit", side = 4, col="red1")
mtext("alice", side = 2, col="royalblue")

alice.rabbit.m <- cbind(alice.m, rabbit.m)
dim(alice.rabbit.m)
alice.rabbit.m

colnames(alice.rabbit.m) <- c("alice", "rabbit")
alice.rabbit.m

barplot(alice.rabbit.m, main = "Bar plot alice y rabbit", beside = TRUE, col = c("darkgrey", "darkblue"))



#En la matriz tenemos valores nulos que eliminar
alice.rabbit.m [1:12,]
length(alice.rabbit.m)

#Eliminando los NA
alice.rabbit.m[which(is.na(alice.rabbit.m))] <- 0

alice.rabbit.m [1:12,]
class(alice.rabbit.m)



#calculando la correlaci?n entre alice y rabbit, el resultado es una matriz de correlaci?n
cor(alice.rabbit.m)

#Cuantas 
dim(alice.rabbit.m)
class (alice.rabbit.m)

#Utilizaremos un tipo de dato diferente no una matriz sino un dataframe
cor.data.df <- as.data.frame(alice.rabbit.m)
class(cor.data.df)
cor(cor.data.df)

#Al randomizar la columna alice del DF, cada vez que se ejecuta genera una versi?n randomizada
sample(cor.data.df$alice)

#Calculamos nuevamente la correlaci?n con la versi?n randomizada
cor(sample(cor.data.df$alice), cor.data.df$rabbit)

#Vemos que las correlaciones varian, vamos a almacenar 10000 correlaciones para hacer un mejor an?lisis
cors.v <- NULL
for (i in 1:10000){
  
  cors.v <- c(cors.v, cor(sample(cor.data.df$alice), cor.data.df$rabbit))
  
}
class(cors.v)
cors.v

#Calculamos algunos estadisticos descriptivos
min(cors.v)
max(cors.v)
range(cors.v)
mean(cors.v)
sd(cors.v)

h<- hist(cors.v, breaks=100, col = "blue", xlab = "Coeficientes de correlaci?n", main="Histogramas de coeficientes de correlaci?n", plot =T)


library(ggplot2)
library(shiny)
library(bslib)
library(shinydashboard)

alice.df <- data.frame(alice.v,rabbit.v)

ui <- dashboardPage(
  dashboardHeader(title = "Analisis de Alice's Adventures in Wonderland "),
  
  
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Histograma", tabName = "dashboard", icon = icon("chart-bar")),
      menuItem("Grafico de barras", tabName = "widgets", icon = icon("chart-simple"))
    )
  ),
  
  
  
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                tags$img(
                  src = "https://cdn.culturagenial.com/es/imagenes/las-aventuras-de-alicia-en-el-pais-de-las-maravillas-og.jpg",
                  style = 'position: absolute'
                ),
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                ),
                
                box(plotOutput("plot1", height = 250))
                
                
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              fluidRow(
                 
                box(
                  title = "Controls",
                  selectInput("column", "Choose a column to plot:", 
                              choices = c("alice", "rabbit"))
                ),
                
                box(plotOutput("plot2", height = 400),align = "down")
                
                ) 
                 
                  
            
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  histdata
  
  output$plot1 <- renderPlot({
    alice.df <- histdata[seq_len(input$slider)]
    hist(alice.df, main="Histograma Alicia y conejo")
  })
  
  
  output$plot2<-renderPlot({
    if (input$column== "alice"){
    gga<- ggplot(alice.df,aes(x=row.names(alice.df),y= alice.df$alice.v)) +
      geom_bar(stat = "identity",width = 0.5) + coord_flip()+
      theme_bw(base_size = 20)
    gga<-gga+labs(x="Capitulos",y="Alicia",
                   title="Apariciones de Alicia Por capitulos")
    gga
    
    }else if(input$column== "rabbit"){
     ggr<- ggplot(alice.df, aes(x = row.names(alice.df), y = alice.df$rabbit.v)) +
        geom_bar(stat = "identity",width = 0.5)+ coord_flip()+
        theme_bw(base_size = 20)
     ggr<-ggr+labs(x="Capitulos",y="Conejo",
                    title="Apariciones de Conejo Por capitulos")
     ggr
    }
  })
  
  
  
}

shinyApp(ui, server)


