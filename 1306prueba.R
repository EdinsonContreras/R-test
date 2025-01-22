#operadores logico
2<3
2!=2
a=c(2,3,4)
b=c(2,3,1)
a==a #comparacion elemento de vectores
!a  #
!a(a==b) #
a&b #CONJUCION y 
a|b #DISYUCION o
a<4 #muestra como verdadero los valores menores a  4
a<3&b>2  #coloca como verdadero las posiciones que cumplan las dos condiciones
a<3|b>2 #coloca como verdadero si alguna de las condiciones se cumple
rep (3,5) #repite el numero 3 varias veces
1:10 #genera valores de uno hasta 10 
seq (1:10) #genera valores de uno hasta 10 
seq (1,10) #genera valores de uno hasta 10
seq (1,20,4) #genera un secuencia  de 1 al 20 de 4 en 4
seq (from=1, to=20, by=4)#genera un secuencia  de 1 al 20 de 4 en 4
rep (1:4, 3) #repetir la secuencia de 1 al 4  tres veces
rep (x=1:4,each=3) #repite cada numero 3 veces en su orden class numerico
gl (5,3)  #repite cada numero 3 del 1 al 5veces en su orden class factor
gl (2,3, labels = c("cartagena", "barranquilla")) #remplaza los valores por  los mencionados en el labels 
gl (2,3, labels = c("cartagena", "barranquilla"), length = 10) #repite las veces que este determinado en el length =

pi<3.14156472
round(pi,4) #redondea a sus cuatro primeros decimales
ceiling(pi) #genera el minimo enteto  mayor a pi
trunc (pi) #obtinene la parte entera del numero
b
sort(b) #ordena los elementos del vector de mayor a menor
rank (b) #muestra el valor en cuanto a la posicion comparando de mayor a menor
b[b>2] # muestra los valores del vector que son mayores a  2
b[b==7]
c=c(4,5,6) 
c[c>4&c<6]


#creacion de funciones
x=?
  y=3+5*x
recta = function(x){y=5*x+3
return(y)
}

recta(1)

#calcular area de un triangulo

area = function(b,h){area=(b*h)/2 
return(area)
} 
area (6,4) 

#CARCULAR NOTA FINAL  DONDE EL QUIZ VALE 30%, TALLER 20% Y EXAMEN 50% e IMPRIMIR CON COMENTARIO
notafinal =function(q,t,e) {NF=((q*0.30)+ (t*0.20) + (e*0.50))
return(paste("la nota final es ",NF))
}

nf<- notafinal (3.3,5,4)




if(3<4){"verdadero"} else {"falso"}


if(nf<=4){"aprobado"}else{"reprobado"}


X  = function(n) {f=(if(n=0)print("Es igual a cero",n) else if(n>0)print("numero positivo",n) else if(n<0)print("numero negativo",n))  }

x(-3)

#ciclo for 

for (i in 1:10) {
print(i*2)
  }
vector<-c ()
vector[i]<- 1
for (i in 2:10) {vector[i]=vector(i-1)*3}
plot(vector)

edad = c(21,21,22,23,25,46,34,30)
salario= c(5000000,1700000, 3000000,0,0,0,8500000,1900000)
nombre = c("JOSE" ,"mauricio", "rosa", "yeirys", "mauro", "yuranis", "eddie", "andy")
Genero = c("M","M","F","F","M","F","M","M")
df <-data.frame(edad,salario,Genero)

row.names(df) <-(nombre)

for (i in 1:length(df$salario)) {
  if(df$salario[i]>=4000000) print(row.names(df)[i],df$salario[i])  
}



