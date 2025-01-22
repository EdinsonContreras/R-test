NOMBRE= c("juan","manuel","jose","luis","ana","gabriel","lina","luz")
SALARIO= c(120,180,500,130,156,135,135,230)
GENERO= c("M","M","M","M","F","M","F","F")

db <- data.frame(SALARIO,GENERO)

row.names(db) <- (NOMBRE)

#b. Mediante comandos en R, obtenga el nombre de la persona con el salario más alto.
maxs <- max(db$SALARIO)
for (i in 1:length(db$SALARIO)) {
  if(db$SALARIO[i]>=maxs) print(paste(row.names(db)[i],db$SALARIO[i]))
}
#c. Usando comandos de R, muestre los resultados de Lina.
#Use las funcionesgrepl y grep te permiten buscar coincidencias de patrones dentro de un vector de caracteres.
for(i in 1:length(row.names(db))){
  if(grepl(row.names(db)[i],"lina")==TRUE) print(paste(row.names(db)[i],db$SALARIO[i],db$GENERO[i]))
}
#d. usando comandos de R muestre los resultados para las personas que son de género
#Femenino.
for (i in 1:length(db$GENERO)) {
  if(grepl(db$GENERO[i],"F")==TRUE) print(paste(row.names(db)[i],db$SALARIO[i],db$GENERO[i]))
}
#e. usando comandos de R muestre los resultados de las personas que tienen un salario
#superior a 200.

for (i in 1:length(db$SALARIO)) {
  if(db$SALARIO[i]>=200) print(paste(row.names(db)[i],db$SALARIO[i],db$GENERO[i]))
}
#2. Realice una función que obtenga el valor de la factorial de un número.
numfactor =function(x) {y <- 1  
for(i in 1:x){
  y <- y*((1:x)[i])
}
print(y)
}

numfactor(4)
#se comprueba se es correcto 
factorial(4)
#3. Obtenga una función que indique el tipo de sobrepeso de un paciente de acuerdo con
#su índice de masa corporal.

imc = function(peso,altura) {ic =peso/(altura^2)
  if(ic<18.5)print(paste("Su IMC es bajo peso",ic)) 
      else if(ic<=24.9)print(paste("SU IMC es Peso Normal",ic)) 
        else if(ic>=25 && ic<=26.9)print(paste("Su IMC es SobrePeso grado I",ic))
         else if(ic>=27 && ic<=29.9)print(paste("Su IMC es SobrePeso grado II",ic))
          else if(ic>=30 && ic<=34.9)print(paste("Su IMC es Obesidad tipo I",ic))
            else if(ic>=35 && ic<=39.9)print(paste("Su IMC es Obesidad tipo II",ic))
              else if(ic>=40 && ic<=49.9)print(paste("Su IMC es Obesidad tipo III(móbida)",ic))
                else if(ic>=50)print(paste("Su IMC es Obesidad tipo IV(Extrema)",ic))
}

imc(71.9,1.66)
