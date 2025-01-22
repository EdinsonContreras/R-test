sqrt(8)
abs(-3)
edad <- c(21,22,23,25,40,30)
salario <- c(5000000,1700000,0,0,1000000,0)
nombre <- c("mauricio","rosa","mauro","yuranis","eddie","andi")
genero <- c("m","f","m","f","m","m")
length(salario)
base <- data.frame(edad,salario,nombre,genero)
row.names(base)<- nombre #asginia un vector de nombre a las filas 
colnames(base)
base[1,1]
base[1,]
base[,2]
attach(base)
base$edad
class(base)
class(edad)
class(genero)
a <- matrix(1:9, nrow = 3)
b<- matrix(base,ncol=4, byrow = FALSE)
c<- matrix(base,nrow = 4)
dim(a)
x<- matrix(2:10,nrow = 3)
v<- matrix(3:11, nrow = 3)
a %*% x 
a + x
a - x 
solve(a)
det(a)
#Let’s try to get the inverse of the matrix by using the solve function.
#solve(mat)
#solve(mat)Error in solve.default(mat) : 
 # Lapack routine dgesv: system is exactly singular: U[2,2] = 0
#The error occurs because mat is a singular matrix. It is not possible to invert a singular matrix. LAPACK is a Linear Algebra package used underneath solve(). DGESV computes the solution to a real system of linear equations A * X = B.
#https://researchdatapod.com/how-to-solve-r-error-in-solve-default-lapack-routine-dgesv-system-is-exactly-singular/
solve(a)%*%a
t(a)
#https://r-coder.com/matrix-operations-r/
crossprod(a,x)
tcrossprod(a,x)
diag(a)
diag(x)
sum(diag(a))
k <- matrix(c(2,3,4,5,6,7,8,9,4,5,9,7,5),ncol = 3)
df <- as.data.frame(k)
colnames(df) <- c("primera","segunda","tercera")
rownames(df) <- c("f1","f2","f3","f4","f5")
