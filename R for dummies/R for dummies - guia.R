
#=======================================================
rm(list = ls())
gc()

cat("\014")
#=======================================================

a <- 3

#Estructuras de datos con R
x <- c(a,a,8)
y<-c(1,1,1)

t <- c('cesar')
class(t)

z <-2*x-y/2
z

nombres <- c("perro","gato","conejo")
nombres[4]

boll <- c(TRUE,FALSE,TRUE)

FALSE & FALSE

#Secuencias
x_1 = seq(1:20)
x_2 = seq(1,20, by = 2)

vec = c(x_2)



rep(1,5)
rep(c(1,3), time = 2)

#Ordenar 
x<-c(2,6,3,5,8,22,31,10)
sort(x)
sort(x, decreasing=T)
sort(x, decreasing=TRUE)

muestra = sample(c(18:65),200,replace = TRUE)

length(muestra)
mean(muestra)
median(muestra)
sd(muestra)
sd(muestra)^2


# listas

lista01 = list(x,y,boll,muestra)

lista01[[3]]



summary(muestra)

sum((muestra - mean(muestra))^2)/(length(muestra)-1)
muestra
sort(muestra)

#Arreglos de matrices
matrix(c(2,3,4,5), nrow = 4)
matrix(c(2,3,4,5), ncol = 4)

mat = matrix(c(3,4,5,6,7,8,4,5,6,7,8,9), nrow = 4,ncol = 3)
colnames(mat) = c("x1","x2","x3")
mat

rownames(mat) = c("c1","c2","c3","c4")
mat

dim(mat)
nrow(mat)
ncol(mat)
length(mat)
diag(mat)

rowSums(mat)
colSums(mat)

rowMeans(mat)
colMeans(mat)

t(mat)
mat

colnames(mat) = c("x1","x2","x3")
mat

valor = matrix(1:12, nrow = 4)
ejem = matrix(c(2,2,2), ncol = 1)

dim(mat)
dim(ejem)

matsum = mat+valor
matdif = mat - valor
matprod = mat*valor

mat%*%ejem


matprod[,3]
matprod[2,]

###===###====###=====Praticando matrices
A = matrix(c(1,2,3,1,2,3,1,2,3), nrow = 3)
B = matrix(rep(1,9), nrow = 3)

A
B

B[,1] = A[,1]
B[,2] = A[,2]
B[,3] = A[,1]+A[,2]

B

a1 = c(1,1,1)
a2 = c(2,2,2)
a3 = c(3,3,3)
a4 = c(4,4,4)

X = rbind(a1,a2,a3)

a2 = rep(2,3)

cbind(a1,a2,a3)


#funciones
cuadrado = function(x){x^2}
cuadrado(100)

f_suma = function(n){
  resu = n*(n-1)/2
  return(resu)
}

f_suma(200)

f_IC = function(me,varianza,n){
  inf = me - 1.96*(varianza/(n-1))^0.5
  sup = me + 1.96*(varianza/(n-1))^0.5
  
  cat("media IC = ",inf,"-",sup)
}

f_IC(23,230,30)

###===###====###=====Praticando funciones
f_IC2=function(x){
  promedio = mean(x)
  varianza = sd(x)^2
  longi = length(x) -1
  
  inf = promedio - 1.96*(varianza/longi)^0.5
  sup = promedio + 1.96*(varianza/longi)^0.5
  cat("media IC = ",inf,"-",sup)
}

f_IC2(muestra)

muestra2 = sample(c(18:65),800,replace = TRUE)

f_IC2(muestra2)


#Trabajando con dataframes
#==========================
setwd("D:/CESAR QUEZADA/03. Proyectos DMC/03. Proyecto Cursos DMC para universitarios (CERRADO)/R para Universitarios/Version 03/Grupo 03")
datos = read.csv("Base Credit Card.csv")

#limpiando nuestro entorno de trabajo
rm(list = ls())

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
datos = read.csv("Base Credit Card.csv")

#Analisis inicial
View(datos)

str(datos)
names(datos)

mean(datos$LINEA_CREDITO)
median(datos$LINEA_CREDITO)
quantile(datos$LINEA_CREDITO)
summary(datos$LINEA_CREDITO)
summary(datos$EDAD)

#Analizando valores perdidos
#===========================
(186/length(datos$EDAD))*100

promedio = mean(datos$EDAD[!is.na(datos$EDAD)])

datos$EDAD2 = datos$EDAD
datos$EDAD2[is.na(datos$EDAD2)] = promedio

Q1 = quantile(datos$EDAD[!is.na(datos$EDAD)],0.25)
Q3 = quantile(datos$EDAD[!is.na(datos$EDAD)],0.75)

LS = Q3 + 1.5*(Q3-Q1)
LI = Q1 - 1.5*(Q3-Q1)

datos$EDAD3 = datos$EDAD
datos$EDAD3[datos$EDAD3>=LS] = NA

summary(datos$EDAD3)
summary(datos$EDAD)

promedio2 = mean(datos$EDAD3[!is.na(datos$EDAD3)])
datos$EDAD3[is.na(datos$EDAD3)] = promedio2

par(mar=c(3, 3, 3, 3))
hist(datos$EDAD2)
hist(datos$EDAD3)

par(mfrow = c(1,3))
#boxplot(datos$EDAD,main = "edad imput0", col = 2)
boxplot(datos$EDAD2,main = "edad imput1", col = 3)
boxplot(datos$EDAD3,main = "edad imput2", col = 4)

#Tratamiento de datos faltantes
#=============================
summary(datos$EDAD)
boxplot(datos$EDAD)
dim(datos)

datos <- datos[!is.na(datos$EDAD),]
dim(datos)

datos <- na.omit(datos)

x = 4.555

round(x,digits = 0)

#Datos at?picos 
#==#==#==#==#==#==#
summary(datos$EDAD)
boxplot(datos$EDAD)

#Reemplazar los valores at?picos
media = mean(datos$EDAD[!is.na(datos$EDAD)])
datos$edad2 = datos$EDAD
datos$edad2[is.na(datos$EDAD)] = media 

par(mfrow = c(1,2))
boxplot(datos$EDAD, main = "EDAD real", col  = 3)
boxplot(datos$edad2, main = "EDAD imputado", col = 2)


Q3 = quantile(datos$edad2,0.75)
Q1 = quantile(datos$edad2,0.25)

datos$EDAD3 = datos$edad2
datos$EDAD3[datos$EDAD3>(Q3+1.5*(Q3-Q1))] = (Q3+1.5*(Q3-Q1))

par(mfrow = c(1,2))
boxplot(datos$edad2, main = "edad imputado", col  = 3)
boxplot(datos$EDAD3, main = "edad sin at?picos", col = 2)


#Gr?ficos
#==#==#==#==
par(mfrow = c(1,3))
#par(mar = c(1.5,1.5,1.5,1.5))

hist(datos$EDAD, xlim = c(0,100),ylim = c(0,190), 
     xlab = "EDAD", ylab = "Frecuencia", main = "histograma",col  = 3)

plot(datos$EDAD, datos$Total_facturacion, xlim = c(0,100),ylim = c(0,190), 
     xlab = "edad", ylab = "Frecuencia", main = "Dispersi?n")

boxplot(datos$EDAD,xlab = "edad", 
        ylab = "Frecuencia", main = "Caja")

#install.packages("plotrix")


#Gr?ficos con ggplot2
#==#==#==#==#==#==#==
str(datos)

library(ggplot2)#ggplot2 trabaja por capas

#barras
ggplot(datos, aes(x=ZONA)) + geom_bar()#cuadro datos

gg1 = ggplot(datos, aes(x=ZONA)) + geom_bar()
gg1 + xlab("Zona") + ylab("N?mero de casos")#a?adimos una capa mas

gg1 = ggplot(datos,aes(ZONA))+ geom_bar(width = 0.5, fill = "blue") #cambiamos el ancho de las barras
gg1 + xlab("Zona") + ylab("N?mero de casos")
gg1 + ggtitle("Frecuencia por zonas")

gg1 = ggplot(datos,aes(SEXO, fill = ZONA))+ geom_bar(position = "dodge")
gg1 + xlab("Zona") + ylab("N?mero de casos")
gg1 + ggtitle("Frecuencia por zonas")

#Histogramas
ggplot(datos, aes(P19A)) + geom_histogram()

hist1 <- ggplot(datos, aes(P19A)) + geom_histogram(fill = "Red")
hist1 + facet_grid(SEXO ~.)


#CLLUSTER: k-mean
#================
library(datasets)
library(cluster)
library(factoextra)

head(iris)

table(iris$Species)

iriscluster <- kmeans(iris[,3:4],2,nstart = 20)
table(iriscluster$cluster,iris$Species)

datos = data.frame(iris)
datos$cluster = iriscluster$cluster

ggplot(datos, aes(Petal.Length,Petal.Width, color = datos$cluster)) + geom_point()

fviz_cluster(iriscluster,data = iris[,-5])

#N?mero optimo de clusters
fviz_nbclust(iris[,-5],kmeans,method = "wss")
fviz_nbclust(iris[,-5],kmeans,method = "silhouette")


#ARBOLES DE CLASIFICACION
#========================
#https://rpubs.com/jboscomendoza/arboles_decision_clasificacion

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
datos = read.csv("Base Credit Card.csv")


library(rpart)
library(rpart.plot)
library(caret)

names(datos)
#par(mar = c(1,1,1,1))
#par(mfrow = c(1,1))
arbol1 <- rpart(formula = TIPO_TARJETA~SEGMENTO_BANCO+
                  ECIVIL , data = datos)
rpart.plot(arbol1)


#REGRESION LINEAL
#================
datos = read.csv('Base ciudades.csv')
str(datos)

#Normalidad
#par(mar = c(1.5,1.5,1.5,1.5))
#par(mfrow = c(2,2))

hist(datos$GDP, main = "GDP")
qqnorm(datos$GDP,main = "GDP")
shapiro.test(datos$GDP)


#Homocedasticidad
ggplot(datos,aes(x=GDP, y=HIV))+geom_point()

#Correlaci?n
pairs(x = datos[,-1], lower.panel = NULL)
cor(x = datos[,-1],method = "pearson")

#Regresion lineal simple
regre01 <- lm(Life_expentacy ~ GDP,
              data = datos)
summary(regre01)

regre02 <- lm(Life_expentacy ~ HIV,
              data = datos)
summary(regre02)

regre03 <- lm(Life_expentacy ~GDP + HIV,
              data = datos)
summary(regre03)

regre04 <- lm(Life_expentacy ~ log(GDP + HIV),
              data = datos)
summary(regre04)




#REGRESION LOGISTICA
#===================
summary(datos$Life_expentacy)
datos$Y[datos$Life_expentacy >= 69] = 0
datos$Y[datos$Life_expentacy < 69] = 1

logi01 <- glm(Y ~ GDP + HIV,
              data = datos,family = "binomial")

summary(logi01)





