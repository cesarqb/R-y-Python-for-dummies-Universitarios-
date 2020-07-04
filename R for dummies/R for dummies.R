
#=======================================================
rm(list = ls())
gc()
#=======================================================

#Estructuras de datos con R
x <- c(3,6,8)
y<-c(1,1,1)

z <-2*x-y/2
z

nombres <- c("perro","gato","conejo")
boll <- c(TRUE,FALSE,TRUE)


#Secuencias
seq(1:20)

rep(1,5)

#Ordenar 
x<-c(2,6,3,5,8,22,31,10)
sort(x)

muestra = sample(c(18:65),200,replace = TRUE)

length(muestra)
mean(muestra)
median(muestra)
sd(muestra)
sd(muestra)^2

sum((muestra - mean(muestra))^2)/(length(muestra)-1)
muestra
sort(muestra)

#Arreglos de matrices
matrix(c(2,3,4,5), nrow = 4)
matrix(c(2,3,4,5), ncol = 4)

mat = matrix(c(3,4,5,6,7,8,4,5,6,7,8,9), nrow = 4,ncol = 3)

valor = matrix(1:12, nrow = 4)
ejem = matrix(c(2,2,2), ncol = 1)


###===###====###=====Praticando matrices




#funciones
#===========================
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



#Trabajando con dataframes
#===========================
setwd("C:/....../Sesión 01 - Introducción")
datos = read.csv("Base Credit Card.csv")

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

datos = read.csv("Base Credit Card.csv")

#Analisis inicial

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

#Retiramos los valores atípicos
#===============================
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
boxplot(datos$EDAD,main = "edad imput0", col = 2)
boxplot(datos$EDAD2,main = "edad imput1", col = 3)
boxplot(datos$EDAD3,main = "edad imput2", col = 4)

#Gráficos
#==#==#==#==
par(mar = c(1.75,1.75,1.75,1.75))

par(mfrow = c(1,1))

hist(datos$EDAD)

plot(datos$EDAD)

boxplot(datos$EDAD)



#Gráficos con ggplot2
#==#==#==#==#==#==#==
str(datos)

library(ggplot2)#ggplot2 trabaja por capas

#barras
ggplot(datos, aes(x=ZONA)) + geom_bar()#cuadro datos

gg1 = ggplot(datos, aes(x=ZONA)) + geom_bar()
gg1 + xlab("Zona") + ylab("Número de casos")#añadimos una capa mas

gg1 = ggplot(datos,aes(ZONA))+ geom_bar(width = 0.5, fill = "blue") #cambiamos el ancho de las barras
gg1 + xlab("Zona") + ylab("Número de casos")
gg1 + ggtitle("Frecuencia por zonas")

gg1 = ggplot(datos,aes(SEXO, fill = ZONA))+ geom_bar(position = "dodge")
gg1 + xlab("Zona") + ylab("Número de casos")
gg1 + ggtitle("Frecuencia por zonas")


#CLLUSTER: k-mean
#================
library(datasets)
library(cluster)
#install.packages("factoextra")
library(factoextra)

head(iris)

iriscluster <- kmeans(iris[,1:4],3,nstart = 20)
table(iriscluster$cluster,iris$Species)

datos = data.frame(iris)
datos$cluster = iriscluster$cluster

ggplot(datos, aes(Petal.Length,Petal.Width, color = datos$cluster)) + geom_point()

fviz_cluster(iriscluster,data = iris[,-5])

#Número optimo de clusters
fviz_nbclust(iris[,-5],kmeans,method = "wss")
fviz_nbclust(iris[,-5],kmeans,method = "silhouette")


#ARBOLES DE CLASIFICACION
#========================
#https://rpubs.com/jboscomendoza/arboles_decision_clasificacion

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

datos = read.csv("Base Credit Card.csv")


library(rpart)
library(rpart.plot)
library(caret)

names(datos)
#par(mar = c(1,1,1,1))
par(mfrow = c(1,1))
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

#Correlación
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


#REGRESION LOGISTICA
#===================
summary(datos$Life_expentacy)
datos$Y[datos$Life_expentacy >= 69] = 0
datos$Y[datos$Life_expentacy < 69] = 1

logi01 <- glm(Y ~ GDP + HIV,
              data = datos,family = "binomial")

summary(logi01)





