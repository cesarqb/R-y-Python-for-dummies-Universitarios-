#=======================================================
rm(list = ls())
gc()
#=======================================================

#Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#setwd('/Estudio de caso')
base = read.csv('Estudio de caso/EastWestAirlinesNN.csv')

names(base)

names(base) = c("ID","Top","Premio","Cant_miles","cc1_miles","cc2_miles","cc3_miles",
                "Bonus_miles","Bonus_trans","Flight_miles_12mo","Flight_trans_12","Online_12",
                "Email","Club_member","Any_cc_miles_12mo","target")

str(base)

#Sepramos la base de datos seg?n el tipo de variable
numericos = c("Premio","Bonus_miles","Bonus_trans","Flight_miles_12mo","Online_12")
dicotomicos = c("Top","cc1_miles","cc2_miles","cc3_miles","Email","Club_member","Any_cc_miles_12mo")
target = c("target")


#analizamos la dimencion de la base de datos y de la variable target
prop.table(table(base$target))

summary(base$target)
table(base$target)/sum(table(base$target))



#Tratamiento de variables dicot?micas
#========#========#========#========
summary(base[dicotomicos])

#Eliminados los valores perdidos
base = base[!is.na(base$Top),]


#Obtenemos frecuencias
prop.table(table(base$Top))
prop.table(table(base$cc1_miles))
prop.table(table(base$cc2_miles))
prop.table(table(base$cc3_miles))
prop.table(table(base$Email))
prop.table(table(base$Club_member))
prop.table(table(base$Any_cc_miles_12mo))


par(mar = c(1,1,1,1))
par(mfrow = c(2,4))

pie(table(base$Top), main = "Top")
pie(table(base$cc1_miles), main = "cc1")
pie(table(base$cc2_miles), main = "cc2")
pie(table(base$cc3_miles), main = "cc3")
pie(table(base$Email), main = "email")
pie(table(base$Club_member), main = "club")
pie(table(base$Any_cc_miles_12mo), main = "cc")

#Probamos nuevas variables
table(base$cc2_miles,base$cc3_miles)

#Creamos una nueva variable
base$Online_dm = base$cc2_miles + base$cc3_miles
table(base$cc2_miles,base$Online_dm)
table(base$cc3_miles,base$Online_dm)
table(base$Online_dm)

pie(table(base$Online_dm), main = "Online")


windows()
par(mfrow = c(2,4))
#Cruzamos con la variable target
mosaicplot(base$Top ~ base$target, col = T, main = "Top")
mosaicplot(base$cc1_miles ~ base$target, col = T, main = "cc1")
mosaicplot(base$cc2_miles ~ base$target, col = T, main = "cc2")
mosaicplot(base$cc3_miles ~ base$target, col = T, main = "cc3")
mosaicplot(base$Email ~ base$target, col = T, main = "email")
mosaicplot(base$Club_member ~ base$target, col = T, main = "club")
mosaicplot(base$Any_cc_miles_12mo ~ base$target, col = T, main = "cc")
mosaicplot(base$Online_dm ~ base$target, col = T, main = "online")

table(base$Club_member, base$target)

#Aderimos la variable "Online_dm" a nuestro vector "dicotomicos"
dicotomicos = c(dicotomicos,"Online_dm")

#######Prueba de igualdad de proporciones
#H0: la variable A es independiente de la variable B
#H1: la variable A es dependiente de la variable B
#######Si el p_value < 0.05 rechazamos la hipotesis nula

chisq.test(prop.table(table(base$Top)))
chisq.test(prop.table(table(base$cc1_miles)))

#Eliminados las variables que no aportan
dicotomicos = dicotomicos[-c(3,4)]


#Tratamiento variables num?ricas
#========#========#========#====
#========#========#========#====
summary(base[numericos])

#Eliminados valores perdidos
base = base[!is.na(base$Premio),]

#An?lisis gr?fico inicial
windows()
par(mar = c(1,1,1,1))
par(mfrow = c(1,5))
hist(base$Premio, main = "Premio")
hist(base$Bonus_miles, main = "Bonus_miles")

windows()
par(mar = c(1,1,1,1))
par(mfrow = c(1,5))
boxplot(base$Premio, main = "Premio")
boxplot(base$Bonus_miles, main = "Bonus_miles")

#Analisis de correlaci?n
library(PerformanceAnalytics)

#Analizamos los valores perdidos
#=========#===========#==========
Q3 = quantile(base$Premio,0.75)
Q1 = quantile(base$Premio,0.25)

LI = Q1 - 1.5*(Q3-Q1)
LS = Q3 + 1.5*(Q3-Q1)

base$Premio2 = base$Premio
base$Premio2 = ifelse(base$Premio > LS,1,0)
prop.table(table(base$Premio2))

#Imputamos los valores at?picos
Q3 = quantile(base$Flight_miles_12mo,0.75)
Q1 = quantile(base$Flight_miles_12mo,0.25)

LI = Q1 - 1.5*(Q3-Q1)
LS = Q3 + 1.5*(Q3-Q1)

base$Flight_miles_12mo2 = base$Flight_miles_12mo
base$Flight_miles_12mo2 = ifelse(base$Flight_miles_12mo2>= LS,NA,base$Flight_miles_12mo)
summary(base$Flight_miles_12mo2)

base$Flight_miles_12mo2[is.na(base$Flight_miles_12mo2)] = LS

windows()
par(mfrow = c(1,2))
boxplot(base$Flight_miles_12mo)
boxplot(base$Flight_miles_12mo2)

windows()
par(mfrow = c(1,2))
hist(base$Flight_miles_12mo)
hist(base$Flight_miles_12mo2)


#Trabajando la variable Premio
hist(base$Premio)

#A?adimos nuevas variables encontradas
base$Premio2 = as.numeric(base$Premio2)
base$Flight_miles_12mo2 = as.numeric(base$Flight_miles_12mo2)

numericos = c(numericos,"Flight_miles_12mo2","Premio2")
numericos


#ANALISIS ESPECIALES
#====#=====#=========
#Cluster
########
library(cluster)
library(factoextra)

#N?mero optimo de clusters
fviz_nbclust(base[numericos],kmeans,method = "wss")
fviz_nbclust(base[numericos],kmeans,method = "silhouette")

cluster01 <- kmeans(base[numericos],2,nstart = 20)
table(cluster01$cluster,base$target)
pie(table(cluster01$cluster))

base$cluster01 = cluster01$cluster
prop.table(table(base$cluster01))


#Probando un segundo cluster
cluster02 <- kmeans(base[numericos],3,nstart = 20)
table(cluster02$cluster,base$target)

base$cluster02 = cluster02$cluster

prop.table(table(base$cluster02))
pie(table(cluster02$cluster))

#Agrupando el nuevo cluster


#Componentes principales
########################
comptotal = prcomp(base[numericos], center = TRUE,scale. = TRUE)
head(comptotal)
plot(comptotal)
plot(comptotal, type = "l")  

summary(comptotal$x)

PC = as.data.frame(comptotal$x)


#Particionando nuestra base de datos
#===================================

#Generamos nuestra base modeler
base_modeler = data.frame(base[dicotomicos],
                          base[numericos],
                          base$cluster01,
                          base$cluster03,
                          PC$PC1,
                          PC$PC2,
                          PC$PC3,
                          base[c("target")])


names(base_modeler)

set.seed(123)
muestra = sample(nrow(base_modeler),0.8*nrow(base_modeler))
base_train = base_modeler[muestra,]
base_test = base_modeler[-muestra,]

#Guardando nuestra base de datos





#Practicando ML
#==#==#==#==#==

#Arbol de clasificacion
#======================
library(rpart)
library(rpart.plot)
library(caret)
library(tidyverse)
library(e1071)

#Entrenando nuestro modelo
modelo01 = rpart(formula = target ~ ., data = base_train, method = "class")
rpart.plot(modelo01)

#Evaluando train
predic1_modelo01 = predict(modelo01, base_train, type = "c")
confusionMatrix(table(predic1_modelo01,base_train$target))

#Evaluando test



#Randon Forest
#======================
library(randomForest)
library(ModelMetrics)
library(ranger)

set.seed(120)
modelo02 = randomForest(factor(target)~., data = base_train)

#Evaluando train
predic1_modelo02 = predict(modelo02,base_train, type = "response")
tabla02 = table(predic1_modelo02,base_train$target)
aciertos = sum(diag(tabla02))/sum(tabla02)
aciertos

#Evaluando test



#Probando modelos
#----------------
modelo03 = randomForest(factor(target)~., data = base_train,
                     num.trees = 600,
                     num.threads = 6,
                     write.forest = T)


#Evaluando train
predic1_modelo03 = predict(modelo03,base_train, type = "response")
tabla02 = table(predic1_modelo03,base_train$target)
aciertos = sum(diag(tabla02))/sum(tabla02)
aciertos

#Evaluando test



#Modelo svm
#==#========
library(LiblineaR)
library(e1071)

#base_train
x = base_train[,-19]
y = base_train[,19]

modelo04 = LiblineaR(data = x,
                     target = y,
                     type = 1,
                     epsilon = 0.6,#Parametro de sintonizaci?n
                     kernel = "linear",
                     cost = 100)#Funcio de costo


#Evaluando train
predic1_modelo04 = predict(modelo04,base_train, type = 'class')$predictions
tabla02 = table(predic1_modelo04,base_train$target)
aciertos = sum(diag(tabla02))/sum(tabla02)
aciertos

#Evaluando test



#Regresion logistica
#====================
modelo05 = glm(target~., data = base_train, family = "binomial")
summary(modelo05)

#Generamos las predicciones
predic1_modelo05 = ifelse(test = modelo05$fitted.values > 0.5, yes = 1, no = 0)

tabla02 = table(predic1_modelo05,base_train$target)
aciertos = sum(diag(tabla02))/sum(tabla02)
aciertos







