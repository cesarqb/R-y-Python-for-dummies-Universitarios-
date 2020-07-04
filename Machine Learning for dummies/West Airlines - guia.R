#=======================================================
rm(list = ls())
gc()
#=======================================================
#Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#setwd('/Estudio de caso')
base = read.csv('ML Univ - Sesión 01/Estudio de caso/EastWestAirlinesNN.csv')
names(base)

names(base) = c("ID","Top","Premio","Cant_miles","cc1_miles","cc2_miles","cc3_miles",
                "Bonus_miles","Bonus_trans","Flight_miles_12mo","Flight_trans_12","Online_12",
                "Email","Club_member","Any_cc_miles_12mo","target")
str(base)

#Sepramos la base de datos seg?n el tipo de variable
numericos = c("Premio","Bonus_miles","Bonus_trans","Flight_miles_12mo","Online_12")
dicotomicos = c("Top","cc1_miles","cc2_miles","cc3_miles","Email","Club_member","Any_cc_miles_12mo")
target = c("target")

str(base[numericos])

#analizamos la dimencion de la base de datos y de la variable target
dim(base)

prop.table(table(base$target))

summary(base$target)
table(base$target)/sum(table(base$target))


#Tratamiento de variables dicot?micas
#========#========#========#========
names(base[dicotomicos])

summary(base[dicotomicos])
length(base$Top)

dim(base)

#Utilizando librerias
library(dplyr)
base <- arrange(base,Top)

#Eliminados los valores perdidos
base = base[!is.na(base$Top),]
length(base$Top)

View(base)

#Elimina todas las filas de nulos
#base = na.omit(base)

#Obtenemos frecuencias
library(foreign)

table(base$Top)

prop.table(table(base$Top))
prop.table(table(base$cc1_miles))
prop.table(table(base$cc2_miles))
prop.table(table(base$cc3_miles))
prop.table(table(base$Email))
prop.table(table(base$Club_member))
prop.table(table(base$Any_cc_miles_12mo))

#Automatizando los procesos
for (variable in dicotomicos){
  t = prop.table((table(base[variable])))
  cat(variable," = ", t,"\n")
}

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

table(base$Club_member,base$target)

table(base$target)

#Aderimos la variable "Online_dm" a nuestro vector "dicotomicos"
dicotomicos = c(dicotomicos,"Online_dm")

#######Prueba de igualdad de proporciones
#H0: la variable A es independiente de la variable B
#H1: la variable A es dependiente de la variable B
#######Si el p_value < 0.05 rechazamos la hipotesis nula

chisq.test((table(base$Top,base$target)))
chisq.test(table(base$cc1_miles,base$target))
chisq.test(table(base$Club_member,base$target))

#Corremos en masa
for (variable in dicotomicos){
  t = chisq.test(table(base[variable],base$target))
  cat(variable," = ", t$p.value,"\n")
  } 


#Var dicotomicas -> son beneficios que tiene la persona
base$beneficios = base$Top + base$cc1_miles + base$cc2_miles+
  base$cc3_miles + base$Email + base$Club_member + base$Any_cc_miles_12mo

table(base$beneficios) # hay personas que tienen varios beneficios
round(prop.table(table(base$beneficios)),2)

base$Premio2[base$Premio2 < q1] = 1
base$Premio2[base$Premio2 >= q1 & base$Premio2 < q2] = 2
base$Premio2[base$Premio2 >= q2 & base$Premio2 < q3] = 3
base$Premio2[base$Premio2 >= q3] = 4


#Eliminados las variables que no aportan
dicotomicos = dicotomicos[-c(3,4)]

#Generamos nuevas variables
base$sum_dic = base

View(base)

dicotomicos

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
hist(base$Bonus_trans, main = "Bonus_trans")
hist(base$Flight_miles_12mo, main = "Flight")
hist(base$Online_12,main = "Online_12")





windows()
par(mar = c(1,1,1,1))
par(mfrow = c(1,5))
boxplot(base$Premio, main = "Premio")
boxplot(base$Bonus_miles, main = "Bonus_miles")
boxplot(base$Bonus_trans, main = "Bonus_trans")
boxplot(base$Flight_miles_12mo, main = "Flight")
boxplot(base$Online_12,main = "Online_12")

#Transoformando la variable Online_12
round(prop.table(table(base$Online_12)),4)

base$compra_online = ifelse(base$Online_12 == 0,0,1)

table(base$Online_12,base$compra_online)
dicotomicos = c(dicotomicos,"compra_online")

numericos = numericos[-c(5)] # eliminamos la variable online_12 por no tener un comportamiento discreto

#Analisis de correlaci?n
#install.packages('PerformanceAnalytics')
library(PerformanceAnalytics)
??PerformanceAnalytics

cor(base[numericos])

windows()
chart.Correlation(base[numericos])


#Analizamos los valores perdidos
#=========#===========#==========
Q3 = quantile(base$Premio,0.75)
Q1 = quantile(base$Premio,0.25)

LI = Q1 - 1.5*(Q3-Q1)
LS = Q3 + 1.5*(Q3-Q1)

base$Premio2 = base$Premio
base$Premio2 = ifelse(base$Premio > LS,1,0)
prop.table(table(base$Premio2))

#Funci?n l?mites diagrama de caja
#atipicos = function(x){
#  Q3 = quantile(x,0.75)
#  Q1 = quantile(x,0.25)
#  
#  LI = Q1 - 1.5*(Q3-Q1)
#  LS = Q3 + 1.5*(Q3-Q1)
#  
#  valor = x
#  valor = ifelse(x > LS,1,0)
#  prop.table(table(valor))
#  }

#Mostramos el porcentaje de at?picos
  #atipicos(base$Premio)
  #atipicos(base$Bonus_miles)
  #atipicos(base$Bonus_trans)
  #atipicos(base$Flight_miles_12mo)
  #atipicos(base$Online_12)


#Imputamos los valores at?picos
Q3 = quantile(base$Premio,0.75)
Q1 = quantile(base$Premio,0.25)

LI = Q1 - 1.5*(Q3-Q1)
LS = Q3 + 1.5*(Q3-Q1)

base$Premio2 = base$Premio
base$Premio2 = ifelse(base$Premio2>= LS,NA,base$Premio)
summary(base$Premio2)

base$Premio2[is.na(base$Premio2)] = LS

windows()
par(mfrow = c(1,2))
boxplot(base$Premio)
boxplot(base$Premio2)

windows()
par(mfrow = c(1,2))
hist(base$Premio)
hist(base$Premio2)

summary(base[c("Premio","Premio2")])
sd(base$Premio)
sd(base$Premio2)

#Trabajando de otra forma la variable Premio
hist(base$Premio)

q1 = quantile(base$Premio,0.25)
q2 = quantile(base$Premio,0.50)
q3 = quantile(base$Premio,0.75)

base$Premio2 = base$Premio
#Generamos una escala ordinal
base$Premio2[base$Premio2 < q1] = 1
base$Premio2[base$Premio2 >= q1 & base$Premio2 < q2] = 2
base$Premio2[base$Premio2 >= q2 & base$Premio2 < q3] = 3
base$Premio2[base$Premio2 >= q3] = 4

mean(base$Premio)
median(base$Premio)

table(base$Premio,base$Premio2)


#A?adimos nuevas variables encontradas
base$Premio2 = as.numeric(base$Premio2)
base$Flight_miles_12mo2 = as.numeric(base$Flight_miles_12mo2)

#analizando la variable Flight_miles_12mo2
Q3 = quantile(base$Flight_miles_12mo,0.75)
Q1 = quantile(base$Flight_miles_12mo,0.25)

LI = Q1 - 1.5*(Q3-Q1)
LS = Q3 + 1.5*(Q3-Q1)

base$Flight_miles_12mo2 = base$Flight_miles_12mo
base$Flight_miles_12mo2 = ifelse(base$Flight_miles_12mo > LS,1,0)
prop.table(table(base$Flight_miles_12mo2))

??tidyverse

numericos = c(numericos,"Premio2")
numericos

#ANALISIS ESPECIALES
#====#=====#=========
#Cluster
########
library(cluster)
library(factoextra)

#Consideraciones
#---------------

# PUEDEN AFECTAR A LOS RESULTADOS
# Escala en que se miden las variables
# Magnitud de las varianzas

summary(base[numericos])

#escalamos los datos para un mejor ajuste del método
escalado = scale(base[numericos])
summary(escalado)

?scale

#N?mero optimo de clusters
fviz_nbclust(base[numericos],kmeans,method = "wss")
fviz_nbclust(base[numericos],kmeans,method = "silhouette")

#Nuevo método -> scaler
#N?mero optimo de clusters
fviz_nbclust(escalado,kmeans,method = "wss")
fviz_nbclust(escalado,kmeans,method = "silhouette")


cluster01 <- kmeans(escalado,2,nstart = 20)
table(cluster01$cluster,base$target)
chisq.test(table(cluster01$cluster,base$target))

pie(table(cluster01$cluster))

base$cluster01 = cluster01$cluster
prop.table(table(base$cluster01))


#Probando un segundo cluster
cluster02 <- kmeans(escalado,3,nstart = 20)
table(cluster02$cluster,base$target)
chisq.test(table(cluster02$cluster,base$target))

base$cluster02 = cluster02$cluster

pie(table(cluster02$cluster))

#Agrupando el nuevo cluster
base$cluster03 = base$cluster02
base$cluster03 = ifelse(base$cluster03 == 1,2,base$cluster03)

prop.table(table(base$cluster03))
pie(table(base$cluster03))

#Probando ultimo cluster
cluster_f <- kmeans(base[numericos],4,nstart = 20)
prop.table(table(cluster_f$cluster))


#Componentes principales
########################
escalado = scale(base[numericos],center = TRUE, scale = TRUE)
summary(escalado)

?scale

comptotal = prcomp(escalado, center = TRUE,scale. = TRUE)
head(comptotal)
plot(comptotal)
plot(comptotal, type = "l")  

biplot(x = comptotal, scale = 0, cex = 0.6, col = c("blue4", "brown3"))
summary(comptotal$x)

#Varianza explicada
comptotal$sdev^2
prop_var = comptotal$sdev^2 / sum(comptotal$sdev^2)
prop_var

PC = as.data.frame(comptotal$x)
View(PC)


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

View(base_modeler)
names(base_modeler)

set.seed(123)
muestra = sample(nrow(base_modeler),0.8*nrow(base_modeler))
base_train = base_modeler[muestra,]
base_test = base_modeler[-muestra,]

#Guardando nuestra base de datos
save(base_modeler, file="base_modeler.rda") #guardamos en un formato del R.
write.csv(base_modeler, file="base_modeler.csv") #guardamos en un archivo CSV.


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
predic2_modelo01 = predict(modelo01, base_test, type = "c")
confusionMatrix(table(predic2_modelo01,base_test$target))

varImp(modelo01)


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
predic2_modelo02 = predict(modelo02,base_test, type = "response")
tabla02 = table(predic2_modelo02,base_test$target)
aciertos = sum(diag(tabla02))/sum(tabla02)
aciertos

#Graficando la importancia de las variables
par(mfrow = c(1,1))
varImp(modelo02)

windows()
varImpPlot(modelo02)


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
predic2_modelo03 = predict(modelo03,base_test, type = "response")
tabla02 = table(predic2_modelo03,base_test$target)
aciertos = sum(diag(tabla02))/sum(tabla02)
aciertos

windows()
varImp(modelo03)
varImpPlot(modelo03)


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

predic2_modelo04 = predict(modelo04,base_test, type = 'class')$predictions
tabla02 = table(predic2_modelo04,base_test$target)
aciertos = sum(diag(tabla02))/sum(tabla02)
aciertos


#Regresion logistica
#====================
modelo05 = glm(target~., data = base_train, family = "binomial")
summary(modelo05)

#Generamos las predicciones

#Evaluando train
predic1_modelo05 = ifelse(test = modelo05$fitted.values > 0.5, yes = 1, no = 0)

tabla02 = table(predic1_modelo05,base_train$target)
aciertos = sum(diag(tabla02))/sum(tabla02)
aciertos

#Evaluando test
predic2_modelo05 = predict(modelo05,newdata = base_test,type = 'response')
predic2_modelo05 = ifelse(predic2_modelo05 > 0.5, yes = 1, no = 0)

tabla02 = table(predic2_modelo05,base_test$target)
aciertos = sum(diag(tabla02))/sum(tabla02)
aciertos

