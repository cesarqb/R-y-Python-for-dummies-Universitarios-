rm(list=ls())
############################################################################

# Librerias basicas para el estudio de series temporales

library(ggplot2)
library(TSA)
library(forecast)
library(scales)
library(stats)


#Leer la serie de tiempo, desde un archivo csv.
setwd("C:/..../Data")
pbi=read.csv("pbi.csv",header = T,sep="",dec=",")
View(pbi)

##Creacion de una serie de tiempo
pbits<-ts(pbi,start=c(1991,1),end=c(2009,12),frequency=12)
fechas = seq(as.Date("1991/1/1"), length.out = length(pbits), by = "months")
pbits

# Grafica de la serie de tiempo
plot(pbits,
     main="PBI (Enero 1991 - Diciembre 2009)",
     ylab="PBI",
     xlab="Años")

# Graficamos las cajas agregadas para observar si existe estacionalidad
windows(width=800,height=350) 
boxplot(split(pbits, cycle(pbits)), names = month.abb, col = "gold")


#############################################################
############ Enfoque de Descomposicion ######################
#############################################################

# Usamos el comando decompose para descomponer la serie de tiempo
Yt_desc = decompose(pbits,type = "multiplicative",filter = NULL)

# Grafico de descomposicion de la serie
plot(Yt_desc , xlab='Año')

# Serie original
pbits_original<-Yt_desc$x
View(pbits_original)

# Coeficientes estacionales
Coeficientes_Estacionales<-Yt_desc$seasonal
plot(Coeficientes_Estacionales)

# Tendencia de la serie
Yt_desc$trend

# Tipo de modelo aplicado
Yt_desc$type

# A la serie original,le quitamos la componente de estacionalidad, nos quedamos
# solo con la tendencia.
Tendencia_pbi<-pbits_original/Coeficientes_Estacionales
plot(Tendencia_pbi)

# Debido a que nos hemos quedado solo con la tendencia
Tendencia_pbi<-as.double(Tendencia_pbi)

#Viendo solo la componente tendencia, le ajuste la curva que mejor modele su
#comportamiento o que mejor la ajuste.

T = length(Tendencia_pbi)
yi = Tendencia_pbi[1:T]


# Ajustar 4 modelos: lineal, cuadratico, cubico
t = seq(1:T)
t2 = t**2
t3 = t**3

# Ajuste de Polinomiales a la Componente Tendencia
mod.lin = lm(yi~t)
mod.cuad = lm(yi~t+t2)
mod.cub = lm(yi~t+t2+t3)


summary(mod.lin)
summary(mod.cuad)
summary(mod.cub)

# Tenemos las estimaciones del modelo lineal, cuadratico y cubico
ajust_lineal  <- mod.lin$fitted.values
ajust_cuadrado<- mod.cuad$fitted.values
ajust_cubico  <- mod.cub$fitted.values

# Construimos la estimacion de la Zt serie de tiempo
estimacion_lineal    <- ajust_lineal*Coeficientes_Estacionales
estimacion_cuadratico<- ajust_cuadrado*Coeficientes_Estacionales
estimacion_cubico    <- ajust_cubico*Coeficientes_Estacionales

# Graficamos
plot(forecast(estimacion_cubico,h=12),col=2)
lines(pbits,col=3)
lines(estimacion_lineal, col=1)
lines(estimacion_cuadratico, col=7)

legend("topleft", lty=1, col=c(2,3,1,7),
       legend=c("Est.Cubica","Datos Ori.",
                "Est.Lineal","Est.Cuadratica"),
       bty="n")

# Validacion de Modelos
accuracy(estimacion_lineal,pbits)

# FIN!!
