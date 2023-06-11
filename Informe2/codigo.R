# ajuste de distribuciones y probabilidades

library(dplyr)   # manipulacion de datos
library(ggplot2) # visualizacion
library(readxl) # leer excel y escribir excel
library(moments) # asimetria y curtosis
library(tidyverse)
library(nortest)
library(gamlss)
library(gamlss.dist)
library(fitdistrplus)

paleta <- c("#FF5A5F", "#FFB400", "#007A87", 
            "#8CE071", "#7B0051", "#00D1C1", "#FFAA91", "#B4A76C", 
            "#9CA299", "#565A5C", "#00A04B", "#E54C20")



# caso continuo ----
#install.packages("ggvis")
# consideremos los siguientes datos de la libreria siguiente
#install.packages("ggvis")
library(ggvis)
cocaina = ggvis::cocaine %>% as_tibble()
cocaina

# cocaina / potencia ---- HISTOGRAMA
potencia = cocaina$potency
hist(potencia,col="grey100",breaks = 15,freq = F,ylim=c(0,0.03)) #NO ES NORMAL
lines(density(potencia), col = "grey40", lty="dotted",lwd=3)
boxplot(potencia,horizontal = T)
summary(potencia) #porcentaje

# transformaremos la variable a porcentaje % dado 
# que la potencia se mide entre 0 y 100

coca_pot = potencia/100 #distribucion beta de 0 a 1

# se mantiene el grafico pues la transformacion no cambia los datos, 
# solo los pasa a decimales en [0,1]

hist(coca_pot,col="grey100",breaks = 15,freq = F,ylim=c(0,3))
lines(density(coca_pot), col = "grey40", lty="dotted",lwd=3)

# probamos el ajuste con la libreria gamlss() y solo con distribuciones 
# donde x esta entre 0 y 1

m1 = fitDist(coca_pot,
             k=2,
             type = "real0to1",
             trace=FALSE,
             try.gamlss = T)
m2 = fitDist(potencia,
             k=2,
             type = "realplus",
             trace=FALSE,
             try.gamlss = T)
m2$fits
m1$fits # revisamos los AIC entregados para las distribuciones ajustadas

mu = m1$mu
s = m1$sigma

a = mu*(1-s^2)/s^2
b = (1-mu)*(1-s^2)/s^2

a/(a+b)



# podemos ver que la que mejor ajusta en este caso es la distribucion Beta, 
# por lo que usaremos esa a ver si es buen ajuste

fitbeta <- fitdist(coca_pot,"beta")
fitgamma <- fitdist(coca_pot,"gamma")



cdfcomp(list(fitbeta,fitgamma))
denscomp(list(fitbeta,fitgamma))
qqcomp(list(fitbeta,fitgamma))
ppcomp(list(fitbeta,fitgamma))
cbind(gofstat(fitbeta),gofstat(fitgamma))

gofstat(list(fitbeta,fitnn))


ks.test(potencia,"pbeta",shape1 = fitbeta$estimate[1], shape2 = fitbeta$estimate[2])
ks.test(potencia,"pgamma",shape = fitgamma$estimate[1], scale = fitgamma$estimate[2])

hist(coca_pot,col="grey100",breaks = 15,freq = F,ylim=c(0,3))
lines(density(coca_pot), col = "grey40", lty="dotted",lwd=3)

curve(dbeta(x,shape1 = fitbeta$estimate[1],shape2 = fitbeta$estimate[2]), add=TRUE,col="darkorange", lwd=2)
curve(dgamma(x,shape = fitgamma$estimate[1],rate =fitgamma$estimate[2] ), add=TRUE,col="steelblue", lwd=2)

fitbeta$aic
fitgamma$aic
# extra
shapiro.test(coca_pot) #mut baja el pvalue no se puede
fitnormal <- fitdist(coca_pot,"norm")
curve(dnorm(x,mean=fitnormal$estimate[1],sd=fitnormal$estimate[2]), add=TRUE,col="yellowgreen", lwd=2)

#TIEMPO EXPONENCIAL

# preguntas: ----

#FITBETA
#X: potencia de la dosis de cocaina
#x dist beta(4.16,2.56)

#' ¿cuál es el promedio de la variable X?

alpha = fitbeta$estimate[1]
beta = fitbeta$estimate[2]

alpha/(alpha+beta)
#' ¿Què probabilidad hay de obtener una dosis de cocaina con una potencia muy baja (<40)?

pbeta(0.40,alpha,beta)

#' ¿desde que valor puedo considerar que tiene una potencia de buena calidad
#' (asumiendo calidad en el 15% mas alto.)
#' p(X<Q) = 0.85
qbeta(0.85,alpha,beta)
# ´P(0.15>Q) = 0.15
qbeta(0.15,alpha,beta,lower.tail = F)


#' Con que posibilidad recibire una dosis con potencia entre 20 y 45%.
#' P(20<X<45) = F(45)-F(20)
pbeta(0.45,alpha,beta)-pbeta(0.20,alpha,beta)
#' 
#' 


# caso discreto
### calculo de cocaiona de buena calidad >85%



# PROPOSITO DE INVESTIGACION

#Evaluat que tan posible es encotrar dosis de cocaina con potencia superior al 85%



# REVISION DE LOS DATOS (CALIDAD DE LOS DATOS)



# DEFINICION EXPERIMENTO

#creamos la vvariable calidad  y de ella tamremos muestras  de tamaño n = 40 para ver si es posible obtener mayor calidad
# binomial(40,p)

#X: cantidad de dosis en las 40 muestras con buena calidad 
#Exito: calidad >= 0.85
#FRacASO : calidad <0.85
cocaina = cocaina %>%
  mutate(calidad = ifelse(potency>85,1,0))
cocaina
length(coca_pot)
probs = NULL
n1 = round(length(cocaina$calidad)*0.2,0)
for (i in 1:100) {
  sample.aux = cocaina$calidad %>% sample(n1,replace=F)
  probs = c(probs,mean(sample.aux))
  
}
p = mean(probs)
p
mean(cocaina$calidad)
# DEFINICION DE VARIABLE


# CALCULAMOS LAS PROBABILIDADES NECESARIAS

#con que probailidad voy a obtern entre 15 y 21 muestra de calidad alta


pbinom(20,size = 40,prob = p) -pbinom(15,size = 40,prob = p)

#install.packages("mosaic")
library(mosaic)
n =40
plotDist(dist="binom",size = n,prob =p)

plotDist(dist="pois",6)


plotDist(dist="binom",size = n,prob =p,kind ="histogram")


# respuesta.

# otro


##----


# cocaina / precio ----

precio = cocaina$price
boxplot(cocaina$price,horizontal = T)

hist(precio,col="grey100",breaks = 120,freq = F,xlim = c(0,4000),ylim = c(0,.002))
lines(density(precio), col = "grey40", lty="dotted",lwd=3)