library(tidyverse)
library(GGally)
library(corrplot)
library(dplyr)
library(ggplot2)
library(car)
library(readxl)
library(nortest)
library(ggfortify)
library(lmtest)
library(gamlss)
library(ppcor)
library(MASS)

datosTotales <- read_excel("C:/Users/josem/Downloads/MATUS_TORO_JOSE (1).xlsx")

aire <- datosTotales %>% dplyr :: select(-year)

####################################################################
############################ PASO 1 ################################
####################################################################

#Analizar la correlacion de los datos, se quuta la variable año ya que como los
# datos son todos del 2013, esta variable no ayuda en el calculo

corrplot(cor(aire),
         method="color",      
         addCoef.col = "black",  
         number.cex=0.9,  
         tl.col="black",  
         tl.srt=45)

# Seleccionamos la variable PRES

####################################################################
############################ PASO 2 ################################
####################################################################

ggpairs(data = aire, #Graficos con recta que se adapta
        lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"),
        axisLabels = "none")

# Para el modelo de regresion lineal de una variable escogeremos la variable TEMP
# ya que este es el que tiene mayor relacion con PRES  como se puede ver en ambos
# graficos anteriosres


modelo1 <- lm(PRES ~ TEMP, data = aire)
summary(modelo1) #0.5303

aire %>% 
  ggplot(aes(TEMP,PRES))+
  geom_point()+
  geom_abline(intercept = modelo1$coefficients[1],
              slope = modelo1$coefficients[2],
              col="purple",linewidth=2)


summary(modelo1) #0.5304
outlierTest(modelo1)

puntosAtipicos <- influencePlot(modelo1)

inffluyentesAtipcos = as.numeric(rownames(puntosAtipicos))
modeloSinPunto = lm(PRES ~ TEMP , data = aire %>% 
              dplyr :: slice(-inffluyentesAtipcos))

summary(modeloSinPunto) #0.5375


# Pa que  sirve esto?
npara = 2
n-nrow(aire)
corte = 4/(n-npara-2)
plot(modelo1,which = 4,cook.levels = corte)

#SUPUESTOS

#NORMALIDAD 

autoplot(modelo1)[2]
plot(density(modelo1$residuals))
nortest::lillie.test(modelo1$residuals) #0.009622

autoplot(modeloSinPunto)[2]
plot(density(modeloSinPunto$residuals))
nortest::lillie.test(modeloSinPunto$residuals) #0.00856


#EL modelo es muy cercano a 0; no es preciso

#INDEPENDENCIA

plot(modelo1,which = 1)
autoplot(modelo1)[1]

lmtest::dwtest(modelo1)

# muy cercano a cero no sirve

#HOMOCEDASTECIDAD

autoplot(modelo1)[1]
lmtest::bptest(modelo1)

# muy cercano a cero

#NO CUMPLE NINGUNO DE LOS 3 SUPUESTOS














