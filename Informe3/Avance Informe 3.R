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

aireTotal <- read_excel("C:/Users/josem/Downloads/MATUS_TORO_JOSE (1).xlsx")

# Paso 1
aire <- aireTotal %>% dplyr :: select(-year)

corrplot(cor(aire),
         method="color",      #CUDRO DE COLORES
         addCoef.col = "black",   # agrega coefcientes 
         number.cex=1,  # tamaño coef
         tl.col="black",  # color nombres de col y rows
         tl.srt=45) # angulo de rotación names col

# Paso 2

modeloFull = lm(TEMP ~ ., data = aire) #0.5906
summary(modeloFull)

m1 = lm(TEMP ~ ., data = aire  %>% dplyr :: select(-RAIN)) # sacando la variable con un pvlue menor
                                                           # RAIN el r baja muy poco, si sirve
summary(m1)

# Paso 3 Multicolinealidad

vif(modeloFull)

# viendo la multicolinealidad se ve que no existen variables 
# relacionadas entre ellas

mback <- stepAIC(lm(TEMP ~ ., data = aire),
                 direction = "backward")
summary(mback)

mback2 <- stepAIC((modeloFull),
                  direction = "backward")

summary(mback2)

# CON ESTO SE MANTIENE EL VALOR ORIGINAL DE 0.5906
# POR LO TANTO SE MANTIENE EL MODELO m1

summary(m1)

# Paso 4 Errores

# Normalidad

autoplot(m1)[2]
plot(density(m1$residuals))
nortest::lillie.test(m1$residuals)

#SI CUMPLE TEST DE NORMALIDAD















