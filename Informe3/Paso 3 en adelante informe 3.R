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
############################ PASO 3 ################################
####################################################################

modeloFull <- lm(PRES ~ .,data = aire) # 0.6166
summary(modeloFull)
vif(modeloFull)

ma = lm(PRES ~., data = aire %>% dplyr :: select(-PM2.5)) # 0.6169 
summary(ma)
vif(ma)

mb = lm(PRES ~., data = aire %>% dplyr :: select(-RAIN)) # 0.6145
summary(mb)
vif(mb)

mc = lm(PRES ~., data = aire %>% dplyr :: select(-hour)) # 0.6135
summary(mc)
vif(mc)

me = lm(PRES ~., data = aire %>% dplyr :: select(-PM2.5,-hour,-RAIN)) # 0.612
summary(me)
vif(me)

#Entre el modelo ma y me hay una diferencia del 0.49%, sin embargo, el 
#modelo me consta con tres variable menos mientras que el el modelo ma
#tiene una variable menos, por lo tanto, considerando la poca diferencia 
#de R^2 escogeremos el modelo me considerando que tambien que le vif de ammbos
#es lo sufientemente bajo para decir que ningulo de los modelos tiene
#multicolinealidad

modelo2 = me

####################################################################
############################ PASO 4 ################################
####################################################################

modelo3 <- stepAIC(lm(PRES ~ ., data = aire),
                 direction = "backward")
summary(modelo3)


####################################################################
############################ PASO 4 ################################
####################################################################

#nos quedamos con el modelo 3

#ERROREES MODELO 2

outlierTest(modelo2)
influencePlot(modelo2)
puntosAtipicos <- influencePlot(modelo2)


inffluyentesAtipcos = as.numeric(rownames(a))
modeloSinPunto = lm(PRES ~ . , data = aire %>% dplyr :: select(-PM2.5,-hour,-RAIN,hour)%>%
                      dplyr :: slice(-inffluyentesAtipcos))
summary(modeloSinPunto)


# Ya qie 
















