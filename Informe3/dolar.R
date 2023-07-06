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



dolar <- read_excel("C:/Users/jose/Downloads/dolar.xlsx")
view(dolar)

cor(dolar)
ggpairs(dolar)

corrplot(cor(dolar),
         method="color",      #CUDRO DE COLORES
         addCoef.col = "black",   # agrega coefcientes 
         number.cex=1,  # tamaño coef
         tl.col="black",  # color nombres de col y rows
         tl.srt=45) # angulo de rotación names col


#' dadas las correlaciones, podemos ver que la variable
#' ocuppation podria descartarse del modelo.
#' age y experience tiene una relacion lineal positiva muy fuerte
#' además, 
#' experiencia y educacion 
#' ocuppation y sector
#' podrian generar algun problema de multicolinealidad

#SEGUNDO PASO

modeloFull = lm(lwage ~ ., data = dolar)
summary(modeloFull)


# 3 MULTICOLINEALIDAD

vif(modeloFull)

# entre 1 e infinito
#' encotramos valores vif altos en las variables
#' education, experiencey age
#' veremos la correlacion parcial
#' 


pcor(dolar) #p value menor al 5% cercano a 0

m1 = lm(lwage ~ ., 
        data = dolar %>% dplyr :: select(-education))

m2 = lm(lwage ~ .,
        data = dolar %>% dplyr :: select(-experience))

m3 = lm(lwage ~ .,
        data = dolar %>% dplyr :: select(-age))

m4 = lm(lwage ~ .,
        data = dolar %>% dplyr :: select(-education, -experience, -age))

m5 = lm(lwage ~ .,
        data = dolar %>% dplyr :: select(-experience, -age))

summary(m1) 
vif(m1) # no sirve

summary(m2) 
vif(m2)

summary(m3) 
vif(m3)

summary(m4)
vif(m4)

summary(m5)
vif(m5)

# m3 modelo seleccionado por tener un R^2 mayor


m3 = lm(lwage ~ .,
        data = dolar %>% dplyr :: select(-age))
summary(m3)

m3a = lm(lwage ~ .,
         data = dolar %>% dplyr :: select(-age, -occupation))
summary(m3a)

m3b = lm(lwage ~ .,
         data = dolar %>% dplyr :: select(-age, -occupation,-race))
summary(m3b)


m3c = lm(lwage ~ .,
         data = dolar %>% dplyr :: select(-age, -occupation,-race, -Married))
summary(m3c)

# se queda con 3c porque son 2 variables menos y el r casi no cambia


mback <- stepAIC(lm(lwage ~ ., data = dolar),
                  direction = "backward")
summary(mback)



mback2 <- stepAIC((m3),
                 direction = "backward")

summary(mback2)

#NOS QUEDAMOS M3C AHORA SI

summary(m3c) # MODELO ELEGIDO

outlierTest(m3c)
a = influencePlot(m3c)

inffluyentesAtipcos = as.numeric(rownames(a))
modelo = lm(lwage ~ ., data = dolar %>% dplyr :: select(-age, -occupation,-race, -Married) %>% 
           dplyr :: slice(-inffluyentesAtipcos))

summary(modelo)


#NORMALIDAD
autoplot(modelo)[2]
plot(density(modelo$residuals))
nortest::lillie.test(modelo$residuals)

# si cumple

#indepencdia

plot(modelo,which = 1)
autoplot(modelo)[1]

dwtest(modelo)
# Aceptamos que no hay autocorrelacion entre los residuos

# homocedasticidad
autoplot(modelo)[1]
lmtest::bptest(modelo)


# si consideramos el primer modelo al 5% si cumple los supuesto
# pero considera 2 variables mas.
lillie.test(m3a$residuals)
lmtest::dwtest(m3a)
lmtest::bptest(m3a)

autoplot(m3a)[1]

# considerando influyentes

lillie.test(m3c$residuals)
lmtest::dwtest(m3c)
lmtest::bptest(m3c)

























