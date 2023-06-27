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

# modelo 1 (una variable)

modelo1 <- lm(PRES ~ TEMP, data = aire)
summary(modelo1) #0.5303

# modelo 2 (varias variables)

modelo2 = lm(PRES ~., data = aire %>% dplyr :: select(-PM2.5,-hour,-RAIN)) # 0.612
summary(me)

# modelo 3 (modelo backward)

modelo3 <- stepAIC(lm(PRES ~ ., data = aire), #0.6169
                   direction = "backward")
summary(modelo3)

# Para empezar revisaremos los supuestos

## SUPUESTOS MODELO 1 ##

autoplot(modelo1)[2]
plot(density(modelo1$residuals))

nortest::lillie.test(modelo1$residuals)

# INDDEPENDENCIA

plot(modelo1,which = 1)
autoplot(modelo1)[1]
lmtest::dwtest(modelo1)


#Homocedasticidad

autoplot(modelo1)[1]
lmtest::bptest(modelo1)

#NO CUMPLE NINUGN SUPUESTO

## SUPUESTOS MODELO 2 ##

autoplot(modelo2)[2]
plot(density(modelo2$residuals))

nortest::lillie.test(modelo2$residuals)

# INDDEPENDENCIA

plot(modelo2,which = 1)
autoplot(modelo2)[1]
lmtest::dwtest(modelo2)


#Homocedasticidad

autoplot(modelo2)[1]
lmtest::bptest(modelo2)

#NO CUMPLE NINUGN SUPUESTO
## SUPUESTOS MODELO 3 ##

autoplot(modelo3)[2]
plot(density(modelo3$residuals))

nortest::lillie.test(modelo3$residuals)

# INDDEPENDENCIA

plot(modelo3,which = 1)
autoplot(modelo3)[1]
lmtest::dwtest(modelo3)


#Homocedasticidad

autoplot(modelo3)[1]
lmtest::bptest(modelo3)

#NO CUMPLE NINUGN SUPUESTO

me = lm(PRES ~., data = aire %>% dplyr :: select(-PM2.5,-RAIN)) # 0.612
summary(me)

puntosAtipicos <- influencePlot(me)
inffluyentesAtipcos = as.numeric(rownames(a))
modeloSinPunto2 = lm(PRES ~ . , data = aire %>% dplyr :: select(-PM2.5,-RAIN)%>%
                       dplyr :: slice(-inffluyentesAtipcos))
summary(modeloSinPunto2)

#normalidad
autoplot(modeloSinPunto2)[2]
plot(density(modeloSinPunto2$residuals))

nortest::lillie.test(modeloSinPunto2$residuals) #0.06616

# INDDEPENDENCIA

plot(modeloSinPunto2,which = 1)
autoplot(modeloSinPunto2)[1]
lmtest::dwtest(modeloSinPunto2)


#Homocedasticidad

autoplot(modeloSinPunto2)[1]
lmtest::bptest(modeloSinPunto2)













