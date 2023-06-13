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

siesta = read.csv(file.choose())
siesta

m1 = lm(sleep ~ wine, data = siesta)
summary(m1)

cor(siesta) #Buena corelacion

#primer supuesto: Normalidad

par(mfrow=c(2,2))
plot(m1)

autoplot(m1)

lillie.test(m1$residuals) 
# Cumple supuesto por ser mayor al 5%,
# No se puede rechazar la hipotesis nula
# los residuos si se distribuyen de forma normal

#Segunso supuest: Independencia de los residuos

dwtest(m1)

#tambien concuimos que los residuos son independientes
# dado que tenemos un p value de 0.4 > 0.05

#homocesdasticidad
# graficamos los valores ajustados vs residuos
autoplot(m1)[1]
bptest(m1)

#Se cumple la wea
#tienen varianza constante
#se cumplen los 3 supuestos



ggplot(siesta,aes(wine,sleep))+
  geom_point()+
  geom_smooth()

#puntos influyentes

outlierTest(m1)
a = influencePlot(m1)
a
head(siesta)

npara = 2
n-nrow(siesta)
corte = 4/(n-npara-2)
plot(m1,which = 4,cook.levels = corte)

aux = as.numeric(rownames(a))
m1 = lm(sleep ~ wine, data = siesta[-aux,])
summary(m1)

# elegimos el modelo 

#predecimos cuantos minutos

nuevo = data.frame(wine = 453)
predict(m1,newdata = nuevo
        , interval = c("confidence"),
        level = 0.95)







