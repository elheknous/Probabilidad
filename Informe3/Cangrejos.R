# IRIS ----

# Analisis de Regresion ---------------------------------------------------

# install.packages("GGally")
# install.packages("corrplot")
# install.packages("car")
# rm(list = ls())
library(tidyverse)
library(GGally)
library(corrplot)
library(dplyr)
library(ggplot2)
library(car)


# cangrejos ----

# C: the female crab's color, 
# S: spine condition, 
# W: weight in kilograms, 
# Wt: carapace width in centimeters, (ancho del cangrejo)

# Sa: response outcome for each female crab is her number of satellites.

cangrejos <- read.table(file='https://raw.githubusercontent.com/fhernanb/datos/master/crab', header=T)
cangrejos

ggpairs(data = cangrejos,
        lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"),
        axisLabels = "none")


          #y = x
mod4 <- lm(W ~ Wt , data=cangrejos)

summary(mod4)

# modelo ejemplo MALO
mod5 <- lm(Sa ~ S , data=cangrejos)

summary(mod5)

# modelo 4 pero con intercepto 0
mod6 <- lm(W ~ 0 + Wt , data=cangrejos)

summary(mod6)

cangrejos %>% 
  ggplot(aes(Wt,W))+
  geom_point()+
  scale_y_continuous(limits = c(-3,50))+
  scale_x_continuous(limits = c(-1,6))+
  geom_abline(intercept = mod4$coefficients[1],
              slope = mod4$coefficients[2],
              col="tomato",linewidth=2)

  
cangrejos %>% 
  ggplot(aes(Wt,W))+
  geom_point()+
  scale_y_continuous(limits = c(-3,50))+
  scale_x_continuous(limits = c(-1,6))+  
  geom_abline(intercept = 0,
              slope = mod6$coefficients[1],
              col="steelblue",linewidth=2)

cangrejos

# No se usa el modelo 6 pese a que el r2 sea mayor debido a que la grafica no se adapta a bien y 
# en el contexto de los datos no tiene sentido que un cangrejo tenga peso 0


# prediccion del peso de un cangrejo con Wt=4

nuevo = data.frame(Wt=5)

predict(mod4,newdata = nuevo)



               