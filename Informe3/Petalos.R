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


## Analisis Descriptivo

str(iris)
summary(iris)

### Suponga que interesa estudiar que 
# variables se relacionan con el largo del petalo.


## Dispersion

plantas <- iris %>% dplyr::select(-Species) #Base de datos sin la columna especie
pairs(plantas)

ggpairs(plantas) # Graficos

ggpairs(data = plantas, #Graficos con recta que se adapta
        lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"),
        axisLabels = "none")




corrplot(cor(iris %>% dplyr::select(-Species)),method="color",      #CUDRO DE COLORES
         addCoef.col = "black",   # agrega coefcientes 
         number.cex=1,  # tamaño coef
         tl.col="black",  # color nombres de col y rows
         tl.srt=45) # angulo de rotación names col


# Ajuste del Modelo -------------------------------------------------------
# Se elige la variable independiente x que mejor ajuste la variable dependiente y: Petal.Length


iris2 <- iris %>% 
  dplyr::select(-Species) %>% 
  as_tibble()
iris2
mod1 <- lm(Petal.Length ~ Sepal.Width,data=iris2)
mod2 <- lm(Petal.Length ~ Sepal.Length,data=iris2)
mod3 <- lm(Petal.Length ~ Petal.Width,data=iris2)

summary(mod1)
summary(mod2)
summary(mod3)

# Segun el R-squared la mejor que ajusta es x: Sepal.Length

iris2 %>% 
  ggplot(aes(Petal.Width,Petal.Length))+
  geom_point()+
  geom_abline(intercept = mod3$coefficients[1],
              slope = mod3$coefficients[2],
              col="tomato",linewidth=2)


# como p-value del modelo es < 0.05, entonces
# existen betas distintos de 0.

# parametros (beta_i)
#  H0: beta_i = 0 vs H1: Beta_i !=0

# beta_0: pvalue <0.05 si es significativo para el modelo
# beta_1: pvalue <0.05 si es significativo para el modelo

#  el modelo queda como
# largo_petalo = 1.08 + 2.22 * Ancho_petalo (Petal.Width)

# donde la variabilidad explicada tieneun R^2 de un 93% aprox