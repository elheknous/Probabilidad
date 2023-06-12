library(tidyverse)
library(GGally)
library(corrplot)
library(dplyr)
library(ggplot2)
library(car)
library(readxl)



rent99 <- read_excel("C:/Users/jose/Downloads/rent99.xlsx")
head(rent99)

#Para ajustar la regresión lineal no consideraremos las variables rentsqm y district.
#Además, ajustaremos 3 modelos distintos:
# ∙ Modelo 1: Renta dado el área de la propiedad.
# ∙ Modelo 2: Renta dado el área de la propiedad y el año de construcción.
# ∙ Modelo 3: Renta dadas todas las variables restantes de la base

renta = rent99[,-c(2,9)]

m1 = lm(rent ~ area, data = renta)
m2 = lm(rent ~ area + yearc, data = renta)
m3 = lm(rent ~ ., data = renta)

summary(m1)
summary(m2)
summary(m3)

