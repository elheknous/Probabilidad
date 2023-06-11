library(tidyverse)
library(GGally)
library(corrplot)
library(dplyr)
library(ggplot2)
library(car)
library(readxl)

# airquality ----

aire = read_excel("C:/Users/jose/Downloads/airquality.xlsx")

aire

cor(aire)[13,]
plot(aire$AQI,aire$PM2.5)

m1 <- lm(AQI ~ . , data=aire)
summary(m1)

names(aire)

m2 <- lm(AQI ~ . , data=aire[,-c(8)]) # -c(8) quita columnas
summary(m2)

m3 <- lm(AQI ~ . , data=aire[,-c(8,6)]) # -c(8) quita columnas
summary(m3)

m4 <- lm(AQI ~ . , data=aire[,-c(8,6,3)]) # -c(8) quita columnas
summary(m4)

# caso extra
m5 <- lm(AQI ~ . , data=aire[,-c(8,6,3,10)]) # -c(8) quita columnas
summary(m5)

modelo = lm(AQI ~ .,data=aire ) # EQUIVALE A M1

## Definir Modelo
library(MASS)

stepAIC(modelo,
        direction = "backward")

mb = stepAIC(lm(AQI ~ .,data=aire ),
             direction = "backward")
summary(mb)

mf = stepAIC(lm(AQI ~ 1 , data=aire),
             scope = ~ PM2.5 + PM10 + NO + NO2 + NOx + NH3 + CO + SO2 + O3 + Benzene + 
               Toluene + Xylene,
             direction="forward")
summary(mf)



