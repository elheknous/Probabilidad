library(tidyverse)
library(GGally)
library(corrplot)
library(dplyr)
library(ggplot2)
library(car)
library(corrplot)
# es necesario ingresar la matriz
# de correlación!

m0 = modelo <- lm(mpg ~ drat + wt  +qsec + vs + am, data = mtcars)
summary(m0)

vif(m0)

corrplot(cor(mtcars[,5:9]),
         method="color",
         addCoef.col = "black",
         number.cex=1,
         tl.col="black",
         tl.srt=45) # rotación nombres

modelo <- lm(mpg ~ disp + hp  +drat + wt, data = mtcars)
summary(modelo)


# calculamos el valor VIF a cada predictor
# del modelo anterior

vif(modelo)

corrplot(cor(mtcars[,c("disp","hp","wt","drat")]),
         method="color",
         addCoef.col = "black", number.cex=1.5,
         tl.col="black",tl.srt=45)
modelo1 <- lm(mpg ~ disp + hp + wt + drat, data = mtcars )
summary(modelo1)
