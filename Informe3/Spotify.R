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

spotify <- read_excel("C:/Users/josem/Downloads/Spotify.xlsx")
ggpairs(spotify)

spotify = na.omit(spotify)
corrplot(cor(baseSpotify),
         method="color",      #CUDRO DE COLORES
         addCoef.col = "black",   # agrega coefcientes 
         number.cex=1,  # tamaño coef
         tl.col="black",  # color nombres de col y rows
         tl.srt=45) # angulo de rotación names col


m1 = lm(Popularity ~.,data = baseSpotify)
summary(m1)
vif(m1)

m2 = lm(Popularity ~.,data = baseSpotify %>% dplyr :: select(-Acousticness))
summary(m2)
vif(m2)

m3 = lm(Popularity ~.,data = baseSpotify %>% dplyr :: select(-Acousticness,-Energy))
summary(m3)
vif(m3)

m4 = lm(Popularity ~.,data = baseSpotify %>% dplyr :: select(-Acousticness,-Energy,-Positive_mood))
summary(m4)
vif(m4)

modf = stepAIC(lm(Weight ~ 1,data = peces),
               scope = formula(modeloCompleto),
               direction = "forward")
summary(modf)
vif(modf)


