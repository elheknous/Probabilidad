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

#fish = read.csv(file.choose())
fish


cor(fish[,-1])
ggpairs(fish)

corrplot(cor(fish[,-1]),
         method="color",      #CUDRO DE COLORES
         addCoef.col = "black",   # agrega coefcientes 
         number.cex=1,  # tamaño coef
         tl.col="black",  # color nombres de col y rows
         tl.srt=45) # angulo de rotación names col

peces = fish %>% dplyr :: select(-Species)
modeloCompleto = lm(Weight ~ .,data = peces)
summary(modeloCompleto)

#multicolinealidad

vif(modeloCompleto)

pcor(peces %>% dplyr :: select(-Weight))

mod1 = lm(Weight ~ .,data = peces %>% dplyr :: select(-Length1))
summary(mod1)
vif(mod1)

mod2 = lm(Weight ~ .,data = peces %>% dplyr :: select(-Length2))
summary(mod2)
vif(mod2)

mod3 = lm(Weight ~ .,data = peces %>% dplyr :: select(-Length3)) #vif muy alto 
summary(mod3)
vif(mod3)

#SE DESCARTA MODELO 3

mod4 = lm(Weight ~ .,data = peces %>% dplyr :: select(-Length1,-Length2,-Height))
summary(mod4)
vif(mod4)

mod4a = lm(Weight ~ .,data = peces %>% dplyr :: select(-Length1,-Length2,-Width))
summary(mod4a)
vif(mod4a)
# mod4a se elimino casi toda la multi, tonce se queda el mod4a

modf = stepAIC(lm(Weight ~ 1,data = peces),
               scope = formula(modeloCompleto),
               direction = "forward")
summary(modf)
vif(modf)

summary(mod4a)
vif(mod4a)

modb = stepAIC(lm(Weight ~ .,data = peces),
               direction = "backward",
               trace = F)

summary(modb)
vif(modb)


AIC(mod4a)

AIC(modf)

outlierTest(mod4a)
influencePlot(mod4a)

mod4a = lm(Weight ~ .,data = peces %>%
             dplyr :: select(-Length1,-Length2,-Width) %>%
           slice(-143,-144,-145))
summary(mod4a)

























