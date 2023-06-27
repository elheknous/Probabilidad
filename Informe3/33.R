library(dplyr)   # manipulacion de datos
library(ggplot2) # visualizacion
library(readxl) # leer excel y escribir excel
library(moments) # asimetria y curtosis
library(tidyverse) # manipulacion de datos
library(nortest)  # test normalidad
library(gamlss) # ajuste de distribuciones
library(car)   # vif()
library(lmtest) # durbin watson test
library(GGally) # para usar la matriz de correlaciones
library(ggfortify) # autoplot
library(ggcorrplot) # tabla de correlaciones (colores)
library(corrplot)
library(ppcor) # matriz de correlacion parcial
library()
dolar <- read_excel("C:/Users/josem/Downloads/dolar.xlsx")
dolar

# 1 ----

cor(dolar)

ggpairs(dolar)


corrplot(cor(dolar),method="color",
         addCoef.col = "black",   # agrega coefcientes 
         number.cex=.8,  # tamaño coef
         tl.col="black",  # color nombres de col y rows
         tl.srt=45) # angulo de rotación names col

#' dadas las correlaciones, podemos ver que la variable
#' ocuppation podria descartarse del modelo.
#' age y experience tiene una relacion lineal positiva muy fuerte
#' además, 
#' experiencia y educacion 
#' ocuppation y sector
#' podrian generar algun problema de multicolinealidad


# 2----
modfull <- lm(lwage ~ . ,data = dolar)
summary(modfull)

# lwage  = 1.07 + 0.17*education -0.10*south +0.22*sex + ...


# 3----
car::vif(modfull)
#' encotramos valores vif altos en las variables
#' education, experiencey age
#' veremos la correlacion parcial
#' 

ppcor::pcor(dolar)

modfull <- lm(lwage ~ . ,data = dolar)
summary(modfull) 

m1 <- lm(lwage ~ . ,
         data = dolar %>% dplyr::select(-education))

m2 <- lm(lwage ~ . ,
         data = dolar %>% dplyr::select(-experience))

m3 <- lm(lwage ~ . ,
         data = dolar %>% dplyr::select(-age))

m4 <- lm(lwage ~ . ,
         data = dolar %>% dplyr::select(-education,-experience,-age))

m5 <- lm(lwage ~ . ,
         data = dolar %>% dplyr::select(-experience,-age))


summary(m1)
vif(m1) 
# aun existe multicolinealidad, experience y age

summary(m2)
vif(m2)
# aqui los valores son cercanos a 1, por lo que la 
# multicolinelidad desaparece

summary(m3)
vif(m3)
# aqui los valores son cercanos a 1, por lo que la 
# multicolinelidad desaparece. 

# esto porque eliminamos en m2 y m3 una de las variables que 
# estaban muy correlacionadas entre ellas.

summary(m4)
vif(m4)
# aqui los valores son cercanos a 1, por lo que la 
# multicolinelidad desaparece. 

summary(m5)
vif(m5)  
# aqui los valores son cercanos a 1, por lo que la 
# multicolinelidad desaparece. 

# decision
# el m1 aun mantiene multicolinealidad, no lo cosideramos
# m2 R2 = 0.3058
# m3 R2 = 0.3060
# m4 R2 = 0.1296
# m5 R2 = 0.2610
# escogemos el m3 por tener un r2 mlevemente mayor

m3 <- lm(lwage ~ . ,
         data = dolar %>% dplyr::select(-age))
summary(m3)


m3a <- lm(lwage ~ . ,
          data = dolar %>% dplyr::select(-age,-occupation))
summary(m3a)

m3b <- lm(lwage ~ . ,
          data = dolar %>% dplyr::select(-age,-occupation,-race))
summary(m3b)

m3c <- lm(lwage ~ . ,
          data = dolar %>% dplyr::select(-age,-occupation,-race,-Married))
summary(m3c)

# a un 5% de significancia para los betas, escogemos el m3c
# a un 10% de significancia para los betas, escogemos el m3a

# m3c sera el utilizado

# 4----

mback1 <- MASS::stepAIC(lm(lwage ~ . , data=dolar),
                        direction="backward",
                        trace = F)

mback2 <- MASS::stepAIC(m3, # iniciando en modelo sin Multicol..
                        direction="backward",trace = F)

summary(mback1)
summary(mback2)

# 5 ---

# finalmente revisaremos el modelo m3c

# 6 ----

outlierTest(m3c)
a = influencePlot(m3c)

influyentesyatipicos = as.numeric(rownames(a))


m3c_inf <- lm(lwage ~ . ,
              data = dolar %>% 
                dplyr::select(-age,-occupation,-race,-Married) %>% # quita columnas
                dplyr::slice(-influyentesyatipicos)) # quita las filas
summary(m3c_inf)

# 7 ---

# normalidad

autoplot(m3c_inf)[2]
plot(density(m3c_inf$residuals))

nortest::lillie.test(m3c_inf$residuals)

# si cumple el supuesto de normalidad

# independencia


plot(m3c_inf,which = 1)
autoplot(m3c_inf)[1]

lmtest::dwtest(m3c_inf)

# aceptamos que no hay autocorrelacion entre los residuos

# homocedasticidad
autoplot(m3c_inf)[1]
lmtest::bptest(m3c_inf)


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

