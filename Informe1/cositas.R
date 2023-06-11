library(dplyr)   # manipulacion de datos
library(ggplot2) # visualizacion
library(readxl) # leer excel y escribir excel
library(moments) # asimetria y curtosis
library(tidyverse)
library(nortest)

baseInforme <- MATUS_TORO_JOSE


##TABLA DE FRECUNCUENCIA DE los meses
mes <- baseInforme$month
tablaFrecuenciaMes <- as.data.frame(table(mes))
tablaFrecuenciaMes <- transform(tablaFrecuenciaMes, 
                  freAcumulada = cumsum(tablaFrecuenciaMes$Freq),
                  frecRelativa = round(prop.table(tablaFrecuenciaMes$Freq),3),
                  frecRelativaAcumulada = round(cumsum(prop.table(tablaFrecuenciaMes$Freq)),3))


##CREA UNA TABLA CON LOS MESES LA MEDIA, MIN Y MAX DE TEMPEARTURA
base1 = baseInforme %>%     
  group_by(month) %>% 
  summarise(total = n(),
            media_temperatura= mean(TEMP), #la media
            temperatura_min = min(TEMP),# minimo}
            temperatura_max = max(TEMP))#maximo
base1

baseMediaTemp <- baseTemp$media_temperatura
baseMedia

baseMinTemp <- baseTemp$temperatura_min
baseMinTemp

baseMaxTemp <- baseTemp$temperatura_max
baseMaxTemp

#RESUMEN DATOS
summary(baseInforme$TEMP) 

##TABLA DE MEDIA Y DESV.ESTANDAR
baseInforme %>% 
  summarise(media = mean(TEMP),
            mediana = median(TEMP),
            desv.est = sd(TEMP),
            asimetria = skewness(TEMP),
            curtosis =kurtosis(TEMP))



##ALGO
baseInforme%>% 
  ggplot(aes(x =month,y=TEMP)) +
  geom_boxplot() + coord_flip() +
  geom_jitter()


#SOLO TOMA EL MES DESEADO
base_mes = baseInforme %>% filter(month=="3")  
summary(base_mes$TEMP)
skewness(base_mes$TEMP)

##HISTOGRAMA MEDIO RARO
base_mes %>% 
  ggplot(aes(x=TEMP)) +
  geom_histogram(bins = 15)



#GRAFIOC DE CAJAS DE SO2
g11 = baseInforme %>%   ##PRIMER PASO
  ggplot(aes(y=NO2)) +
  geom_boxplot()# +  
  #coord_flip() 
g11

g22 = baseInforme %>%   ##PRIMER PASO
  ggplot(aes(y=SO2)) +
  geom_boxplot() #+  
  #coord_flip() 
g22
mean(baseInforme$TEMP)

grid.arrange(g11,g22,ncol=2)



##DENSIDAD

datos_TEM = baseInforme %>% 
  filter(TEMP == "tenm") %>% 
  ggplot(aes(month,  color=factor(TEMP))) +
  geom_density() +
  geom_vline(xintercept = 61.5)
datos_TEM
 
hist(x = baseInforme$SO2, main = "Histograma de dioxido de azufre", 
     xlab = "SO2", ylab = "Mes",
     col = "purple")


a <- ggplot(baseInforme, aes(x = TEMP))+
  geom_histogram(aes(y = ..density..), colour = "black",fill="white")+
  geom_density(alpha = .8, fill = "#FF6666")+
  geom_vline(aes(xintercept=median(TEMP)),
  color="blue", linetype="dashed", size=1)
  
a

hist(baseInforme$TEMP) 


iris %>% group_by(Species) %>% summarise(asimetria = skewness(Petal.Length),
                                         curtosis =kurtosis(Petal.Length)-3 )

temperatura = baseInforme %>%   
  ggplot(aes(y=TEMP)) +
  geom_boxplot() +  
  coord_flip() 
temperatura

summary(baseInforme$TEMP)

#########
baseTemp = baseInforme %>%     
  group_by(month) %>% 
  summarise(total = n(),
            media_temperatura= mean(TEMP), #la media
            temperatura_min = min(TEMP),# minimo}
            temperatura_max = max(TEMP))#maximo
baseTemp

baseMediaTemp <- baseTemp$media_temperatura
mediaTemp = baseTemp %>%   
  ggplot(aes(y=media_temperatura)) +
  coord_flip() +
  geom_boxplot()


baseMinTemp <- baseTemp$temperatura_min
minTemp = baseTemp %>%   
  ggplot(aes(y=temperatura_min)) +
  coord_flip() +
  geom_boxplot()


baseMaxTemp <- baseTemp$temperatura_max
maxTemp = baseTemp %>%   
  ggplot(aes(y=temperatura_max)) +
  coord_flip() +
  geom_boxplot()

grid.arrange(mediaTemp,minTemp,maxTemp,ncol=2)

#########################

s = baseInforme %>%   
  ggplot(aes(y=PM2.5)) +
  geom_boxplot() +  
  coord_flip() 
s
summary(baseInforme$TEMP)

