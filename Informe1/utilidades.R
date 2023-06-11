#####################
######CLASE 3 #######
#####################
install.packages("readxl")
install.packages("tidyverse")
library(readxl)
str(Orders_East) #ver tipo de variables
Orders_East$Ventas <- as.numeric(Orders_East$Ventas) #tranforma la columna a numeros
library(tidyverse)

Orders_East %>%             # da el total de la varible en el parametro, en este caso "state"
  group_by(State) %>%  
  tally()

base1 = Orders_East %>%     #crea tabla con la variable "segment" con el total de datos, media de ventas y venta minima
  group_by(Segment) %>% 
  summarise(total = n(),
            media_ventas= mean(Ventas), #la media
            venta_min = min(Ventas),# minimo}
            venta_max = max(Ventas))#maximo
base1

base2 = Orders_East %>% 
  group_by(State) %>% 
  summarise(total = n(),
            media_ventas= mean(Ventas),
            venta_min = min(Ventas)) 

base2

barplot(total ~ Segment, data = base1)    # grafico de barras
barplot(media_ventas ~ Segment, data = base1)
barplot(venta_min ~ Segment, data = base1)
barplot(total ~ State, data = base2,horiz = TRUE) #Grafico de barras invertido

pie
pie(base1$total,base1$Segment) "graficoi de torta"
pie(base2$total,base2$State)
base3 = Orders_East %>% filter(Ventas<=1000) #crear una base con un filtro

hist(baseInforme$TEMP) #crea histograma

hist(rnorm(n=30,mean=5,sd=4) ) #buscar que hace"

Orders_East$Ventas

length(Orders_East$Ventas)
ventas = sort(Orders_East$Ventas) #ordena de menor a mayor


(ventas[1403]+ventas[1404])/2 #media

median(ventas) # media

median(Orders_East$Ventas)

summary(Orders_East$Ventas) #buscar que hace






