#################
#### clase 4 ####
################
install.packages("moments")

install.packages("tidyverse")
library(dplyr)   # manipulacion de datos
library(ggplot2) # visualizacion
library(readxl) # leer excel y escribir excel
library(moments) # asimetria y curtosis
library(tidyverse)

base <- read_excel("C:/Users/jose/Downloads/Orders_East.xlsx")
str(base)
base$Ventas <- as.numeric(base$Ventas)

base %>%  #grafico de barra con colores 
  ggplot(aes(Category,fill=Category))+
  geom_bar()

base %>%  #grafico de barra sde varias variables 
  ggplot(aes(Category,Ventas,fill=Segment)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  scale_fill_brewer(palette = "Set2") # cambiar colores

#datos
cont.segment <- base %>%  #cuenta la cantidad de la  variable solicitada 
  group_by(Segment) %>%
  summarise(n=n()) %>%
  arrange(n)

install.packages("ggplot2")
library(dplyr)   # manipulacion de datos

### no funcionaaaaa ######
library(ggplot2) # visualizacion
library(tidyverse)
ggplot(cont.segment,
       aes(x="", y=n, fill=Segment)) +
  geom_bar(width = 1, stat = "identity") +
  theme_void() +
  scale_fill_brewer("Blues") +
  coord_polar("y", start=0,direction = 1) +
  geom_text(
    aes(x=1.67,
        y = n/2 + c(0, cumsum(n)[-length(n)]),
        label = paste(round(n/sum(n),2)*100,"%")),
    size=5)
 #NO FUNCIONAAAAAA#####


library(tidyverse)
base %>% 
  ggplot(aes(x = `Sub-Category`, y = Ventas,
             fill = `Sub-Category`)) + # colorea 
  geom_boxplot(alpha=0.4) +
  theme_bw(base_size=10) +
  theme(legend.position = "none") +
  labs(x = "EJE X", y = "EJE Y",
       title = "TITULO",
       subtitle = "SUBTITULO",
       caption = "Elaboracion propia")

table(base$Category) # CANTIDAD DE ELEMENTOS
table(base$Category,base$`Ship Mode`) # Crea tabla con las categorias y el modo en que se compran
addmargins(table(base$Category,base$`Ship Mode`)) # Crea tabla con las categorias y el modo en que se compran añadiendo el total

prop.table(table(base$Category,base$`Ship Mode`),margin = 2) # columna 100% porcentaje 
prop.table(table(base$Category,base$`Ship Mode`),margin = 1) # fila 100% porcentaje

addmargins(prop.table(table(base$Category,base$`Ship Mode`),margin = 1)) # total

table(base$Category) #F absoluta
cumsum(table(base$Category))# F abssoluta acumulada

base %>% group_by(State) %>% tally() %>% arrange(desc(n)) # crea tabla de las ciudades con sus valores

# quantiles ----

quantile(base$Ventas, probs = c(0.1,0.3,0.5) )

quantile(base$Ventas,1 ) # percentil
quantile(base$Ventas,c(0.25,0.5,0.75) ) # cuartil
quantile(base$Ventas,0.70 ) # decil

# ?mean
# ?quantile

summary(base$Ventas) # Entrega una buena base de datos !!"!!!!!!! mean: media aritmetica 


###### nuevo excel #######
library(moments)

library(tidyverse)
empleados %>%  ## cuartiles graficados
  ggplot(aes(x =sexo,y=Salario_inicial)) +
  geom_boxplot() +
  facet_wrap( ~ Categoria )



empleados%>% filter(Salario_actual < max(empleados$Salario_actual)) %>% 
  ggplot(aes(x =Categoria,y=Salario_actual)) +
  geom_boxplot() + coord_flip() +
  geom_jitter()

1+ 3.322*log(dim(empleados %>% filter(Categoria=="Directivo"))[1],base = 10) #amplitud del intervalo

empleados %>% 
  group_by(Categoria) %>%  ## tabla que muestra la categoria con la media y desviacion estandar
  summarise(media = mean(Salario_actual),
            desv.est = sd(Salario_actual)) ## desviacion estandar



empleados %>% filter(Salario_actual < max(empleados$Salario_actual)) %>% ## sin contar los que ganan mas
  group_by(Categoria) %>% 
  summarise(media = mean(Salario_actual),
            desv.est = sd(Salario_actual))


names(empleados)

base_dir = empleados %>% filter(Categoria=="Directivo") # solo toma a los directivos 

base_dir %>% ##histograma del salario de los directivos 
  ggplot(aes(x=Salario_actual)) +
  geom_histogram(bins = 8)

skewness(base_dir[,"Salario_actual"]) # #SESGO NEGATIVO IZQ, POSITIVO DERECHA

kurtosis(base_dir[,"Salario_actual"]) # CURTOSIS, =0 MESOCURTICA, K<O PLATICURTICA, K>0 LEPTOCURTICA

base_dir %>% 
  ggplot(aes(x=Salario_actual)) +
  geom_histogram(bins = 8,fill="Steelblue")+
  geom_vline(xintercept = mean(base_dir$Salario_actual),col="gold")+  ## mean: promedio
  geom_vline(xintercept = median(base_dir$Salario_actual),col="tomato") ## mediam: mediana 




