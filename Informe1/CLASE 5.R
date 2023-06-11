# Clase 5

install.packages("nortest")
library(dplyr)   # manipulacion de datos
library(ggplot2) # visualizacion
library(readxl) # leer excel y escribir excel
library(moments) # asimetria y curtosis
library(tidyverse)
library(nortest)

paleta <- c("#FF5A5F", "#FFB400", "#007A87", 
            "#8CE071", "#7B0051", "#00D1C1", "#FFAA91", "#B4A76C", 
            "#9CA299", "#565A5C", "#00A04B", "#E54C20")

# C: the female crab's color, 
# S: spine condition, 
# W: weight in kilograms, 
# Wt: carapace width in centimeters,
# Sa: response outcome for each female crab is her number of satellites.

# data: cangrejos ----
cangrejos <- read.table(file='https://raw.githubusercontent.com/fhernanb/datos/master/crab', header=T)

cangrejos <- as_tibble(cangrejos)
cangrejos
# revisemos los datos: ----

plot(cangrejos$W[-165] )
abline(h=mean(cangrejos$W[-165]),col=2)
CV_w = sd(cangrejos$W[-165])/mean(cangrejos$W[-165]) * 100 ##Porque -165
CV_w

max(cangrejos$W)
which(cangrejos$W ==max(cangrejos$W)) # busca la posicion que cumple

#cangrejos  %>% filter(C==2) %>% 

#  ggplot(aes(x=1:95,y=W))+
#  geom_point()
  

cangrejos  %>% 
  ggplot(aes(W,  fill=factor(C))) +   #varios graficos
  geom_histogram(alpha=8) +theme_minimal() +
  facet_wrap(~C)+
  scale_fill_manual(values=paleta)

cangrejos  %>% 
  ggplot(aes(W,  color=factor(C))) + #grafico anterior pero con lineas
  geom_density() +theme_minimal()+
  scale_color_manual(values=paleta)+
  facet_wrap(~C)

plot(density(rnorm(30,0,1))) #PREGUNTAR


cangrejos  %>% filter(C==4) %>%   ##PREGUNTAR
  ggplot(aes(W,  fill=factor(C))) +
  geom_histogram(aes(y = after_stat(density)))+
  theme_minimal() + geom_density(alpha=0)+
  facet_wrap(~C)+
  scale_fill_manual(values=paleta) ->g1

cangrejos  %>% 
  ggplot(aes(W,  fill=factor(C))) +
  theme_minimal() + geom_density(alpha=0.3)+
  scale_fill_manual(values=paleta)


# coef. de variacion ----

cangrejos %>% group_by(C) %>% 
  summarise(media = mean(W),
            desv = sd(W),
            CV = desv/media) ##coeficiente de variacion}

# asimetria ----

asimetria <- function(x){ # x son los datos
  n     <- length(x)
  media = mean(x)    #  mean(x) -> media
  #desv  = sqrt((x-mean(x))^2/n)
  
  CA = sum((x-media)^3/n /(sum((x - mean(x))^2)/n)^(3/2))
  
  return(CA)
}

asimetria(cangrejos$W)

skewness(cangrejos$W) # moments #SESGO, COEFICIENTE DE ASIMETRIA

plot(density(cangrejos$W ))
abline(v=mean(cangrejos$W),col=2)
abline(v=median(cangrejos$W),col=3)


cangrejos %>% group_by(C) %>% 
  summarise(asi = skewness(W))

#obs: siempre considerar los graficos individuales

# curtosis  ----


x <- cangrejos$W
n <- length(x)
curtosis = (n * sum((x - mean(x))^4)/(sum((x - mean(x))^2)^2))-3

curtosis

kurtosis(cangrejos$W) # de libreria moments no considera el -3 de la formula 

cangrejos  %>% 
  ggplot(aes(W,fill="tomato")) +  ##GRAFICO DE DENSIDAD
  geom_histogram(aes(y = after_stat(density)))+
  theme_minimal() + geom_density(alpha=0)+
  scale_fill_manual(values=paleta) 

cangrejos  %>%                                          ##DENSIDAD CON GRAFICOS EN CURVA
  ggplot(aes(Wt,  fill=factor(S))) +
  geom_histogram(aes(y = after_stat(density)))+
  theme_minimal() + geom_density(alpha=0.3)+
  facet_wrap(~S)+  scale_fill_manual(values=paleta)

cangrejos  %>% 
  ggplot(aes(Wt,  fill=factor(S))) +                    ##grafico solo de lineas
  theme_minimal() + geom_density(alpha=0.3)+
  facet_wrap(~S)+  scale_fill_manual(values=paleta)


cangrejos %>% group_by(S) %>% summarise(asimetria = skewness(Wt), ##TABLA DE ASIMETRIA Y KURRTOSIS
                                        curtosis =kurtosis(Wt) )




# Ejemplos

# data: iris -----


# caso 1
iris  %>% 
  ggplot(aes(Sepal.Length,  fill=factor(Species))) +
  theme_minimal() + geom_density(alpha=0.3)+
  scale_fill_manual(values=paleta)+
  facet_wrap(~Species)

iris %>% group_by(Species) %>%
  summarise(asimetria = skewness(Sepal.Length),
            curtosis =kurtosis(Sepal.Length) )


# veamos en profundidad el caso setosa

iris  %>% filter(Species=="setosa") %>% 
  ggplot(aes(Sepal.Length,  fill=factor(Species))) +
  theme_minimal() + geom_density(alpha=0.3)+
  scale_fill_manual(values=paleta)+
  facet_wrap(~Species)

# caso 2

iris %>% group_by(Species) %>% summarise(media=mean(Petal.Length))

iris  %>%  #filter(Species=="virginica") %>% 
  ggplot(aes(Petal.Length,  fill=factor(Species))) +
  theme_minimal() + geom_density(alpha=0.3)+
  scale_fill_manual(values=paleta)+
  #geom_vline(xintercept = 5.55)+
  facet_wrap(~Species)

iris %>% group_by(Species) %>%
  summarise(asimetria = skewness(Petal.Length),
            curtosis =kurtosis(Petal.Length) -3 ) ##PREGUNTAR PORQUE LA SETOSA ESTA PARA LA IZAQUIERDA SI ES POSITIVO EL COSO

iris  %>% 
  ggplot(aes(x=Species,y=Petal.Length,  fill=factor(Species))) +  ##cuartiles??
  theme_minimal() + geom_boxplot(alpha=0.6) +  geom_jitter()+
  geom_line(stat = "density", size = 1) +coord_flip()+
  scale_fill_manual(values=paleta)







iris  %>% 
  ggplot(aes(Petal.Length,  fill=factor(Species))) +
  theme_minimal() + geom_density(alpha=0.3)+  scale_fill_manual(values=paleta)

iris %>% group_by(Species) %>% summarise(asimetria = skewness(Petal.Length),
                                         curtosis =kurtosis(Petal.Length)-3 )



# Ejercicio: ----
# data: Empleados -----
library(readxl)
#empleados = read_excel("/Users/aeantequeda/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/UFRO/2022/Estadistica/bases/empleados.xlsx")


empleados %>%  ##CUARTILES
  ggplot(aes(x =Categoria,y=Salario_actual,fill=Categoria)) +
  geom_boxplot() + coord_flip() +
  geom_jitter(alpha=0.5)+  scale_fill_manual(values=paleta)


empleados  %>%  filter(Categoria=="Administrativo") %>% 
  ggplot(aes(Salario_inicial,  fill=factor(Categoria))) +
  theme_minimal() + geom_density(alpha=0.3)+
  scale_fill_manual(values=paleta) +
  #geom_vline(xintercept = 5.55)+
  facet_wrap(~Categoria)

empleados %>% group_by(Categoria) %>% 
  summarise(asimetria = skewness(Salario_actual),
            curtosis =kurtosis(Salario_actual),
            desv = sd(Salario_actual))

