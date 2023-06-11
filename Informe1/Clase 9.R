# Clase 9
library(dplyr)   # manipulacion de datos
library(ggplot2) # visualizacion
library(readxl) # leer excel y escribir excel
library(moments) # asimetria y curtosis
library(tidyverse)
library(nortest)
install.packages("gridExtra")
#library(writexl) # escribir excel

# Datos

# ssc_p:- Secondary Education(%) 10th Grade
# 
# ssc_b:- 10th Board of Education
# 
# hsc_p:- Higher Secondary Education(%) 12th Grade
#   
# hsc_b:- 12th Board of Education
# 
# hsc_s:- Specialization in Higher Secondary Education
# 
# degree_p:- Undergraduate (%)
# 
# degree_t:- Undergraduate degree type
# 
# workex:- Work experience
# 
# etest_p:- Placement test (%)
# 
# specialisation:- MBA specialisation
# 
# mba_p:- MBA (%)
# 
# status:- Hiring status

# analisis en python realizado
# https://github.com/ShuklaPrashant21/Campus_Recruitment/blob/master/Explanatory_Data_Analysis.ipynb


#placement <- read.table("https://raw.githubusercontent.com/ShuklaPrashant21/Campus_Recruitment/master/Placement_Data_Full_Class.csv",header = T,sep =",")

base <- read_excel("C:/Users/jose/Downloads/Placemen2t.xlsx")


#View(base %>% mutate(id = 1:215 )) #añade una columna con el id(numero de posicion)

base <- base %>% mutate(id = 1:215 )

#View(base %>% mutate(Clasificacion = ifelse(ssc_p > 75,"ALTA","BAJA") ))

base <- base %>% mutate(Clasificacion = ifelse(ssc_p > 75,"ALTA","BAJA") ) #añade alta/baja segun el ssc_p


# variables numericas

summary(base$ssc_p)

base %>% filter(ssc_p>100) # son % por lo que deben ser menores que 100% (1)
base$ssc_p[121]
base$ssc_p[123]


# library(GGally)
# 
# ggpairs(base)

#view(base %>% filter(ssc_p<=100)) # se eliminan los ssp_p > 100
base %>% filter(ssc_p<=100) %>% 
  ggplot(aes(y=ssc_p)) +
  geom_boxplot()

# datos atipicos / outliers
base %>% filter(ssc_p <25) 

# plot con todos los datos
g1 = base %>%   ##PRIMER PASO
  ggplot(aes(y=ssc_p)) +
  geom_boxplot()
g1
#' revisando la base encontramos 2 valores: 215 y 315...
#' que los consideraremos errores de tipeo y tambien encontramos
#' 2 valores atipicos: 15.1 y 1.1, los cuales quitaremos del
#' analisis.
#'  

g2 = base %>% filter(ssc_p<=100,ssc_p>20) %>%  #QUITANDO DATOS ATIPICOS
  ggplot(aes(y=ssc_p)) +
  geom_boxplot()

library(gridExtra)

grid.arrange(g1,g2,ncol=2) ## comparacion entre cuartiles de todos los datos y los datos modificasdos

# resumen de la variable CON los datos eliminados
summary(base$ssc_p[c(1:119,124:215)])

summary(base$ssc_p[-c(120,121,122,123)])

summary(base$ssc_p[-c(120:123)])

# ssc_p con la clasificacion

base %>% filter(ssc_p<=100,ssc_p>20) %>% 
  ggplot(aes(x=Clasificacion,y=ssc_p,fill=Clasificacion)) +
  geom_boxplot()

base %>% filter(ssc_p<=100,ssc_p>20) %>% 
  ggplot(aes(y=ssc_p)) +
  geom_boxplot() +
  facet_wrap(~Clasificacion)

# desde clase 5 el grafico
base %>% filter(ssc_p<=100,ssc_p>20) %>% 
  ggplot(aes(ssc_p,  color=factor(Clasificacion))) +
  geom_density() + #theme_minimal()+
  facet_wrap(~Clasificacion)

# ERROR, ES DE ESCALA

alta = base %>% filter(ssc_p<=100,ssc_p>20) %>% 
  filter(Clasificacion=="ALTA") %>% 
  ggplot(aes(ssc_p,  color=factor(Clasificacion))) +
  geom_density() 

baja = base %>% filter(ssc_p<=100,ssc_p>20) %>% 
  filter(Clasificacion=="BAJA") %>% 
  ggplot(aes(ssc_p,  color=factor(Clasificacion))) +
  geom_density() +
  scale_y_continuous(limits = c(0,0.075))


grid.arrange(baja,alta,ncol=1)

base %>% filter( ssc_p<=100 , ssc_p>20 ) %>% ##RESUMEN IMPORTANTE
  group_by(Clasificacion) %>% 
  summarise(asimetria = skewness(ssc_p),
            curtosis = kurtosis(ssc_p),
            media = mean(ssc_p),
            desv = sd(ssc_p))

# sobre variable degree

summary(base$degree_p)

base %>% 
  ggplot(aes(y=degree_p,fill=hsc_s)) +
  geom_boxplot()

base %>% filter(hsc_s=="Science") %>% 
  summarise(media = mean(degree_p),
            desv = sd(degree_p))

base %>% filter(hsc_s=="Science",degree_p>25) %>% 
  summarise(media = mean(degree_p),
            desv = sd(degree_p))

#' concluimos que el dato atipico nos afecta, por lo que lo
#' quitaremos del analisis
#' 

base %>% group_by(hsc_s) %>% 
  summarise(asimetria = skewness(degree_p),
            curtosis = kurtosis(degree_p),
            media = mean(degree_p),
            desv = sd(degree_p))

# sin datos atipicos
base %>% group_by(hsc_s) %>% 
  filter(degree_p<=100,degree_p>20) %>% 
  summarise(asimetria = skewness(degree_p),
            curtosis = kurtosis(degree_p)-3,
            media = mean(degree_p),
            desv = sd(degree_p))

#densidad
base %>% filter(degree_p<=100,degree_p>20) %>% 
  group_by(hsc_s) %>% summarise(media = mean(degree_p))

arte =base %>%   
  filter(hsc_s=="Arts") %>% 
  ggplot(aes(degree_p,  color=factor(hsc_s))) +
  geom_density() +
  geom_vline(xintercept = 61.5)
arte


comercio =base %>%   filter(degree_p<=100,degree_p>20) %>% 
  filter(hsc_s=="Commerce") %>% 
  ggplot(aes(degree_p,  color=factor(hsc_s))) +
  geom_density()+
  geom_vline(xintercept = 66.2)
comercio


ciencia =baseInforme %>%  
  ggplot(aes(month,  TEMP)) +
  geom_density()
ciencia


grid.arrange(arte,comercio,ciencia)




base %>% 
  group_by(gender) %>% 
  summarise(media = mean(salary,na.rm = TRUE))


