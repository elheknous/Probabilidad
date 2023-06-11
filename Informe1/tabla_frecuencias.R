#####################
######CLASE 2 #######
#####################
names(pokemon)
ataquesPokemon <- pokemon$attack

tablaFrecuencia <- as.data.frame(table(ataquesPokemon))
tipi <- transform(tablaFrecuencia, 
          freAcumulada = cumsum(tablaFrecuencia$Freq),
          frecRelativa = round(prop.table(tablaFrecuencia$Freq),3),
          frecRelativaAcumulada = round(cumsum(prop.table(tablaFrecuencia$Freq)),3))
tipi
library(tidyverse)
pokemon %>%
  group_by(attack) %>% 
  tally()

base2 = pokemon %>% 
  group_by(generation) %>% 
  summarise(total = n(),
            media_ataques= mean(attack),
            ataques_min = min(attack))
base2
