## Análisis de la Tem. Min.

```{r}
min(baseTemp$temperatura_min)
```

```{r}
mesMenor <- baseInforme %>% filter(month == "12")
mesMenor
```

```{r message = FALSE, warning = FALSE}
histHoras <- ggplot(mesMenor, aes(x = TEMP))+
  geom_histogram(aes(y = ..density..), colour = "black",fill="white")+
  geom_density(alpha = .8, fill = "#FF6666")+
  geom_vline(aes(xintercept=median(TEMP)),
             color="blue", linetype="dashed", size=1)
histHoras

mesMenor%>% 
  summarise(asimetria = skewness(TEMP),
            curtosis =kurtosis(TEMP)-3)
```

```{r}
horaPromedio = mesMenor %>%
  group_by(hour) %>%
  summarise(total = n(),
            mediaTemperatura = mean(TEMP))
horaPromedio

```

```{r}
min(horaPromedio$mediaTemperatura)



so2 %>% 
  summarise(media = mean(TEMP),
            mediana = median(TEMP),
            desv.est = sd(TEMP),
            asimetria = skewness(TEMP),
            curtosis =kurtosis(TEMP)-3)
no2 %>% 
  summarise(media = mean(TEMP),
            mediana = median(TEMP),
            desv.est = sd(TEMP),
            asimetria = skewness(TEMP),
            curtosis =kurtosis(TEMP)-3)

no2 = baseInforme %>%   
  ggplot(aes(y=NO2)) +
  geom_boxplot() +  
  coord_flip() 


grid.arrange(so2,no2, ncol = 1)
```