
siesta <- read.csv("C:/Users/jose/Downloads/siesta.csv")
head(siesta)

library(ggplot2)
ggplot(siesta,aes(wine, sleep))+
  geom_point()+
  geom_smooth(method = 'lm',
              formula = y~x,col="red")



m1 <- lm(sleep ~ wine, data = siesta)
summary(m1)
