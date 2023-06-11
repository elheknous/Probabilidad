siesta

library(ggplot2)
ggplot(siesta,aes(wine,sleep))+
  geom_point()+
  geom_smooth(method = "lm",
              formula = y ~ x, col = "orange")

modelo1 = lm(sleep ~ wine,data = siesta)
summary(modelo1)