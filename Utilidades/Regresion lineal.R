library(readr)
library(ggplot2)
se_Hoja_1 <- read_csv("C:/Users/josem/Downloads/se - Hoja 1.csv")
View(se_Hoja_1)
regresion <- lm(POBLA ~ YEAR, data = se_Hoja_1)
summary(regresion)

plot(se_Hoja_1$YEAR, se_Hoja_1$POBLA, xlab='Edad', ylab='Grasas')
abline(regresion)


dat = read_csv("C:/Users/josem/Downloads/pelota - Hoja 1.csv")
View(dat)

regre = lm(ALTURA ~ TIEMPO + I(TIEMPO^2),data = dat)
summary(regre)

a = dat$TIEMPO
b = dat$ALTURA
plot(a,b)
a
b
typeof(a)
ggplot(data = dat, aes(x = a, y = b)) + 
  geom_point() + 
  stat_smooth(method = "lm", formula = y~x + I(x^2), se = FALSE)


library(ggplot2)
ggplot(data = dat, aes(x = TIEMPO, y = ALTURA)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  labs(x = "Tiempo", y = "Altura") +
  theme_minimal()

