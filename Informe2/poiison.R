mtcars
library(dplyr)
auto <- mtcars %>% mutate(calidad = ifelse(mpg>=19,1,0))
auto


n1 = ceiling(dim(base_datos)[1]*0.3)

n1

lambdas = NULL

for (i in 1:100) {
  sample.aux = base_datos %>%  slice(sample(1:dim(base_datos)[1],size=n1)) %>%
    group_by(hour) %>% dplyr::summarise(tasa = sum(NO2_alto))
  tasa.aux = mean(sample.aux$tasa)
  lambdas = c(lambdas,tasa.aux)
  
}

lambda = ceiling(mean(lambdas))
lambda #ESPERANZA
