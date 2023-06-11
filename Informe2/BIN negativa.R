base_datos = base_datos %>% 
  mutate(SO2_alto = ifelse(SO2 >= 90,1,0))
probs = NULL

n2 = round(length(base_datos$SO2)*0.2,0)
for (i in 1:1000) {
  sample.aux = base_datos$SO2_alto %>% sample(n2,replace=F)
  probs = c(probs,mean(sample.aux))
  
}
p = mean(probs)
p

