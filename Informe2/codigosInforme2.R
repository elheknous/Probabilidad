
library(dplyr)   # manipulacion de datos
library(ggplot2) # visualizacion
library(readxl) # leer excel y escribir excel
library(moments) # asimetria y curtosis
library(tidyverse)
library(nortest)
library(gamlss)
library(gamlss.dist)
library(fitdistrplus)

######################################

no2 = base_datos$NO2
no2modificado = (base_datos %>% filter(NO2 < 142))$NO2

no2 = no2modificado
fw <-fitdist(no2,"weibull")
fln<-fitdist(no2,"lnorm")
fg <-fitdist(no2,"gamma")
fn <-fitdist(no2,"norm")
fe <-fitdist(no2,"exp")
# fb <-fitdist(no2,"beta") no se mueve entre 0 y 1
plot.legend<-c("Weibull","LogNormal","Gamma","Normal","Exponencial")

denscomp(list(fw,fln,fg,fn,fe), legendtext=plot.legend)
qqcomp(list(fw,fln,fg,fn,fe), legendtext=plot.legend)
cdfcomp(list(fw,fln,fg,fn,fe), legendtext=plot.legend)
ppcomp(list(fw,fln,fg,fn,fe), legendtext=plot.legend)


#chao exponeencial

gofstat(list(fg,fn,fln,fw,fe))
ks.test(no2,"pgamma",fg$estimate[1],fg$estimate[2]) #0.001663

pnorm()
ks.test(no2,"pnorm",mean = fn$estimate[1],sd = fn$estimate[2]) ##MAYOR QUE GAMMA 0.03113

pweibull()
ks.test(no2,"pweibull",shape=fw$estimate[1],scale=fw$estimate[2]) #### PREGUNTAR PARAMETROS 0.2535

0.03113 >  0.001663
###########################################################

# X : Nivel de temperatura
SO2 = base_datos$SO2
fw <-fitdist(SO2,"weibull")
fln<-fitdist(SO2,"lnorm")
fg <-fitdist(SO2,"gamma")
fn <-fitdist(SO2,"norm")
fe <-fitdist(SO2,"exp")
plot.legend<-c("Weibull","LogNormal","Gamma","Normal","Exponencial")

denscomp(list(fw,fln,fg,fn,fe), legendtext=plot.legend)
qqcomp(list(fw,fln,fg,fn,fe), legendtext=plot.legend)
cdfcomp(list(fw,fln,fg,fn,fe), legendtext=plot.legend)
ppcomp(list(fw,fln,fg,fn,fe), legendtext=plot.legend)

gofstat(list(fg,fn,fln,fw,fe))


#LNORMAL






#no tiene distribucion continua estudiada






