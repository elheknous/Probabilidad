airquality <- read_excel("C:/Users/josem/Downloads/airquality.xlsx")

library(MASS)

stepAIC(modelo,
        direction = "backward")

mb = stepAIC(lm(AQI ~ ., data = airquality))

summary(mb)

#normalidad
plot(density(mb$residuals))
autoplot(mb)[2]
lillie.test(mb$residuals)
# el modelo no cumple con la normalidad
# indepndendia

autoplot(mb)[c(1,3)]
dwtest(mb)

#no cumple independencia

bptest(m3)


# el modelo no cumple nigun supuesto

nuevo = data.frame(PM2.5 = 65,
                   PM10 = 110,
                   NO2 = 27.2,
                   NOx = 15,
                   CO = 0.33,
                   O3 = 134,
                   Benzene = 0.23,
                   Toluene = 5.3,
                   Xylene = 0.2)


predict(mb,
        newdata = nuevo,
        interval = c("confidence"),
        level = 0.95)



