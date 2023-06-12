library(car)

M1 <- lm(mpg ~ wt, data = mtcars)
outlierTest(M1)
influencePlot(M1)
