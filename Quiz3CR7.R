## Quiz3

str(mtcars)


fit <- lm(mpg ~ factor(cyl) + wt, data = mtcars)
summary(fit)$coef

##Q2
fit2 <- lm(mpg ~ factor(cyl) + wt, data = mtcars)
summary(fit2)$coef

fit2U <- lm(mpg ~ factor(cyl) , data = mtcars)
summary(fit2U)$coef


##Q3
#an interaction is formed by the product of two or more predictors
#https://www.r-bloggers.com/r-tutorial-series-regression-with-interaction-variables/
fit3 <- lm(mpg ~ I(factor(mtcars$cyl)) + wt, data = mtcars)
summary(fit3)$coef

fit3I <- lm(mpg ~ I(factor(mtcars$cyl)) * wt, data = mtcars)
summary(fit3I)$coef

#Q4
lm(mpg ~ )







