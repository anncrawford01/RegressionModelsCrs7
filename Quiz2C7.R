## Regression Models Course 7

## Quiz 2

## Q1 ###

## https://www.zoology.ubc.ca/~schluter/R/fit-model/

x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

plot(x,y)

fit <-lm(y ~ x)

## this plots the residuals vs fittted, Normal Q-Q, Scale-Location, Residuals vs Leverage
par(mfrow=c(2,2))      
plot(fit)
summary(fit)

#### test NULL hypothesis mean = 0 - get pvalue
summary(lm(y ~ x))
  
## Q2 ###
summary(fit)$sigma
  
## Q3 ###
#http://www.r-tutor.com/elementary-statistics/simple-linear-regression/prediction-interval-linear-regression
summary(mtcars)
attach(mtcars)

mpg.lm <- lm( mpg ~ wt )

avgwt <- mean(wt)
newdata = data.frame(wt=avgwt)
#interval type as "confidence", and use the default 0.95 confidence level.
predict(mpg.lm, newdata, interval="confidence") 
detach(mtcars)

## method 2

mpg.lm <- lm( mpg ~ wt , data = mtcars)
fit <- lm( mpg ~ I(wt - mean(wt) ), data=mtcars)
confint(fit)






## Q4 ###
#[, 6]	 wt	 Weight (1000 lbs)
?mtcars 

## Q5 ###

attach(mtcars)

mpg.lm <- lm( mpg ~ wt )

avgwt <- mean(wt)
newdata = data.frame(wt=3)
#interval type as "confidence", and use the default 0.95 confidence level.
predict(mpg.lm, newdata, interval="predict") 
detach(mtcars)

#slope <- fit$coefficients[2]
#intercept <-fit$coefficients[1]
#y = mx + b
 #slope * 3 + intercept

## Q6 ###
#68-95-99.7 
mpg.lm <- lm( mpg ~ wt , data = mtcars)
fit <- lm( mpg ~ I(wt - mean(wt) ), data=mtcars)
# take slope and multiply by 2
confint(fit)[2]*2   



 

 ## Q9 ###  ???
 fit <- lm( mtcars$mpg ~ mtcars$wt )
 
 
 num <- sum((mtcars$mpg - fitted.values(fit))^2)
 
 ## only intercept
 denom <- sum((mtcars$mpg - mean(mtcars$mpg))^2)   
 
num/denom

