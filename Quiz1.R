#  quiz1 course 7 linear regression 

## Q1
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
z <- c(0.18,0.18, -1.54, 0.42,0.42,0.42, 0.95)
mean(z) ## mean with weights 
##
sum(x *w)/sum(w)


## Q2
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm( y ~ x -1)  ## slope though the origin 

coef(lm(y ~ x -1))
## or
sum(y*x)/sum(x^2)

## Q3
data(mtcars)
x <- mtcars$wt
y = mtcars$mpg
lm(y ~ x)

summary(lm( mpg ~ wt, data =mtcars))

## Q4

## not 0.25
##Q5

## not 0.16

##Q6 normalize X, - Xbar/ s
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
xbar <- mean(x)
s <- sd(x)
z <- (x - xbar)/s

##Q7

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm( y ~ x)

coef(lm( y ~ x))


##Q8
## if mean(x) = 0 and mean(y)=0 the intercept = 0
## The intercept estimate is $\bar Y - \beta_1 \bar X$ and so will be zero.

##Q9
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)

##Q10
## ??? not 
