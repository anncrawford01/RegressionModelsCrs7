## Quiz3

##Q1
str(mtcars)

fit <- lm(mpg ~ factor(cyl) + wt, data = mtcars)
summary(fit)$coef

##Q2    ####
fit2 <- lm(mpg ~ factor(cyl) + wt, data = mtcars)
summary(fit2)$coef

fit2U <- lm(mpg ~ factor(cyl) , data = mtcars)
summary(fit2U)$coef



##Q3    ####
#an interaction is formed by the product of two or more predictors
#https://www.r-bloggers.com/r-tutorial-series-regression-with-interaction-variables/
fit3 <- lm(mpg ~ I(factor(mtcars$cyl)) + wt, data = mtcars)
summary(fit3)$coef

fit3I <- lm(mpg ~ I(factor(mtcars$cyl)) * wt, data = mtcars)
summary(fit3I)$coef
#  strong corrlation
cor(mtcars$wt,mtcars$cyl)
anova(fit3, fit3I)

#Q4             ###
#A data frame with 32 observations on 11 variables.
#
#[, 1]	 mpg	 Miles/(US) gallon
#[, 2]	 cyl	 Number of cylinders
#[, 3]	 disp	 Displacement (cu.in.)
#[, 4]	 hp	 Gross horsepower
#[, 5]	 drat	 Rear axle ratio
#[, 6]	 wt	 Weight (1000 lbs)
#[, 7]	 qsec	 1/4 mile time
#[, 8]	 vs	 V/S
#[, 9]	 am	 Transmission (0 = automatic, 1 = manual)
#[,10]	 gear	 Number of forward gears
#[,11]	 carb	 Number of carburetors
lm(mpg ~ I(wt * 0.5) + factor(cyl), data= mtcars )


#Q5  ?influence.measures   ####
0

x <- c(0.568, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
plot(x,y)
fit5 <- lm(y ~ x)
# hat diagonal values are the diagonal elements of the least-squares projection matrix H
# indicate the amount of leverage (influence) that observations have in a least squares regression.
# The hat diagonals lie between 1/n and 1 and their average value is p/n where p is the number of variables
# s, i.e., the number of columns of x (plus 1 if intercept = TRUE
#  and n is the number of observations (the number of rows of x
hat(x)

influence(lm(y~x))$hat

## showing how it's actually calculated
xm <- cbind(1, x)
diag(xm %*% solve(t(xm) %*% xm) %*% t(xm))

# Q6    ###
x <- c(0.568, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)


fit6 <- lm(y ~ x)
fit6no <- lm(y[-5] ~ x[-5])
fit6no <- lm(y ~ x[-5])
dfbeta_no_s <- fit6$coeff[2] - fit6no$coeff[2]

numerator<-(fit6$coef[2] - fit6no$coef[2])
denominator<-sqrt((summary(fit6no)$sigma^2)*diag(summary(fit6)$cov.unscaled)[2])
dfbetas_with_s <- numerator/denominator
dfbetas_with_s
#source: https://stats.stackexchange.com/questions/141243/how-to-manually-calculate-dfbetas

dfbetaPlots(fit6)
