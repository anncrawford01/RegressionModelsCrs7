# chapt 9 multivarite tips and tricks


n = 100
x2 <- 1 : n
x1 = .01 * x2 + runif(n, -.1, .1)
y = -x1 + x2 + rnorm(n, sd = .01) 
summary(lm(y ~ x1))$coef

## unadjusted
#Estimate Std. Error t value  Pr(>|t|)
#(Intercept)    1.454      1.079   1.348 1.807e-01
#x1            96.793      1.862  51.985 3.707e-73

## adjusted
summary(lm(y ~ x1 + x2))$coef
#Estimate Std. Error  t value   Pr(>|t|)
#(Intercept)  0.001933  0.0017709    1.092  2.777e-01
#x1          -1.020506  0.0163560  -62.393  4.211e-80
#x2           1.000133  0.0001643 6085.554 1.544e-272

dat = data.frame(y=y, x = x1, ey = resid(lm(y ~ x2)), ex1 = resid( lm(x1 ~ x2)))
library(ggplot2)
g = ggplot(dat, aes( y=y, x = x1, colour = x2))
g = g + geom_point(color = "grey50", size = 5) + geom_smooth(method = lm, se = FALSE, color = "black") + geom_point(size = 4)
g

## plot the residuals- this is the coeff with lm x1 and x2( x2 is NOT related to residual x1)
g2  = ggplot(dat, aes(y = ey, x = ex1, color = x2))
g2 = g2 + geom_point(colour = "grey50", size = 5) + geom_smooth(method = lm, se = FALSE, colour = "black") +
                                                                        geom_point(size = 4)
g2

## factor variables as predictors are simply special cases of regression models   ###

require(datasets);data(InsectSprays); require(stats); require(ggplot2)
g = ggplot(data = InsectSprays, aes(y = count, x = spray, fill  = spray))
g = g + geom_violin(colour = "black", size = 2)
g = g + xlab("Type of spray") + ylab("Insect count")
g

summary(InsectSprays)

# compare B to A (B-A) 0.8333, C to A (C-A) -12.4167, intercept 14.5 is the mean of spray A
# inferential stats are testing if mean A is zero, est mean of B is 14.5 + 0.833, 
# est mean C is -12.41 + 14.5
summary(lm(count ~ spray, data = InsectSprays))$coef

## same are summary above, i.e interctp = 14.5
summary(lm(count ~
                   I(1 * (spray == 'B')) + I(1 * (spray == 'C')) +  
                   I(1 * (spray == 'D')) + I(1 * (spray == 'E')) +
                   I(1 * (spray == 'F')) + I(1 * (spray == 'A')), data = InsectSprays))$coef

## drop the intercept and the coeffs are the means for each spray 
summary(lm(count ~ spray - 1, data = InsectSprays))$coef

## show the means = coeff without the intercept
library(dplyr)
summarise(group_by(InsectSprays, spray), mn = mean(count))


### AnCOVA 
# how to fit multiple lines

library(datasets)
data(swiss)
head(swiss)

