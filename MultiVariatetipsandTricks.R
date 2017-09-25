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