## Quiz4
library("MASS")
##Q1
?shuttle
head(shuttle)
table(shuttle$use)
table(shuttle$wind)

library(dplyr)
# odds of using autolander base on headwind or tail wind
shuttle <- mutate(shuttle, autolander = ifelse(use == "auto", 1,0))

fit1 <-glm(autolander ~ wind , data = shuttle, family = "binomial")
fit1$coeff
exp(coef(fit1))
exp(fit1$coef)

windtail <-exp(fit1$coeff)[2]

(1-windtail)/windtail



## Q2 ##############'

shuttle <- mutate(shuttle, autolander = ifelse(use == "auto", 1,0))
table(shuttle$magn)

fit2 <-glm(autolander ~ wind + magn ,data = shuttle, family = "binomial")

summary(fit2)

exp(fit2$coeff)

windtail <-exp(fit1$coeff)[2]



(1-windtail)/windtail

##Q3    ####


shuttle <- mutate(shuttle, autolander = ifelse(use == "auto", 1,0))
table(shuttle$magn)

fit3 <-glm(autolander ~ wind  ,data = shuttle, family = "binomial")
exp(fit3$coeff)

fit3a <-glm(autolander ~ wind -1 ,data = shuttle, family = "binomial")
exp(fit3a$coeff)

# Q4      #######

fit4 = glm(count ~ spray -1 ,family="quasipoisson", data = InsectSprays)
exp(fit4$coeff)

exp(fit4$coeff)[1]/exp(fit4$coeff)[2]



# Q5   ####



# Q6    ###

x <- -5:5
y <- c(5.12,3.93,2.67, 1.87,0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)

#plot(x,y)

fit6 <- lm( y ~ x)
fit6$coeff





