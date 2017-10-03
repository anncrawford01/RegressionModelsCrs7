## Quiz4
library("MASS")
## Q1 #####################
?shuttle
head(shuttle)
table(shuttle$use)
table(shuttle$wind)

library(dplyr)
# odds of using autolander base on headwind or tail wind
shuttle <- mutate(shuttle, autolander = ifelse(use == "auto", 1,0))

fit1 <-glm(autolander ~ wind , data = shuttle, family = binomial(link = "logit"))
fit1$coeff

1-fit1$coeff



library(MASS)
data(shuttle)
## Make our own variables just for illustration
shuttle$auto <- 1 * (shuttle$use == "auto")
shuttle$headwind <- 1 * (shuttle$wind == "head")
fit <- glm(auto ~ headwind, data = shuttle, family = binomial)
exp(coef(fit))

## Another way without redifing variables
fit <- glm(relevel(use, "noauto") ~ relevel(wind, "tail"), data = shuttle, family = binomial)
exp(coef(fit))



## Q2 ##############'

shuttle <- mutate(shuttle, autolander = ifelse(use == "auto", 1,0))
table(shuttle$magn)

fit2 <-glm(autolander ~ wind + magn ,data = shuttle, family = binomial(link = "logit"))

1-fit2$coeff

shuttle$auto <- 1 * (shuttle$use == "auto")
shuttle$headwind <- 1 * (shuttle$wind == "head")
fit <- glm(auto ~ headwind + magn, data = shuttle, family = binomial)
exp(coef(fit))


##Q3    ####


shuttle <- mutate(shuttle, autolander = ifelse(use == "auto", 1,0))
table(shuttle$magn)

fit3 <-glm(autolander ~ wind  ,data = shuttle, family = "binomial")
exp(fit3$coeff)

fit3a <-glm(autolander ~ wind -1 ,data = shuttle, family = "binomial")
exp(fit3a$coeff)

# Q4      #######

fit <- glm(count ~ relevel(spray, "B"), data = InsectSprays, family = poisson)
exp(coef(fit))[2]




# Q5   ####

#The coefficient estimate is unchanged

# Q6    ###

x <- -5:5
y <- c(5.12,3.93,2.67, 1.87,0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)

#plot(x,y)

z <- (x > 0) * x
fit <- lm(y ~ x + z)
coef(fit)
sum(coef(fit)[2:3])





