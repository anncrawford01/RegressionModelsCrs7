## Quiz4
library("MASS")
##Q1
?shuttle
head(shuttle)
table(shuttle$use)
table(shuttle$wind)

library(dplyr)

shuttle <- mutate(shuttle, autolander = ifelse(use == "auto", 1,0))

fit1 <-glm(autolander ~ wind, data = shuttle, family = "binomial")

summary(fit1)

exp(fit1$coeff)

windtail <-exp(fit1$coeff)[2]
windhead <-exp(fit1$coeff)[1]

windhead/windtail

##Q3    ####

#Q4             ###


#Q5  ?influence.measures   ####


# Q6    ###



