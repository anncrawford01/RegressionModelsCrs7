## homework reading Week 3.1 chapter 9  Multivariable tips tricks
# 1
libary(Datasets)
library(dplyr)
data("Seatbelts")
seatbelts <- as.data.frame(Seatbelts)

head(seatbelts)

seatbelts <- mutate(seatbelts,
                    ppnorm =(PetrolPrice - mean(PetrolPrice))/sd(PetrolPrice),
                    mm =  kms/1000,
                    mmnorm = mm - mean(mm) )

fit<- lm(DriversKilled ~ mmnorm + ppnorm, data = seatbelts)
## since we scaled the decimals are good.
round(summary(fit)$coef,4)
## intercept 122.80 number of deaths for avg kms and avg petrol price
## or
### intercept is expected nbr of drivers killed for zero Kilometers and petroprice = 0

#Q2
## interpret on the log scale, relative interpretation
## pp = -0.06441 

fit2<- lm(I(log(DriversKilled)) ~ mmnorm + ppnorm, data = seatbelts)
round(summary(fit2)$coef, 4)

## approx 6 % decrease in deaths for every 1 std dev increase in normal petro price, holding kms constant.
1 - exp(fit2$coef[3])

## 13 % decrease in deaths geometric mean for every 1 std dev increase in kms holding PP constant.
1 - exp(fit2$coef[2])

#Q3 - add random variable
seatbelts <- as.data.frame(Seatbelts)

seatbelts <- mutate(seatbelts,
                    law = cbind(c(rep(0, times = 96) , c(rep(1, times = 96) ) )),
                    ppnorm =(PetrolPrice - mean(PetrolPrice))/sd(PetrolPrice),
                    mm =  kms/1000,
                    mmnorm = mm - mean(mm) )

fit3<- lm(DriversKilled ~ mmnorm + ppnorm + law, data = seatbelts)
## Add dummy 0/1variable changes intercept from 127 to 122 so we excpect  approx 12 fewer deaths ( intercep on 
## law is -)

round(summary(fit3)$coef,4)
round(summary(fit)$coef,4)


#Q3 a  
## video https://www.youtube.com/watch?v=ikKQv98i-EQ&list=PLpl-gQkQivXji7JK1OP1qS7zalwUBPrX0&index=41
## Add dummy 0/1variable - intercept law = -11.888 so expect apprx 12 few deaths occur when law went from 0 to 1
## holding kms and pp constant

seatbelts <- as.data.frame(Seatbelts)

n <- length(seatbelts$DriversKilled)

seatbelts <- mutate(seatbelts,
                    law <-  rep(c(0, 1), c(n/2, n/2)),
                    ppnorm =(PetrolPrice - mean(PetrolPrice))/sd(PetrolPrice),
                    mm =  kms/1000,
                    mmnorm = mm - mean(mm) )
fit3a<- lm(DriversKilled ~ mmnorm + ppnorm + law, data = seatbelts)
round(summary(fit3a)$coef,4)


## Q3b fit it as a factor - same as 3a

fit3b<- lm(DriversKilled ~ mmnorm + ppnorm + I(factor(law)), data = seatbelts)
round(summary(fit3b)$coef,4)


#Q4 Discretize the PetrolPrice variable into four factor levels
summary(seatbelts$ppnorm)
hist(seatbelts$ppnorm)







