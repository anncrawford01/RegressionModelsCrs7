## homework reading Week 3.1 chapter 8 Multivariable regression analysis
# 1
libary(Datasets)
library(dplyr)
data("Seatbelts")
seatbelts <- as.data.frame(Seatbelts)

head(seatbelts)

fit<- lm(DriversKilled ~ kms + PetrolPrice, data = seatbeltls)
round(summary(fit)$coef, 4)

### intercept is expected nbr of drivers killed for zero Kilometers and petroprice = 0

## check units on perdictors
summary(seatbelts$kms)
summary(seatbelts$PetrolPrice)

## normalize the petro price and scale and  kms ( kms has units so don't need to normaliz)

seatbelts <- mutate(seatbeltls,
                     ppnorm =(PetrolPrice - mean(PetrolPrice))/sd(PetrolPrice),
                    mm = kms/1000,
                    kmc= (mm - mean(mm))
                     )
##head(seatbeltls)

fit1<- lm(DriversKilled ~ kmc + ppnorm, data = seatbelts)
## since we scaled the decimals are good.
summary(fit1)$coef
## now intercept 123 is the estimated killed for avg kms and avg petrol price
## ppnorm coeff of -7.8 means expect 7.8 less deaths for 1 std deviation change in petro price
## kmc cief if -1,74 is expected death decrease for each additional 1000 km driven

##Q2
seatbelts <- mutate(seatbelts,
                     ppnorm =(PetrolPrice - mean(PetrolPrice))/sd(PetrolPrice),
                     kmnorm= (kms - mean(kms))/sd(kms)
)
fit2<- lm(DriversKilled ~ kmnorm + ppnorm, data = seatbelts)
## since we scaled the decimals are good.
round(summary(fit2)$coef,4)
## inercept 122.80 number of deaths for avg kms and avg pp
## or
fit2b <- lm(DriversKilled ~ kms + PetrolPrice, data=seatbelts)
predict(fit2b, newdata = data.frame(kms = mean(seatbelts$kms),
                                    PetrolPrice = mean(seatbelts$PetrolPrice)))

##Q3

fitfull<- lm(DriversKilled ~ kms + PetrolPrice, data = seatbeltls)
##round(summary(fitfull)$coef, 4)

## these both include an intercept

edk <- resid(lm(DriversKilled ~  kms , data = seatbelts))
epp <- resid(lm(PetrolPrice ~ kms  , data = seatbelts))

fit3 <-(lm(edk ~ epp -1))  ## regression thru the origin

round(summary(fitfull)$coef, 4)
round(summary(fit3)$coef,4)

# adjustment
# remove the linear association of kms on everything. i.e. take the residuals
## so if we want to match coeff kms, then regress out petrol price from both kms and Drivers killed







