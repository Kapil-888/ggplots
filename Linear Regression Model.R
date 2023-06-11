#Linear Regression Models for predictive analytics 

#Null Hypo (H0) is that slope is not positive, it is 0 (no slope)
#Alternative Hypo (H1) is that graph has a positive  or negative slope
#Here x axis is independent variable, whereas y axis is dependent variable
#Hence we are suggesting that when variable in x axis changes, so will the variable in y axis

#There is not a perfect relationship, have to check before implementation (remember ascombe plots)
#but in this way we can predict appox relation of y axis variable in relation to the x axis variable

View(airquality)
names(airquality)

#This is a simgle linear model (using tidyverse)
airquality %>% 
  lm(Solar.R ~ Ozone, data = .) %>% 
  summary()
#For the case of having a linear model the residuals should be symmetrically distributed
#In this case they are asmmetrically in distribution
#It also tells y intercept = 144.63, and slope = 0.954
#slope will tell y axis variable as a function of x axis variable
#Lastly R2 value = 0.1213 meaning approx 12% value of y axis variable can be explained by x axis variable
#Hence we can reject H0 and accept H1 (because of slope)
#But the model is not good enough (Less confidence interval)

#For multivariate linear model we have to look for adjusted R square value 
#For multiple variable F statistics tell about the p value accurately and overall distribution 

#without tidyverse
lm.uk <- lm(Country.s.Share.of.World.Pop ~ World.Population, data = uk)
summary(lm.uk)
attributes(lm.uk)                     #For checking what all things are present
hist(lm.uk$residuals)           #For checking distribution of values

#Design worthy Model
mod <- cars %>% 
  lm(dist ~ speed, data = .) %>% 
  summary()
#R square value = 0.65 meaning 65% relation of y axis variable can be explained by x axis variable 

summary(mod)
summary(new_speeds)
#With a model we can use the model for predictive analytics 
new_speeds <- data.frame(speed = c(10, 15, 20))
View(new_speeds)
predict(mod, new_speeds) %>% round(1)     #Rounding off to one decimal

or 

cars %>% 
  lm(dist ~ speed, data = .) %>%
  predict(data.frame(speed = c(10, 15, 20))) %>% 
  round()
