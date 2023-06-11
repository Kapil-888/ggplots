#Chi Square Test : 

Two types 
#1. goodness of fit
#2. test of independence

#The question is, are the propotion of data in each category available same? 
#Even though the data you collected could be showing differences but that could just be a coincidence?
#To check if there's truely a difference we have to do hypothesis testing. 

#Over here null hypothesis is that small, medium, large flowers are present in same propotions 
#If Null hypothesis is true, then what is the probablity that we could have the sample that is like one we have
#If that probablity is extremely small, then we can reject null hypothesis and accept alternative hypo
#and with we can tell that there's stat signficance and our data is representative of larger population of flowers

#In simple words we are checking the probablity of a sample which can have this much or greater variation in distribution of means or variables

#For test of independence the only difference is that we check multiple variables and their interdependency

View(iris)
names(iris)
unique(iris)

#Creating variable size with breaking into 3 equal parts and naming them
#by this way we can convert a numerical variable into categorical variable
#Though we are losing info by converting continous variable into a discrete one 
#It can help in making interactive data visualisations like making boxplot where only scatterplots can be made

flowers <- iris %>% 
  mutate(Size = cut(Sepal.Length, breaks = 3, labels = c("Small", "Medium", "Large"))) %>% 
  select(Species, Size)

#This will arrange the table for two parameters select above in console
#How the different species are distributed according to the size
table(flowers)
#For checking the total flowers in each size 
table(flowers$Size)

#Chi-square test without tidyverse
chisq.test(table(flowers$Size)

#Goodness of fit test: just looking for size (with tidyverse)
           
flowers %>% 
  select(Size) %>% 
  table () %>% 
  chisq.test()

#p-value - 6.673e-07 = 0.0000006673 (very small)
#It's very unlikely that if the propotions were equal, then we would get a sample with such or greater difference
#So we can reject null and accept alternative hypo that true propotions are indeed not equal

#Chi-Square test of independence

#Null hypo (H0) is that variables are independent of each other (Species, & Size)
#That knowing the value of one variable dosen't help at all in predicting the value of another
#Alternative Hypo (H1) is that variables are interdependent on each other and
#Knowing one variable can help in prediction of propotion of another variable 

table(flowers)
#Chi square test without tidyverse
chisq.test(table(flowers))

#with tidyverse

flowers %>% 
  table() %>% 
  chisq.test()

#p-value is very small.
#Again we can reject the H0 and accept H1. 

flowers %>% 
  table() %>% 
  chisq.test() %>% 
  .$expected

#For checking the expected values
#Since none of the values are less than 5, we don't need to us the fisher exact test (For checking contingencies)
#Or we can do it like this
chi_test <- flowers %>% 
  table() %>% 
  chisq.test()

attributes(chi_test)
chi_test$expected

           