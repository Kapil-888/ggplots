#t-test (Single sample, One sided, Two Sided, Paired)

#Assumptions of t-test
#1. Large, representative sample
#2. Values or normally distributed 
#3. Two samples have similar variance 

#Values have to be pre-decided for hypothesis. After checking the p value and deciding the confidence intervals to produce statistically signifiant results is knowing as p hacking (bad science)

#For checking variance 
var(gapminder$lifeExp[gapminder$country == "India"])
var(gapminder$lifeExp[gapminder$country == "Kuwait"])
#we can do levine test to check if the difference in variance is statistically significant. 

install.packages("patchwork")
library(patchwork)    #Used for viewing more than one graph in single plot
library(gapminder)

View(gapminder)

#In t-test we are checking if the mean of a subset same or different from mean of another subset



---------------------------------------------------------
#Single sample t-test

#Hypothesis Testing
#: HO: the mean life expectancy is 50 years (Null hypothesis)
#: H1: the mean life expectancy is not 50 years (Alternative hypothesis)
#we have to find confidence intervals to check statistical significance
#To check we assume that the above mentioned Null Hypothesis is correct i.e lifeExp is 50 yrs
#What is the probablity that the difference is different than 50 years?

#Observations - Sample data provides a mean life expectancy of 48.9. Is this statistically significant? 
#Usually we take the threshold of 5%, i.e value of alpha (p) = 0.05
#If it's improbable (greater than 0.05) to get the sample has value of mean 50 then we can reject the null hypothesis (HO) and accept the alternative hypothesis H1. 

#Single sample t-test (for assumed mean of 50)
gapminder %>% 
  filter(continent == "Africa") %>% 
  select(lifeExp) %>% 
  t.test(mu = 50)       #Here mu which is pop mean set 50 in the argument

#Results in console shows 95% confidence intervals (CI) for sample mean being between 48.1 - 49.5. Results also shows the actual mean which is 48.86
#Over here we can reject the Null Hypothesis and accept the alternative hypothesis, i.e - mean is not = 50

#You can create the value for same t-test to be stored in global environment
my_ttest <- gapminder %>% 
  filter(continent == "Africa") %>% 
  select(lifeExp) %>% 
  t.test(mu = 50)

#after this you can add attributes(my_ttest) in the console below the argument to see the names and class
#For checking particular value from within the names
my_ttest$p.value



-----------------------------------------------------
#Two-sided t-test for difference of means 

#Over here we are asking the question is Africa's lifeExp (mean) is different from that of Europe's. Assuming we don't know mean could be either more or less
#Is the difference statistically significant from 0 (being same)

gapminder %>% 
  filter(continent %in% c("Africa", "Europe")) %>% 
  t.test(lifeExp ~ continent, data = ., alternative = "two.sided")

#p values is really really small indicating very high difference among the mean of both groups
#We can reject the null hypothesis and accept the alternative hypothesis which is displayed on console. 



--------------------------------------------------------
#One-sided t-test for difference of means

#Null Hypo - Over here we're asking if true differ b/t means of India and UK group more than 0

gapminder %>% 
  filter(country %in% c("India", "United Kingdom")) %>% 
  t.test(lifeExp ~ country, data = ., alternative = "less", conf.level = 0.95)

#Result are statistically significant, 95% CI is between - 23.9 to - 22.1
#Hence we can reject the null hypothesis and accept alternative hypo
#which is that true differ b/t means of India and UK group is less than 0



-------------------------------------------------------------
#Paired t-test

#We are comparing the same variable in two different set of time period, hence each variable will have it's counterpart
#Here we have used mutate to change the data type and levels (for 2007 minus 1957 and not the other way around)
#Null hypothesis is that it remains constant over period of time (Hence 0)

gapminder %>% 
  filter(year %in% c(1957, 2007) &
           continent == "Asia") %>% 
  mutate(year = factor(year, levels = c(2007, 1957))) %>% 
  t.test(lifeExp ~ year, data = ., paired = TRUE)

#Mean difference in lifeExp of Asia from 2007 to 1957 is 21.4
#Here we can reject the null hypo and accept the alternative hypo that true mean differ is not equal to 0.



