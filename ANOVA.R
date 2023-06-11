#ANOVA (Analysis of Variance)

#Here we take 3 or more variables but the underlying principle dosen't change
#Null Hypothesis is same, that there's no difference in lifeExp of continents 
#If we can show that, that isn't true then can we can accept the alternative hypothesis (that there is differ)

#If there we no difference, how likely (probablity) would it be 
#that we can have SAMPLE having means like the one we are evaluating
#and above is very improbable then we reject the null hypo and accept the alternative hypo.
#Pre-decide alpha value, deciding retrospectively is p hacking (bad science)

library(tidyverse)
library(patchwork)
library(gapminder)
library(forcats)

#Only taking data of continents (Americas, Europe, Asia) for lifeExp in 2007
gapdata <- gapminder %>% 
  filter(year == 2007,
         continent %in% c("Americas", "Europe", "Asia")) %>% 
  select(continent, lifeExp)

#Looking at distribution of means of continents (Americas, Europe, Asia)
gapdata %>% 
  group_by(continent) %>% 
  summarise(Mean_life = mean(lifeExp)) %>%   #we created new col naming it as Mean_life
  arrange(Mean_life)

#Question we are asking is the difference real or is it by chance that it can include such a difference 
gapdata %>% 
  aov(lifeExp ~ continent, data = .) %>% 
  summary()  
#Checking p value is very small 10 minus 5, so we can reject the null hypo and accept alternative hypo

aov_model <- gapdata %>% 
  aov(lifeExp ~ continent, data = .)

#Now if we are taking multiple variables (like countries) 
#then to find out if the result is statistcally significant between two countries 

gapdata %>% 
  aov(lifeExp ~ continent, data = .) %>% 
  TukeyHSD()        #HSD is equal to honestly significant difference
#This tells the p values of each group by which you can check statistical significance of each group

#For drawing plot of same
gapdata %>% 
  aov(lifeExp ~ continent, data = .) %>% 
  TukeyHSD() %>%          
  plot()
