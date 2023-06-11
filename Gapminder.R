install.packages("tidyverse")
install.packages("ggplot2")
install.packages("gapminder")

library(tidyverse)
library(ggplot2)
library(gapminder)

gapminder %>% 
  filter(continent %in% c("Asia", "Europe")) %>% 
  filter(gdpPercap < 30000) %>% 
  ggplot(aes (x = gdpPercap, y = lifeExp, size = pop, color = year))+
  geom_point()+
  facet_wrap(~continent)+
  labs(title = "Life expectancy explained by GDP",
       x = "GDP per capita", y = "Life expectancy")


