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




#For animating the ggplot for a span of time period
View(gapminder)
library(ggplot2)
library(gganimate)

ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')



