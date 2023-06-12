#airquality project

installed.packages("tidyverse")
require(tidyverse)
library(ggplot2)
data()

View(airquality)
str(airquality)
airquality$Wind <- as.integer(airquality$Wind
                              levels = c("slow", "fast"))

levels(airquality$Solar.R)

sw <- airquality %>%
  select(Ozone, Temp, Wind, Solar.R) %>%
  mutate(Solar.R = Solar.R / 100) %>%
  na.omit() %>%
  filter(Wind %in% c("8", "9", "10")) %>%
  mutate(Danger = Ozone > 30, Temp > 75, 
         Danger = if_else(Danger == TRUE, "red", "Green"))



p <- ggplot(airquality, aes(x = Solar.R, y = Ozone, fill = Temp)) +
  geom_point (aes(col = Temp))+ geom_smooth() + 
  theme_bw()+ xlab("Solar.R - kWh/m2")+ ylab("Ozone - ppb")+ 
  ggtitle("ANOVA [LOESS]")+
  theme(legend.position = "bottom") + 
  stat_smooth(
    method = "lm",
    color = "cyan",
    se = FALSE
  ) +
  theme( axis.line = element_line(colour = "darkblue", 
                                  size = 1, linetype = "solid"))+ 
  theme(axis.text.x= element_text(face="bold", color="#000099", 
                                  size=10, angle=0))+
  theme(axis.text.y = element_text(face="bold", color="#000099", 
                                   size=10, angle=0))


#Renaming the column name
p + labs(colnames("Temp in Celciums")) 

