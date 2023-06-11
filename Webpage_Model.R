#Designing model from webpage

#Data Scraping
install.packages("rvest")
installed.packages("rvest")
library("rvest")
webpage <- read_html("https://www.worldometers.info/world-population/") tbls <- html_nodes(webpage, "table")
head(tbls)

# subset list of table nodes for tables 1 and 2 in website (out of 5 tables)
table1 <- webpage %>% html_nodes("table") %>% .[1:2] %>%
  html_table(fill = TRUE)


uk <- read.csv("Population.csv")
View(uk)
library(ggplot2)

ggplot(uk, aes(x = World.Population, y = Country.s.Share.of.World.Pop, fill = Fertility.Rate)) +
  geom_point (aes(col = Fertility.Rate))+ geom_smooth() + 
  theme_bw()+ xlab("World Population in million")+ ylab("UK Share to World Population in %")+
  ggtitle("UK Population vs World Population from 1955 - 2019")+
  theme(legend.position = "bottom") + 
  stat_smooth(
    method = "lm",
    color = "cyan",
    se = FALSE
  ) +
  theme(axis.line = element_line(colour = "darkblue", 
                                 size = 1, linetype = "solid"))+ 
  theme(axis.text.x= element_text(face="bold", color="#00008B", 
                                  size=10, angle=0))+
  theme(axis.text.y = element_text(face="bold", color="#00008B", 
                                   size=10, angle=0))



  