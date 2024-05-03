library(tidyverse)
library(ggplot2)
library(dplyr)
data<-read.csv("../data/561000.csv")

#1.Histograma
names(data)=c("1","2","3","Pavadinimas","5","6","7","menuo","VidAtlyginimas","ApdraustiejiSk","11","12","13")
g1=data%>%
  ggplot(aes(x=VidAtlyginimas))+
  geom_histogram(bins = 200,fill='green')+
  labs(title = 'Vidutinis atlyginimas',x='Atlyginimas',y='Kiekis')
ggsave('../img/1grafikas.png',g1)

#2. Penkios įmonės, kurių faktinis sumokėtas darbo užmokestis per metus buvo didžiausias

top5imones= data%>%
  group_by(Pavadinimas)%>%
  summarise(top=max(VidAtlyginimas))%>%
  arrange(desc(top))%>%
  head(5)
g2 <- data %>%
  filter(Pavadinimas%in% top5imones$Pavadinimas) %>%
  mutate(Menuo=ym(menuo))%>%
  ggplot(aes(x = Menuo, y = VidAtlyginimas, color = Pavadinimas)) +
  geom_line()
ggsave('../img/2grafikas.png',g2)

#3. Išrinkti maksimalų apdraustų darbuotojų skaičių iš top 5 įmonių

apdraustieji <- data %>%
  filter(Pavadinimas %in% top5imones$Pavadinimas) %>%
  group_by(Pavadinimas) %>%
  summarise(maxapdraust=max(ApdraustiejiSk))%>%
  arrange(desc(maxapdraust))

apdraustieji$Pavadinimas=factor(apdraustieji$Pavadinimas,levels=apdraustieji$Pavadinimas[order(apdraustieji$maxapdraust)])


g3 <- apdraustieji%>%
  ggplot(aes(x=Pavadinimas,y=maxapdraust,fill=Pavadinimas))+
  geom_col()+labs(title="Max apdraustųjų skaičius",x="Įmonių pavadinimai",y="Apdraustų skaičius")
ggsave('../img/3grafikas.png',g3)