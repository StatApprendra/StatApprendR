#https://www.youtube.com/watch?v=303Pc0L3Lx0&ab_channel=SizeS

# packages ----------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tibble)

# data --------------------------------------------------------------------
data=read_csv2('data2.csv')
data
# plot --------------------------------------------------------------------
p=ggplot(data=data,mapping=aes(x="",y=taille_cm,label=personnage))+
  geom_boxplot(fill="orange")+theme_classic()+xlab("")+ylab("taille (cm)")+geom_text(hjust=0, vjust=1)
p
ggsave("boxplot7.png",w=4,h=6)

p=ggplot(data=data,mapping=aes(x="",y=taille_cm,label=personnage))+
  geom_boxplot(fill="orange")+theme_classic()+xlab("")+ylab("taille (cm)")+
  ylim(0,500)+
  geom_text(hjust=0, vjust=1, angle=45)+
  coord_flip()
p
ggsave("boxplot72.png",w=6,h=4)

# indicators --------------------------------------------------------------
median(data$taille_cm)
summary(data$taille_cm)
data %>% arrange(taille_cm)

# removing slender man ----------------------------------------------------
data2=data %>% filter(personnage!='slenderman')
summary(data2$taille_cm)
