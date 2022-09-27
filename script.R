# data --------------------------------------------------------------------
(data=as_tibble(read.csv2("data.csv")))
data$brand=as.factor(data$brand)
view(data)





# packages ----------------------------------------------------------------
library(tidyverse)
library(ggplot2)


library(officer)
source("functions.R")








# automatized script ------------------------------------------------------
my_doc<-read_docx()

for(i in c(2:length(levels(data$brand)))){# a loop per brand
  
  sub=data %>% filter(brand==levels(data$brand)[i])# sub table per brand
  
  add.title(my_doc, paste0(levels(data$brand)[i]))# add title with the brand name
  
  my_doc <- body_add_par(my_doc,
                         paste0("Le prix moyen d'un sous-vêtement de la marque ",levels(data$brand)[i],
                                " est de ", round(mean(sub$price,na.rm=TRUE))," dollars."),
                         style="Normal")
  
  add.page.break(my_doc)
}

print(my_doc,target="document.docx")

# automatized script 2 ------------------------------------------------------
my_doc<-read_docx()

for(i in c(2:length(levels(data$brand)))){# a loop per brand
  
  sub=data %>% filter(brand==levels(data$brand)[i])# sub table per brand
  
  add.title(my_doc, paste0(levels(data$brand)[i]))# add title with the brand name
  
  my_doc <- body_add_par(my_doc,
                         paste0("Le prix moyen d'un sous-vêtement de la marque ",levels(data$brand)[i],
                                " est de ", round(mean(sub$price,na.rm=TRUE))," ± ",round(sd(sub$price,na.rm=TRUE))," dollars."),
                         style="Normal")
  my_doc <- body_add_par(my_doc,
                         paste0("Le prix minimal est de ",
                                round(min(sub$price,na.rm=TRUE))," dollars."),
                         style="Normal")
  my_doc <- body_add_par(my_doc,
                         paste0("Le prix maximal est de ",
                                round(max(sub$price,na.rm=TRUE))," dollars."),
                         style="Normal")
  
  add.page.break(my_doc)
}

print(my_doc,target="documentv2.docx")

# automatisation 3 --------------------------------------------------------
my_doc<-read_docx()

for(i in c(2:length(levels(data$brand)))){
  
  sub=data %>% filter(brand==levels(data$brand)[i])# sub table per brand
  
  add.title(my_doc, paste0(levels(data$brand)[i]))# add title with the brand name
  
  my_doc <- body_add_par(my_doc,
                         paste0("Le prix moyen d'un sous-vêtement de la marque ",levels(data$brand)[i],
                                " est de ", round(mean(sub$price,na.rm=TRUE))," ± ",round(sd(sub$price,na.rm=TRUE))," dollars."),
                         style="Normal")
  my_doc <- body_add_par(my_doc,
                         paste0("Le prix minimal est de ",
                                round(min(sub$price,na.rm=TRUE))," dollars."),
                         style="Normal")
  my_doc <- body_add_par(my_doc,
                         paste0("Le prix maximal est de ",
                                round(max(sub$price,na.rm=TRUE))," dollars."),
                         style="Normal")
  
  p<-ggplot(sub,aes(y=price))+
    geom_boxplot()+ylab("Prix")+theme_classic()
  p
  ggsave("plot.png",h=4,w=4.8)
  
  add.image(my_doc,"plot.png",h=4,w=4.8)
  
  add.page.break(my_doc)
}

print(my_doc,target="documentv3.docx")
