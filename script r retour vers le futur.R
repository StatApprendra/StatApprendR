library(tidyverse)
library(lubridate)
library(bioRad)


# TAB_M -------------------------------------------------------------------

TAB_M<-read_csv2("TAB_M.csv")
TAB_M


p <- ggplot(TAB_M, aes(x=date_depart, y=nb_passagers)) +
  geom_point(color="firebrick",size =4) +
  xlab("")+
  ylab("nombre de passagers")+
  theme(axis.text.y = element_text(face = NULL, color = "black" ,size = 10,margin = unit(c(0, 1, 0, 0),"mm"))) +
  theme(axis.text.x = element_text(face = NULL, color = "black" ,size = 10,angle=45,margin = unit(c(7, 0, 0, 0),"mm"))) +
  theme(axis.title.y = element_text(face = NULL, color = "black" ,size = 15,margin = unit(c(0, 2, 0, 0),"mm")))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
p

ggsave("bad_graph.png")


TAB_M$date_depart<-dmy(TAB_M$date_depart)
TAB_M$date_arrivee<-dmy(TAB_M$date_arrivee)
TAB_M

#ajouter des dates et des 0.
p <- ggplot(TAB_M, aes(x=date_depart, y=nb_passagers)) +
  geom_point(color="dodgerblue4",size =4) +
  ylab("nombre de passagers")+
  scale_x_date(date_labels = "%d/%m/%Y",date_breaks = "15 year")+xlab("")+
  theme(axis.text.y = element_text(face = NULL, color = "black" ,size = 10,margin = unit(c(0, 1, 0, 0),"mm"))) +
  theme(axis.text.x = element_text(face = NULL, color = "black" ,size = 10,angle=45,margin = unit(c(2, 0, 0, 0),"mm"),hjust = 1)) +
  theme(axis.title.y = element_text(face = NULL, color = "black" ,size = 15,margin = unit(c(0, 2, 0, 0),"mm")))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
p

ggsave("good_graph.png")


# bind with TAB_D ---------------------------------------------------------

TAB_D<-read_csv2("TAB_D.csv")
TAB_D

TAB_D$date_depart<-mdy(TAB_D$date_depart)
TAB_D$date_arrivee<-mdy(TAB_D$date_arrivee)
TAB_D

TAB<-rbind(TAB_M,TAB_D)


# composant date ----------------------------------------------------------
month(TAB$date_depart)
year(TAB$date_depart)

labels <- c("dimanche", "lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi")


wday(TAB$date_depart,label=TRUE)
wday(TAB$date_depart,label= TRUE, abbr = FALSE)#pareil sauf un jeudi
wday(TAB$date_arrivee,label=TRUE,abbr=FALSE)

#a part 2 lundi et un jeudi 


# Temps qui s'écoule ------------------------------------------------------
dmy("07/01/2021")-dmy("09/12/1991") 


# Comment définir les zones de temps --------------------------------------


Sys.timezone()
heure_marty<-dmy_h("07/01/2021 19",tz="Europe/Paris")
heure_marty

#https://en.wikipedia.org/wiki/List_of_tz_database_time_zones

heure_doc <- with_tz(heure_marty,tz="us/pacific")
heure_doc
#Hill valley est en californie

# Calcul complexe ---------------------------------------------------------
lat = 33.8766
lon = -118.2987

#par défaut, se met à l'heure du système, donc ici Europe/Paris => CET
# Sys.setenv(TZ="Europe/Paris")
sunset(dmy("08/01/2021"),lon,lat)

#pour changer ca il faut changer le système de temps, se mettre à l'heure de doc
#tu peux utiliser Sys.setenv

Sys.setenv(TZ="us/pacific")
sunset(dmy("08/01/2021"),lon,lat)


# NOW <- Sys.time()
# NOW


