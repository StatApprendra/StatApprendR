library(tidyverse)
library(ggplot2)

Y<-c(10,11,12,13,13,12,14,15,10,10,11,12,19,18)
TAB<-tibble(Y)


ggplot(TAB, aes(y=Y)) + 
  geom_boxplot(fill="darksalmon")+
  labs(title="All you need are outliers....",x="", y = "")+
  theme_classic()

ggsave("boxplots.png",height=5,width=5)


Y2<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,6,7,8,10,11,12,13,13,12,14,15,10,10,11,12,19,18)
TAB2<-tibble(Y2)

ggplot(TAB2) + 
  geom_histogram(aes(x=Y2),fill="darksalmon")+
  labs(title="0 is all you need...",x="", y = "")+
  theme_classic()


ggsave("zeros.png",height=5,width=5)

# logarithm ---------------------------------------------------------------
Y3<-log(seq(1,100,by=0.1))
X3<-seq(1,100,by=0.1)

TAB3<-tibble(Y3,X3)

ggplot(TAB3,aes(y=Y3,x=X3)) + 
  geom_point(colour="darksalmon")+
  labs(title="How deep is your log...",x="", y = "")+
  theme_classic()
ggsave("log.png",height=5,width=5)



# residuals ---------------------------------------------------------------

mod<-lm(Y3~X3,data=TAB3)
summary(mod)
residuals(mod)
fitted(mod)

plot(mod)

TAB4<-tibble(residuals(mod),fitted(mod))
ggplot(TAB4,aes(y=residuals(mod),x=fitted(mod))) + 
  geom_point(colour="darksalmon")+
  labs(title="Crazy / outliers drive me crazy",x="residuals", y = "fitted values")+
  theme_classic()

ggsave("residuals.png",height=5,width=5)



# normality ---------------------------------------------------------------
X4 <- seq(-100, 100, length=1000)
Y4 <- dnorm(X4)
Y4<-rnorm(10000)

TAB4<-tibble(Y4,X4)

ggplot(TAB4) + 
  geom_histogram(aes(x=Y4),fill="darksalmon")+
  labs(title="stop in the name of normality..",x="", y = "")+
  theme_classic()

hist(Y4,ylab="",col="darksalmon",main="Take my normality away...")


ggsave("zeros.png",height=5,width=5)