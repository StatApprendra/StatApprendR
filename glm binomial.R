# https://www.kaggle.com/c/titanic/data

# packages and functions --------------------------------------------------
{
  library(rpart)
library(tidyverse)

library(rpart.plot)
library(RColorBrewer)
library(rattle)

library(lme4)
library(skimr)
library(PerformanceAnalytics)
library(lattice)
library(car)

source("HighstatLibV13.R")
    
}



# data --------------------------------------------------------------------
titanic<-read_csv('titanic.csv')
# skim(titanic)
titanic


# data exploration --------------------------------------------------------
#do we have enough data in each factors
table(titanic$Survived,titanic$Pclass)

table(titanic$Sex,titanic$Pclass)
table(titanic$Sex,titanic$Embarked)
table(titanic$Pclass,titanic$Embarked)


# quali_var_title<-c("Pclass","Embarked")
# Mybwplot(titanic,quali_var_title,"Sex")

#collinearity among factors
quanti_var<-titanic %>% select(Age,SibSp,Parch,Fare)
chart.Correlation(quanti_var, histogram=TRUE, pch=19)

# Colinéarité 
boxplot(Fare~as.factor(Pclass),data=titanic)
boxplot() %>% rename()
# Parch = nombre de parents et d’enfants à bord du Titanic
# SibSp = nombre de frères, soeurs, époux, épouses
# Fare = tarif du passager
# Embarked = port d’embarquement 
    # C = Cherbourg, Q = Queenstown, S = Southampton 




# logit -------------------------------------------------------------------
p<-ggplot(titanic,aes(x=Age,y=Survived))+geom_point()+theme_classic()+
  stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial),
              col="blue")
p

T11<-titanic %>% filter(Survived==0 & Age>=20)
T12<-titanic %>% filter(Survived==1 & Age<=30)
TAB1=rbind(T11,T12)

COLOR=rgb(red=0.0116,green=0.4709,blue=0.5174)
p<-ggplot(TAB1,aes(x=Age,y=Survived))+geom_point()+theme_classic()+
  stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial),
              col=COLOR)+  theme_classic()+
  theme(axis.title.y=element_text(size=20))+
  theme(axis.title.x=element_text(size=20))+
  ylab("Survie")+xlab("Age")
p
ggsave("glmtrend.png",w=4,h=4.5)

p<-ggplot(TAB1,aes(x=Age,y=Survived))+geom_point()+theme_classic()+
  stat_smooth(method="lm", se=TRUE, method.args = list(family=binomial),
              col=COLOR)+  theme_classic()+
  theme(axis.title.y=element_text(size=20))+
  theme(axis.title.x=element_text(size=20))+
ylab("Survie")+xlab("Age")
p
ggsave("lmtrend.png",w=4,h=4.5)




# 
# 
# p<-ggplot(titanic,aes(x=Age,y=Survived))+geom_point()+theme_classic()+
#   stat_smooth(method="lm", se=TRUE, method.args = list(family=binomial),
#               col="blue")
# p
# fit = glm(Survived ~ Age, data=titanic, family=binomial)
# newdat <- data.frame(Age=seq(min(titanic$Age,na.rm=TRUE), max(titanic$Age,na.rm=TRUE),len=100))
# newdat$Survived = predict(fit, newdata=newdat, type="response")
# plot(vs~hp, data=mtcars, col="red4")
# lines(vs ~ hp, newdat, col="green4", lwd=2)

# #si on triche
# younger<-titanic %>% filter(Age<=30 & Survived==1)
# older<- titanic %>% filter(Age>=30 & Survived==0)
# TAB=rbind(younger,older)
#         p<-ggplot(TAB,aes(x=Age,y=Survived))+geom_point()+theme_classic()+
#           stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial),
#                       col="blue")
#         p
        
        
#que pauvres
# TAB2<-titanic %>% filter(Pclass!=1, Sex=="male")        
#         
# p<-ggplot(TAB2,aes(x=Age,y=Survived))+geom_point()+theme_classic()+
#   stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial),
#               col="blue")
# p
# 
# T31<-TAB2 %>% filter(Survived==0 & Age>=25)
# T32<-TAB2 %>% filter(Survived==1 & Age<=35)
# TAB3=rbind(T31,T32)
# 
# p<-ggplot(TAB3,aes(x=Age,y=Survived))+geom_point()+theme_classic()+
#   stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial),
#               col="blue")
# p



# model -------------------------------------------------------------------
mod <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
           data=titanic,
           family="binomial")

summary(mod)
summary(titanic)
vif(mod)
exp(mod$coefficients)
mod$coefficients


mod <- glm(Survived ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
           data=titanic,
           family="binomial")

summary(titanic)


mod <- glm(Survived ~ Pclass+Sex+ SibSp+ Parch+Fare,
           data=titanic,
           family="binomial")

library(pROC)
library(broom)
titanic$prob <- predict(mod, type = c('response'))
plot.roc(Survived ~ prob, data = titanic, ci = TRUE, print.auc = TRUE)


# model validation --------------------------------------------------------
E1<-residuals(mod,type="pearson")
F1<-fitted(mod)

plot(E1~F1,xlab="fitted values",ylab="pearson residuals")


plot(E1~titanic$Age,xlab="fitted values",ylab="pearson residuals")



# plots -------------------------------------------------------------------
MyData<-data.frame(Age=20, Pclass=3, Sex="male", SibSp=0, Parch=0, Fare=0, Embarked="S")
Pred<-predict(mod, newdata=MyData, type="response")
conclusion<-paste0("Jack n’avait de toute façon que ",round(Pred*100,1),"% de chances de survie. Et ce, malgré cette fichue planche.")
cat(conclusion)

plot(x=titanic$Age ,y=titanic$Survived)
lines(MyData$Age, Pred)
