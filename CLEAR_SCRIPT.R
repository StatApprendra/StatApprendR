####data management####
got2<-read.csv2("data.csv")
got2

got3<-got2[,-1]
got3
View(got3)

#consider character names as rownames
got<-got3[,-1]
rownames(got)<-got3[,1]
got

#View(got)
#took allegence house for illustrative variable
got$Allegence<-as.factor(got$Allegence)
str(got)

#scaling the quantitative variables
got.PCA<-scale(got[, c("nb_chp_narres", 
                        "genre", "marie",
                        "age", "nb_morts_ds_relations", "popularite")])

#and add the allegence houses
got.PCA<-cbind(got.PCA,as.data.frame(got$Allegence))
got.PCA

#View(got.PCA)
nrow(got.PCA)
ncol(got.PCA)


####Rcmdr####
####package loading####
library(Rcmdr)
library(FactoMineR)
library(RcmdrPlugin.FactoMineR)


#####Rcmdr outputs####
##########################


#data management
names(got.PCA) <- make.names(names(got.PCA))
got.PCA.PCA<-got.PCA[, c("nb_chp_narres", "genre", "marie", "age", 
                         "nb_morts_ds_relations", "popularite", "got.Allegence")]
summary(res, nb.dec = 3, nbelements=10, nbind = 10, ncp = 3, file="")

#PCA 
res<-PCA(got.PCA.PCA , scale.unit=TRUE, ncp=5, quali.sup=c(7: 7), graph = 
           FALSE)

#clutering and plots from HCPC
res.hcpc<-HCPC(res ,nb.clust=-1,consol=FALSE,min=3,max=10,graph=TRUE)

plot(res.hcpc)
plot(res.hcpc,choice="tree")

#PCA plots
#individual cloud
plot.PCA(res, axes=c(1, 2), choix="ind", habillage="none", col.ind="black", 
         col.ind.sup="blue", col.quali="magenta", label=c("ind", "ind.sup", "quali"),
         new.plot=TRUE)

#with the illustrative variabile
plot.PCA(res, axes=c(3, 4), choix="ind", habillage="none", col.ind="black", 
         col.ind.sup="blue", col.quali="magenta", label=c("ind", "ind.sup", "quali"),
         new.plot=TRUE,invisible = "quali")

  #other dimensions
plot.PCA(res, axes=c(3, 4), choix="ind", habillage="none", col.ind="black", 
         col.ind.sup="blue", col.quali="magenta", label=c("ind", "ind.sup", "quali"),
         new.plot=TRUE)

  #size letters
plot.PCA(res, axes=c(1, 2), choix="ind", habillage="none", col.ind="black", 
         col.ind.sup="blue", col.quali="magenta", label=c("ind", "ind.sup", "quali"),
         new.plot=TRUE,cex=0.4)

  #contributive individuals
plot.PCA(res, axes=c(1, 2), choix="ind", habillage="none", col.ind="black", 
         col.ind.sup="blue", col.quali="magenta", label=c("ind", "ind.sup", "quali"),
         new.plot=TRUE,cex=0.7,select="contrib 5")

  #cos2
plot.PCA(res, axes=c(1, 2), choix="ind", habillage="none", col.ind="black", 
         col.ind.sup="blue", col.quali="magenta", label=c("ind", "ind.sup", "quali"),
         new.plot=TRUE,cex=0.7,select="cos2 0.7")

  #illustrative variable colors
plot.PCA(res, axes=c(3, 4), choix="ind", habillage="none", col.ind="black", 
         col.ind.sup="blue", col.quali="magenta", label=c("ind", "ind.sup", "quali"),
         new.plot=TRUE,invisible = "quali")

#variables cloud
plot.PCA(res, axes=c(1, 2), choix="var", new.plot=TRUE, col.var="black", 
         col.quanti.sup="blue", label=c("var", "quanti.sup"), lim.cos2.var=0)


summary(res, nb.dec = 3, nbelements=10, nbind = 10, ncp = 3, file="")

#remove(got.PCA.PCA)


#help to analyse the dimensions
dimdesc(res, axes = 1:3, proba = 0.05)


#3D visualisation

  #creation of a dataset : coordinate and cluster category
tab_3D<-cbind(as.data.frame(res$ind$coord),as.data.frame(res.hcpc$data.clust$clust))
tab_3D
names(tab_3D)<-c("dim1","dim2","dim3","dim4","dim5","clust")



clust1<-tab_3D[which(tab_3D$clust==1),]
clust1
clust2<-tab_3D[which(tab_3D$clust==2),]
clust2
clust3<-tab_3D[which(tab_3D$clust==3),]
clust3
clust4<-tab_3D[which(tab_3D$clust==4),]
clust4
clust5<-tab_3D[which(tab_3D$clust==5),]
clust5
clust6<-tab_3D[which(tab_3D$clust==6),]
clust6

  #rgl

library(rgl)
plot3d(tab_3D$dim1,tab_3D$dim2,tab_3D$dim3,col=as.numeric(tab_3D$clust),size=20,labels=row.names(tab_3D),xlab = "dim 1",ylab = "dim 2",zlab = "dim 3", lwd=100)
axes3d(box=TRUE,lwd=2)

# ellips1 <- ellipse3d(cov(cbind(clust1[,1],clust1[,2],clust1[,3])), 
#                      centre=c(mean(clust1[,1]), mean(clust1[,2]), mean(clust1[,3])), level = 0.95)
# wire3d(ellips1, col = "black",  lit = FALSE)
# shade3d(ellips1, col = "black",alpha=0.2)
ellips2 <- ellipse3d(cov(cbind(clust2[,1],clust2[,2],clust2[,3])), 
                     centre=c(mean(clust2[,1]), mean(clust2[,2]), mean(clust2[,3])), level = 0.95)
wire3d(ellips2, col = "red",  lit = FALSE)
shade3d(ellips2, col = "red",alpha=0.2)
ellips3 <- ellipse3d(cov(cbind(clust3[,1],clust3[,2],clust3[,3])), 
                     centre=c(mean(clust3[,1]), mean(clust3[,2]), mean(clust3[,3])), level = 0.95)
wire3d(ellips3, col = "green",  lit = FALSE)
shade3d(ellips3, col = "green",alpha=0.5)
ellips4 <- ellipse3d(cov(cbind(clust4[,1],clust4[,2],clust4[,3])), 
                     centre=c(mean(clust4[,1]), mean(clust4[,2]), mean(clust4[,3])), level = 0.95)
wire3d(ellips4, col = "blue",  lit = FALSE)
shade3d(ellips4, col = "blue",alpha=0.2)
ellips5 <- ellipse3d(cov(cbind(clust5[,1],clust5[,2],clust5[,3])), 
                     centre=c(mean(clust5[,1]), mean(clust5[,2]), mean(clust5[,3])), level = 0.95)
wire3d(ellips5, col = "cyan",  lit = FALSE)
shade3d(ellips5, col = "cyan",alpha=0.2)
ellips6 <- ellipse3d(cov(cbind(clust6[,1],clust6[,2],clust6[,3])), 
                     centre=c(mean(clust6[,1]), mean(clust6[,2]), mean(clust6[,3])), level = 0.95)
wire3d(ellips6, col = "pink",  lit = FALSE)
shade3d(ellips6, col = "pink",alpha=0.2)



####related packages####

#Factoshiny
library(Factoshiny)

  #directly on the dataset
RES<-PCAshiny(as.data.frame(got.PCA))
RES

  #on the results of a pca
resshiny = PCAshiny(res)

  #classification
res2=HCPCshiny(res)

%>% 
~

#MCAshiny


#Factoinvestigate
library(FactoInvestigate)

Investigate(res)


