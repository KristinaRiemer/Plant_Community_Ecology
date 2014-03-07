install.packages("vegan")
library(vegan)
library(MASS)

#---NMDS Script for Plant Community Ecology--------
data<-read.csv("C:/Users/a00988540/Downloads/KLEEdataE.csv",header=TRUE)
data
data1<-data[,3:ncol(data)]
data1

#calculate dissimilarities, use function "vegdist"in VEGAN package
data.dis<-vegdist(data1)
data.dis

#calculate isoMDS function, use isoDS function in MASS package
data.mds<-isoMDS(data.dis)

#--------stressplot and ordiplot----------
stressplot(data.mds,data.dis)
ordiplot(data.mds,type="t")

#read in environmental data
data.env<-data[,1:2]
data.env

#set distclass to factor
data.env[,1]=as.factor(data.env[,1])
data.env[,1]
is.factor(data.env[,1])  

#set distclass to factor
data.env[,2]=as.factor(data.env[,2])
data.env[,2]
is.factor(data.env[,2])  

data.mds<-metaMDS(comm=data1,distance="euclidean",trace=FALSE)
data.mds

#function "envfit" fits environmental vectors or factors onto an ordination.
#requires ordination plot first before plot(fit)
fit<-envfit(data.mds,data.env,perm=1000)
fit

#plotMDS
plot(data.mds,type="t",main="NMDS using Euclidean Distance")

#plot environmental loadings
plot(fit)
