##Preparation for running code
##Import two CSV datasets (species_biomass and pft_biomass)
##If necessary, change pathfile in Import Datasets below
##If necessary, install package matrixStats



##Import Datasets-----
read.csv("/Users/Kristina/Dropbox/Macbook Documents/Documents/Graduate School/Year 1/Plant Community Ecology (Adler)/R Scripts/Diversity Stability Assignment/pft_biomass.csv")
read.csv("/Users/Kristina/Dropbox/Macbook Documents/Documents/Graduate School/Year 1/Plant Community Ecology (Adler)/R Scripts/Diversity Stability Assignment/species_biomass.csv")

##Problem 1--------------
##Use library rpart for ?? 
##library(rpart)
##Use library matrixStats for colSds function
library(matrixStats)

  ##subset by site
    ##by species dominate species only
      spp.mean=colMeans(species_biomass)
      print(spp.mean>0.05) ##or
      spp_biomaa.05b=which(spp.mean>0.05)
      spp_biomass.05=subset(species_biomass, select=c("site","year","AGSM",
          "ARFR","ARLO","BOGR","BUDA","CAHE","CHLE","FEOC","FORB",
            "GUSA","LEDE","OPPO","PSTE","SAIB","SIHY","SPCO"))
      common.spp=colMeans(spp_biomass.05)
    
      site1.spp=subset(spp_biomass.05, site==1, select=c(3:18))
       #or
      site.1.spp=spp_biomass.05[spp_biomass.05$site=="1",3:18]
      site2.spp=subset(spp_biomass.05, site==2, select=c(3:18))
      site3.spp=subset(spp_biomass.05, site==3, select=c(3:18))
        
      ##by functonal group
      site1.fun=subset(pft_biomass, site==1, select=c(3:8))
      site2.fun=subset(pft_biomass, site==2, select=c(3:8))
      site3.fun=subset(pft_biomass, site==3, select=c(3:8))
    
      ##by community
      site1.comm=rowSums(site1.fun)
      site2.comm=rowSums(site2.fun)
      site3.comm=rowSums(site3.fun)
    

##calculate coefficient of variation for each grouping by site
  #Community Level CV
      site1.comm.CV=100*(sd(site1.comm)/mean(site1.comm))
      site2.comm.CV=100*(sd(site2.comm)/mean(site2.comm))
      site3.comm.CV=100*(sd(site3.comm)/mean(site3.comm))
        commCV.matrix=rbind(site1.comm.CV,site2.comm.CV,site3.comm.CV)

  #functional group CV
    ##site 1
      site1.fun.mean=colMeans(site1.fun)
      site1.fun.sd=apply(site1.fun, 2, sd)
      #or
      site1.fun.sd2=colSds(site1.fun)
      site1.fun.CV=100*(site1.fun.sd/site1.fun.mean)
    ##Site 2
      site2.fun.sd=colSds(site2.fun)
      site2.fun.mean=colMeans(site2.fun)
      site2.fun.CV=100*(site2.fun.sd/site2.fun.mean)
    ##Site 3
      site3.fun.sd=colSds(site3.fun)
      site3.fun.mean=colMeans(site3.fun)
      site3.fun.CV=100*(site3.fun.sd/site3.fun.mean)
        funCV.matrix=rbind(site1.fun.CV,site2.fun.CV,site3.fun.CV)

  #Species CV
      ##site 1
        site1.spp.sd=colSds(site1.spp)
        site1.spp.mean=colMeans(site1.spp)
        site1.spp.CV=100*(site1.spp.sd/site1.spp.mean)
      ##site 2
        site2.spp.sd=colSds(site2.spp)
        site2.spp.mean=colMeans(site2.spp)
        site2.spp.CV=100*(site2.spp.sd/site2.spp.mean)
      ##site 3
        site3.spp.sd=colSds(site3.spp)
        site3.spp.mean=colMeans(site3.spp)
        site3.spp.CV=100*(site3.spp.sd/site3.spp.mean)
    
      sppCV.matrix=rbind(site1.spp.CV,site2.spp.CV,site3.spp.CV)

##Problem 2---------
    site1.ssp.var=subset(species_biomass, select=c("BOGR","BUDA","FEOC","GUSA","SPCO"))
    spp1=rnorm(20,mean(site1.ssp.var[,1]), sd(site1.ssp.var[,1]))
    spp2=rnorm(20,mean(site1.ssp.var[,2]), sd(site1.ssp.var[,2]))
    spp3=rnorm(20,mean(site1.ssp.var[,3]), sd(site1.ssp.var[,3])) 
    spp4=rnorm(20,mean(site1.ssp.var[,4]), sd(site1.ssp.var[,4]))  
    spp5=rnorm(20,mean(site1.ssp.var[,5]), sd(site1.ssp.var[,5])) 
    ran.matrix=rbind(spp1,spp2,spp3,spp4,spp5)
    spp1.cv=100*(sd(spp1)/mean(spp1))
    spp2.cv=100*(sd(spp2)/mean(spp2))
    spp3.cv=100*(sd(spp3)/mean(spp3))
    spp4.cv=100*(sd(spp4)/mean(spp4))
    spp5.cv=100*(sd(spp5)/mean(spp5))
    spp.ran.matrix=rbind(spp1.cv,spp2.cv,spp3.cv,spp4.cv,spp5.cv)

    comm.vector=c(spp1,spp2,spp3,spp4,spp5)
    comm.CV=100*(sd(comm.vector)/mean(comm.vector))


fivespecies=c(site1.ssp.var[,1], site1.ssp.var[,2],site1.ssp.var[,3],site1.ssp.var[,4],site1.ssp.var[,5])
fivespecies.CV=100*sd(randomspecies)/mean(randomspecies)

##Problem #3--------
##Use library Hmisc for rcorr function
##library(Hmisc)


  ##correlation by species -- are we not leaving out rare species still for this analysis?
    ## already did the subsetting above, e.g., sitex.spp
      ##spp_biomass.05.1=subset(spp_biomass.05, site==1, select=c(3:18))
      ##spp_biomass.05.2=subset(spp_biomass.05, site==2, select=c(3:18))
      ##spp_biomass.05.3=subset(spp_biomass.05, site==3, select=c(3:15,17,18))
    ## don't need to take subset to use whole data set
      ##spp_biomass.05.all=subset(spp_biomass.05, select=c(3:18))  
    
      ##spp_corr.1=cor(spp_biomass.05.1)
          ##spp_corr.1.p=rcorr(spp_corr.1)
          ##spp_corr.1.p=rcorr(spp_corr.1,type="spearman")
          ##data.table(spp_corr.1.p)
      ##spp_corr.2=cor(spp_biomass.05.2)
          ##spp_corr.2.p=rcorr(spp_corr.2)
      ##spp_corr.3=cor(spp_biomass.05.3)
          ##spp_corr.3.p=rcorr(spp_corr.3)
      ##spp_corr.all=cor(spp_biomass.05.all) ##all sites combined
          ##spp_corr.all.p=rcorr(spp_corr.all)

  ##correlation by species
    spp_corr.1=cor(site1.spp,method="spearman")
    spp_corr.2=cor(site2.spp,method="spearman")
    spp_corr.3=cor(site3.spp,method="spearman")

  ##Spearman's rank correlation w/ cor.test
    ##first.two.spp=cor.test(spp_biomass.05.1[,1],spp_biomass.05.1[,2],method="spearman",exact=FALSE)
      ##loop this somehow?

  ## function for p value stolen from internet 
   ##  http://stackoverflow.com/questions/13112238/a-matrix-version-of-cor-test-in-r
    cor.test.p <- function(x){
     FUN <- function(x, y, method,exact) cor.test(x, y, method="spearman",exact=FALSE)[["p.value"]]
     z <- outer(
       colnames(x), 
       colnames(x), 
       Vectorize(function(i,j) FUN(x[,i], x[,j]))
     )
     dimnames(z) <- list(colnames(x), colnames(x))
     z
    }
  

  ##p values for Spearmann's rank correlation using cor.test.p by species
    spp_pvalue.1=cor.test.p(site1.spp)
    spp_pvalue.2=cor.test.p(site2.spp)
    spp_pvalue.3=cor.test.p(site3.spp)

  ##correlation by func. group
    fun_corr.1=cor(site1.fun,method="spearman")
          ##fun_corr.1.p=rcorr(fun_corr.1)  
    fun_corr.2=cor(site2.fun,method="spearman")
          ##fun_corr.2.p=rcorr(fun_corr.2)  
    fun_corr.3=cor(site3.fun,method="spearman")
          ##fun_corr.3.p=rcorr(fun_corr.3) 
      ##fun_corr.all=cor(rbind(site1.fun,site2.fun,site3.fun))
          ##fun_corr.all.p=rcorr(fun_corr.all) 

  ##p values for Spearmann's rank correlation using cor.test.p by func. group
    fun_pvalue.1=cor.test.p(site1.fun)
    fun_pvalue.2=cor.test.p(site2.fun)
    fun_pvalue.3=cor.test.p(site3.fun)