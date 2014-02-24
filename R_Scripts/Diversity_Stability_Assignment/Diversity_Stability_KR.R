
##Import Datasets-----
read.csv("/Users/Kristina/Dropbox/Macbook Documents/Documents/Graduate School/Year 1/Plant Community Ecology (Adler)/R Scripts/Diversity Stability Assignment/pft_biomass.csv")
read.csv("/Users/Kristina/Dropbox/Macbook Documents/Documents/Graduate School/Year 1/Plant Community Ecology (Adler)/R Scripts/Diversity Stability Assignment/species_biomass.csv")
##Problem 1--------------

library(rpart)
##subset by site
  ##by species
  site1.spp=subset(species_biomass, site==1, select=c(1,3:58))
  site2.spp=subset(species_biomass, site==2, select=c(1,3:58))
  site3.spp=subset(species_biomass, site==3, select=c(1,3:58))

  ##by functonal group
  site1.fun=subset(pft_biomass, site==1, select=c(3:8))
  site2.fun=subset(pft_biomass, site==2, select=c(1,3:8))
  site3.fun=subset(pft_biomass, site==3, select=c(1,3:8))

  ##by community
  site1.comm=cbind(site1.spp[,1],rowSums(site1.fun[,2:7]))
  site2.comm=cbind(site2.spp[,1],rowSums(site2.fun[,2:7]))
  site3.comm=cbind(site3.spp[,1],rowSums(site3.fun[,2:7]))

##calculate coefficient of variation for each grouping by site
  site1.spp.CV=100*()



site1.comm.CV=100*(sd(site1.comm[,2])/mean(site1.comm[,2]))