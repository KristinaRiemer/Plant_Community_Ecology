
##Import Datasets-----
read.csv("/Users/Kristina/Dropbox/Macbook Documents/Documents/Graduate School/Year 1/Plant Community Ecology (Adler)/R Scripts/Diversity Stability Assignment/pft_biomass.csv")
read.csv("/Users/Kristina/Dropbox/Macbook Documents/Documents/Graduate School/Year 1/Plant Community Ecology (Adler)/R Scripts/Diversity Stability Assignment/species_biomass.csv")
##Problem 1--------------

library(rpart)
##subset by site
##by species
site1.spp=subset(species_biomass, site==1, select=c(3:58))
site2.spp=subset(species_biomass, site==2, select=c(3:58))
site3.spp=subset(species_biomass, site==3, select=c(3:58))

##by functonal group
site1.fun=subset(pft_biomass, site==1, select=c(3:8))
site2.fun=subset(pft_biomass, site==2, select=c(3:8))
site3.fun=subset(pft_biomass, site==3, select=c(3:8))

##by community
site1.comm=rowSums(site1.fun)
site2.comm=rowSums(site2.fun)
site3.comm=rowSums(site3.fun)

##calculate coefficient of variation for each grouping by site
site1.spp.CV=100*()



site1.comm.CV=100*(sd(site1.comm)/mean(site1.comm))
site2.comm.CV=100*(sd(site2.comm)/mean(site2.comm))
site3.comm.CV=100*(sd(site3.comm)/mean(site3.comm))