# Part I ------------------------------------------------

site1 = c(50,20,10,5,2,1,1,0,0)
site2 = c(10,30,5,15,0,0,2,1,1)
site3 = c(1,1,1,1,1,1,1,0,1)

species = paste("spp",c(1:9),sep="")
par(mfrow=c(3,1))
barplot(site1,ylab="Abundance",ylim=c(0,55),main="Site1")
barplot(site2,ylab="Abundance",ylim=c(0,55),main="Site2")
barplot(site3,ylab="Abundance",ylim=c(0,55),names.arg=species,main="Site3")

# Similarity based on presence absence:
# Jaccard's index
jaccards=function(x,y){
  #x and y are vectors of spp abundances
  #they must be the same length!
  if(length(x)!=length(y)) stop("Bad abundances!")
  # get number of species present at both sites
  a = length(which(x>0 & y>0))
  # get number of species present at first but not second site
  b = length(which(x>0 & y==0))
  # get number of species present at second but not first site
  c = length(which(x==0 & y>0))
  out = a/(a+b+c)
  out
}

# Try:
jaccards(site1,site2)
jaccards(site1,site3)
jaccards(site2,site3)

# Ecological distance (dissimilarity), using
# Euclidean distance
ED = function(x,y){
  #x and y are vectors of spp abundances
  #they must be the same length!
  if(length(x)!=length(y)) stop("Bad abundances!")
  out =  sqrt(sum((x-y)^2))
  out
}

# Try:
ED(site1,site2)
ED(site1,site3)
ED(site2,site3)

#Bray-Curtis 

BC = function(x,y){
  #x and y are vectors of spp abundances
  #they must be the same length!
  if(length(x)!=length(y)) stop("Bad abundances!")
  out = sum(abs(x-y))/sum(x+y)
  out
}

# Try:
BC(site1,site2)
BC(site1,site3)
BC(site2,site3)


##  Create similarity/ distance matrix

speciessitematrix = rbind(site1, site2, site3)
speciessitematrix

similaritydistancematrix = matrix(NA,3,3)
similaritydistancematrix

# and we want to compare all possible pairs of the species, 1 vs. 2, 1 vs 3, etc.
# First set up a matrix:
compare = matrix(NA,length(species),length(species))
compare
# To fill in this matrix, we can set up a couple of nexted loops
for(x in 1:length(species)){
  for(y in 1:length(species)){
    compare[x,y]=paste(x,"vs",y)
  }
}
compare







# Part II ------------------------------------------------
dev.off()  # close the graphing window

D1=read.csv("gber_data.csv")

# take mean density across quads
D2 = aggregate(D1$density,by=list(location=D1$location,
   year=D1$year,species=D1$species),FUN=mean)
# fix the name of the density column
names(D2)[4] = "density"

# put each species in its own column
D2 = reshape(D2,idvar=c("location","year"),
   timevar="species",direction="wide")
# make missing values = 0
D2[is.na(D2)] = 0

# order the data frame by location and year
D2=D2[order(D2$location,D2$year),]

# make sure year is numeric
D2$year = as.numeric(as.character(D2$year))

# EXAMPLE: similarity over time for one location
# using Euclidean Distance

D3 = subset(D2,location=="Snwbry")   # subset data for one site
EDmatrix = matrix(NA,dim(D3)[1],dim(D3)[1])   # create a place to store the dissimilarity values
timeLags = matrix(NA,dim(D3)[1],dim(D3)[1])   # create a place to store the time lags

# HERE IS WHERE YOUR REAL WORK STARTS

# we need two loops to compare all possible pairs of years "j" and "k"

# First loop through all "j" years from 1 to the total number of years
  # Now loop through all "k" years from 1 to the total number of years
    
    # grab density data for year "j"
    
    # grab density data for year "k"
    
    # calculate and store (in the EDmatrix) the ED value based on the data for year j and k
    
    # calculate and store (in timeLags) the time lag (the absolute value of the difference 
    # in time between year j and k
    
  # exit  k loop
# exit j loop

EDmatrix[lower.tri(EDmatrix, diag=T)]=NA    # set duplicate entries to NA
timeLags[lower.tri(timeLags, diag=T)]=NA     # set duplicate entries to NA
y = as.vector(EDmatrix)  # turn the matrix into a vector
x = as.vector(timeLags)  # turn the matrix into a vector
plot(sqrt(x),y)

# You will need to modify this code to also calculate the
# Jaccard's similarity and Bray-Curtis distance and to
# analyze data from the other two sites