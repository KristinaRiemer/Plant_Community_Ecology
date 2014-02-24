# parameters r and K
r=.8
K=1000

# time vector for 100 time steps
time <- seq(1:100)

# setting up N vector
N=rep(NA,length(time))

# initial N to start loop at
N[1]=2

# creating logistic growth function
logistic.growth <- function(N,r,K) {
  newN = N+r*N*(1-N/K)
  return(newN)
}
  
# loop of logistic growth function
for(i in 1:(length(time)-1)) {
  N[i+1] = logistic.growth(N[i],r,K)
}

# plot output
plot(time,N)