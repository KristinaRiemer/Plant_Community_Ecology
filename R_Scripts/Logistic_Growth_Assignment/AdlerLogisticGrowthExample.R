r=1.02
K=500
initialN=2
Time=seq(1:200)
N=rep(NA,length(Time))
N[1]=initialN
growth=function(r,K,N) {
  updatedN=N+ r*N* (1-N/K)
  return(updatedN)
}
for(i in 2:length(Time)) {
  N[i]=growth(r=r,K=K,N=N[i-1])
}
plot(Time,N)