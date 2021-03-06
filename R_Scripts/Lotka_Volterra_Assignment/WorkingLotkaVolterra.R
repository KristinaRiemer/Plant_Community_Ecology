#Lokta-Volterra Two-Species Model


# r and K parameters into vectors
r=c(1.01,1.05)
K=c(1000,900)

# initializing N and time
initialN=30
Time=seq(1:100)
N1=rep(NA,length(Time))
N2=rep(NA,length(Time))
N1[1]=initialN
N2[1]=initialN

# alpha parameters into matrix
A=matrix(c(1,1,1,1),nrow=2)


# defining Lotka Volterra function
# firstN and firstA are for the target sp., secondN and secondA for second sp.
lotka.volterra = function(firstN,secondN,r,K,firstA,secondA){
  newfirstN = firstN+r*firstN*((K-firstA*firstN-secondA*secondN)/K)
  return(newfirstN)
}


#loop through lotka.volterra function for species 1 and 2
for(i in 2:length(Time)){
  N1[i] = lotka.volterra(N1[i-1],N2[i-1],r[1],K[1],A[1,1],A[1,2])
  N2[i] = lotka.volterra(N2[i-1],N1[i-1],r[2],K[2],A[2,2],A[2,1])
}


#plot species abundances
plot(Time,N1,col="red",xlab="Time (Days)",ylab="Abundances")
points(Time,N2,col="blue")
legend("right",title="Species", pch=c(1,1), col=c("red","blue"),legend=c("Species 1","Species 2"))








