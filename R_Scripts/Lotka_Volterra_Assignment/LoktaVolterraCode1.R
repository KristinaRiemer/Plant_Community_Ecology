#Becka Downard
#WILD 6770
#Lokta-Voltera
#assign parameters and vectors
r1=1.01
r2=1.05
K1=1000
K2=900
initalN1=30
Time=seq(1:100)
N1=rep(NA,length(Time))
N2=rep(NA,length(Time))
N1[1]=initialN1
N2[1]=initialN1
A=matrix(c(1,2,3,4),nrow=2)


#add alpha, can add as a matrix and second population
#define logistic growth function
#modify
LK.growth=function(r,K,Na,Nb,Aa,Ab) {
  updatedN=Na+r*Na*((K-Aa*Na-Ab*Nb)/K)
  return(updatedN)
}
#we updated with Na, only running loop for 2 rounds

#cleaner loop because we've put the variables in the right order
for(i in 2:length(Time)){
  N1[i]=LK.growth(r1,K1,N1[i-1],N2[i-1],A[1,1],A[1,2])
}
plot(Time,N1)
#something in the loop is wrong
#dirty loop
#the loop
for(i in 2:length(Time)){
  N1[i]=LK.growth(r=r1,K=K1,Na=N1[i-1],Nb=N2[i-1],Aa=A[1,1],Ab=A[1,2])
}
plot(Time,N1)