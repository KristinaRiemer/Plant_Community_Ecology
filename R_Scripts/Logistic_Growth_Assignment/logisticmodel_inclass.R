# parameters
r=0.1
K=100

#set up vectors; empty columns
Time=1:200
N=rep(0,length(Time))

# set initial value of N by indexing vector
N[1]=2

# update N one at a time
# N[2]=N[1]+r*N[1]*(1-n[1]/K)

# example of a loop
# for(i in 1:10){
#  print(i)
#}

for(i in 2:max(Time)){
  N[i]=N[i-1]+r*N[i-1]*(1-N[i-1]/K)
}
