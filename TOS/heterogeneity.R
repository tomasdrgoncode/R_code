#seed data

size<-1000 #number of decision units
prop<-0.005 #portion of adulterated decision units
bad<-size*prop #number of adulterated servings
good<-size*(1-prop) #number of clean servings

data<-c(rep(0, times=good), rep(1, times=(bad))) #data vector
data<-sample(data) #randomized

mean(data) #check clump seeds

#----------------------------------------------------------------------
#measure heterogeneity through moving average before clustering
n=10 #size of the moving average window

cx<-cumsum(data)
rsum <- (cx[(n+1):length(data)] - cx[1:(length(data) - n)]) / n
plot(rsum, type="l")

cx<-cumsum(cdata) #this is for clumped data
rsum <- (cx[(n+1):length(cdata)] - cx[1:(length(cdata) - n)]) / n
plot(rsum, type="l")

#----------------------------------------------------------------------
#confirm where the clumps are 

for (ii in 1:length(data)) {  
  
  if (data[ii]==1){
    print(ii)        
  }
}


#----------------------------------------------------------------------
#grow clumps (non-probabilistic)


for (iii in 1:20) {
  for (ii in 1:(length(data)-1)) {  
    
        if ((data[ii+1]==1)){
            data[ii]<-1  
            print(ii)
          }
  }
}

#---------------------------------------------------------------------
#create clumped and non-clumped data

cdata<-data #create clumped data object
data<-sample(data) #create non-clumped data with same % of adulteration
mean(data) #check total level of adulteration
mean(cdata)

#----------------------------------------------------------------------
#measure clump distribution in non-clumped and clumped data
#non-clumped
#set to zero
clump<-0
clumps<-vector(mode="numeric", length=0)


for (i in data) {
  if (i==1) {
    clump<-clump+1
  } else {
  clumps<-append(clumps, clump)
  clump<-0
  }
}

clumps<-clumps[clumps!=0] #remove zero sized clumps

max(clumps) #largest clump

hist(clumps)

#clumped
clump<-0
clumps<-vector(mode="numeric", length=0)


for (i in cdata) {
  if (i==1) {
    clump<-clump+1
  } else {
    clumps<-append(clumps, clump)
    clump<-0
  }
}

clumps<-clumps[clumps!=0] #remove zero sized clumps

max(clumps) #largest clump

hist(clumps)

#----------------------------------------------------------------------
#Test sampling schemes

result<-vector(mode="numeric", length=0)

b<-vector(mode="numeric", length=0)
sampling<-vector(mode="numeric", length=0)



for (j in seq(from=0, to=500, by=10)) { # loop through the sampling strategies
  #zero out positive/negative counters
  negative<-0 
  positive<-0
  print (j)
  sampling<-append(sampling, j)
  for (i in 1:10000) { # loop through trials of each sampling strategy  
    
    b<-sample(cdata, (j))
    
    if (sum(b)==0) {
      negative<-negative+1
    }
    
    else {
      positive<-positive+1
    } 
    
  }
  
  result<-append(result, (negative/(positive+negative))) 
}

print(result) 
plot(sampling, result, xlim=c(0,500), ylim=c(0,1), type="l")

par(new=TRUE)
plot(sampling, result, xlim=c(0,500), ylim=c(0,1), type="l", col="blue")





