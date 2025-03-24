#Sampling schedules

sam<-c(4,10,20,30,40,60,80,100,200,300,400,500)

#cost
persample<-1000 #cost per sample
samplingcost<-sam*persample
perevent<-100000 #cost per event

#design distribution
n<-10000

# aflat<-rgamma(n,0.15, 0.03) #(5ppb)
# aflat<-rgamma(n,0.3, 0.03) #(10ppb)
# aflat<-rgamma(n,0.46, 0.03) #(15ppb)
# aflat<-rgamma(n,0.6, 0.03) #(20ppb)
# aflat<-rgamma(n,0.75, 0.03) #(25ppb)
#aflat<-rgamma(n,0.91, 0.03) #(30ppb)
 aflat<-rgamma(n,1.05, 0.03) #(35ppb)
# aflat<-rgamma(n,1.21, 0.03) #(40ppb)

print(mean(aflat))
print(sd(aflat))

#---
#regulatory limit
reg<-20 #regulatory threshold

#---  
result<-vector(mode="numeric", length=0)
sampling<-vector(mode="numeric", length=0)

for (x in 1:12) {
  
  trials<-vector(mode="numeric", length=0)
  
  print (x)
  s<-aflat #assign dataset
  j<-sam[x] #sampling strategy
  
  sampling<-append(sampling, j)
  negative<-0
  positive<-0
  for (i in 1:10000) { # loop through trials of each sampling strategy  
    
    sa<-sample(s, (j)) #take a j-sized sample from s
    trials<-append(trials, mean(sa))
    
    
    if (mean(sa)<reg) { #this is the regulatory threshold, mean over this is a hit, mean under this is a miss
      negative<-negative+1
    }
    
    else {
      positive<-positive+1
    } 
    
  }
  assign(paste("trials",j, sep=""), trials)
  result<-append(result, (negative/(positive+negative))) #this is the probability of failure to detect adulteration
}

# plot trials and probability of negative finding

trials<-c(trials4,trials10,trials20,trials30,trials40,trials60,trials80,trials100,trials200,trials300,trials400,trials500) #for ylim for the boxplot
boxplot(trials4,trials10,trials20,trials30,trials40,trials60,trials80,trials100,trials200,trials300,trials400,trials500, names=(sampling), ylim=c(0,max(trials)), ylab="Point Estimate", xlab="Samples")
plot(sam, result, ylim=c(0,1), ylab="probability of negative", type="b")

#bad number of units into population based on probability
bad<-length(aflat[which(aflat>reg)])  #number of units over regulatory limit in the distribution
bad
pbad<-bad*result
plot(sam, pbad, type="b", xlab="samples", ylab="adulterated units released") #plot the number of adulterated units potentially reaching consumer

#economic calculation

totalcost<-samplingcost+pbad*perevent
mincost<-min(totalcost)
cost<-data.frame(sam,totalcost)
optimalsampling<-subset(cost, cost$totalcost == mincost)
optimalsampling
plot(sam, totalcost, type="b", xlim=c(0,2*optimalsampling$sam), xlab="Samples", ylab="Total Cost ($)")