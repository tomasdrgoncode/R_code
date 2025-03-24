#Sampling schedules

sam<-c(4,10,20,30,40,60,80,100)

#---
#gamma distribution of choice (rate = 0.03), 
#shape = 0.46 (mean=15.3, SD=22.8)
#shape = 0.6 (mean=20.12, SD=25.18)
#shape = 0.75 (mean=24.96, SD=22.8)

n<-1000 #number of units/servings
aflat<-rgamma(n, 0.46, 0.03)
aflat<-rgamma(n, 0.6, 0.03)
aflat<-rgamma(n, 0.75, 0.03)

#or designer distribution
aflat<-rgamma(1000,0.15, 0.03) #(5ppb)
aflat<-rgamma(1000,0.3, 0.03) #(10ppb)
aflat<-rgamma(1000,0.46, 0.03) #(15ppb)
aflat<-rgamma(1000,0.6, 0.03) #(20ppb)
aflat<-rgamma(1000,0.75, 0.03) #(25ppb)
aflat<-rgamma(1000,0.91, 0.03) #(30ppb)
aflat<-rgamma(1000,1.05, 0.03) #(35ppb)
aflat<-rgamma(1000,1.21, 0.03) #(40ppb)



print(mean(aflat))
print(sd(aflat))

#---
#regulatory limit
reg<-20 #regulatory threshold
#p_est<-15 #point estimate of aflatoxin concentration
#het<-10 #heterogeneity

#---  
result<-vector(mode="numeric", length=0)
sampling<-vector(mode="numeric", length=0)

for (x in 1:8) {
  
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
  result<-append(result, (negative/(positive+negative))) 
}


plot(sam, result, ylim=c(0,1), type="b")

trials<-c(trials4,trials10,trials20,trials30,trials40,trials60,trials80,trials100)

boxplot(trials4,trials10,trials20,trials30,trials40,trials60,trials80,trials100, names=(sampling), ylim=c(0,max(trials)))


#---
#save after each different distribution
result40<-result

#---
#plot the negatives for each distribution
plot(sam, result20, ylim=c(0,0.6), type="b")
lines(sam, result25, type="b")
lines(sam, result30, type="b")
lines(sam, result35, type="b")
lines(sam, result40, type="b")

#dataframe of all data
alldata<-data.frame(result20, result25, result30, result35, result40)









