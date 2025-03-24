#USDA sampling guidance for aflatoxins


lotlb<-c(220, 440, 1100, 2200, 4400, 11000, 22000, 150000)#lot sizes in pounds
lot<-round(lotlb*0.45*10, digits = 0) #lot sizes in 100g increments

sam<-c(10,15,20,30,40,60,80,100) #sampling schedule oper lot size
#sam<-c(rep(50, times=8)) #if we were taking the same # of samples from each size lot

reg<-15 #regulatory threshold
p_est<-15 #point estimate of aflatoxin concentration
het<-10 #heterogeneity

#or Michael Henry's distribution across lots
aflat<-c(scan("Henry_aflatoxins.txt"), rep(0, times=1844))
plot(density(aflat), xlim=c(0,200))

descdist(aflat, discrete = FALSE) #which distribution fits best?

fitg<-fitdist(aflat, "gamma", method="mme") #fit gamma
plot(fitg)


result<-vector(mode="numeric", length=0)
sampling<-vector(mode="numeric", length=0)

for (x in 1:8) {
  
  trials<-vector(mode="numeric", length=0)
  
  print (x)
#normal distribution
#s<-rnorm(lot[x], mean=(p_est), sd=(het)) # for bulk material with mostly even distribution
#Michael's distribution
  s<-aflat
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
  assign(paste("trials",x, sep=""), trials)
  result<-append(result, (negative/(positive+negative))) 
}

print(result)

hist(s)

barplot(result, names.arg=(lotlb))

trials<-c(trials1,trials2,trials3,trials4,trials5,trials6,trials7,trials8)

#trialsSD represents a utility/importance of lot size.... the smaller SD the more important the lot and thus higher precision in determination of point estimate
trialsSD<-c(sd(trials1),sd(trials2),sd(trials3),sd(trials4),sd(trials5),sd(trials6),sd(trials7), sd(trials8))

boxplot(trials1,trials2,trials3,trials4,trials5,trials6,trials7,trials8, names=(lotlb), ylim=c(0,max(trials)))

#plot utility vs sampling bin
barplot(trialsSD, ylim=c(0,max(trialsSD)), names.arg=lotlb)

#plot utility vs lb
plot(lotlb, trialsSD, type="b", ylim=c(0,max(trialsSD)))

#plot utility vs sampling (does not work yet)
plot(lotlb, -sam, type="b")

#plot sampling and utility
plot(sam, trialsSD, type="b", xlim=c(0,100), ylim=c(0,max(trialsSD)))
plot(log(sam), trialsSD, type="b")

