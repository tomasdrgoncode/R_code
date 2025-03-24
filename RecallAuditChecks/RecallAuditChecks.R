#equation for binomial sampling
#gives probability of missing a single hit given frequency of outcome

c<-0.95 #confidence
N<-50 #number of consignees
p<-c(0.01,0.02,0.05,0.1,0.2,0.3) #prevalence

x<-log(1-c)/log(1-p) #of samples
xf<-(x*N)/(x+(N-1))# with correction for finite population
xf<-round(xf, digits=0) #round to integers

result<-data.frame(p,xf)

names(result)[1]<-paste("Anticipated portion of bad recalls")
names(result)[2]<-paste("Number of samples")

View(result, paste(N," cosignees, ", "Conf = ", c))
