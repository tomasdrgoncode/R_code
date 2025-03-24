k<- 350 #events)
n<- 365 #units(days)


l<-k/n #lambda

x<-rpois(365, lambda = l)
table(x)

year<-x


b<-vector(mode="numeric", length=0)
days<-vector(mode="numeric", length = 0)
negative<-0 
positive<-0

for (i in 1:3650) { # loop through trials  
    
  b<-sample(year, 10, replace=FALSE) #sampling, make sure to set the "replacement"
  days<-append(days, sum(b)) #collect sums of murders per 10 day streaks
  
  if (sum(b)<1) { #what is the minimum of hits for a positive?
  negative<-negative+1
  }
    
  else {
  positive<-positive+1
  } 
    
}
  
result<-negative/(positive+negative) 

table(days)
hist(days, breaks=20, xlim=c(0,25), xlab = "Murders per 10 consecutive days", main = "Baltimore Murders")

descdist(days, discrete = TRUE)
