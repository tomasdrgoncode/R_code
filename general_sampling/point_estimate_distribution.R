#what is the underlying distribution of point estimates given a sampling result?
#say we have 1000 units, sample 10 and get 1 positive, what are the possible numbers of bad units in the 100?

total<-20 # number of sampling units in article

#zero out vectors and counters
result<-vector(mode="numeric", length=0)
pointest<-vector(mode="numeric", length=0)
positive<-0
negative<-0


# loop through datasets with different point estimates, (j = number of point estimates)
for (j in 0:100) {   
  
  portion<-j/100 #portion of adulterated units (equals pointest)
  sampling<-50 #sampling strategy

  bad<-total*portion # number of adulterated units in article
  good<-total-bad # number of clean units in article 
  data<-c(rep(0, times = good), rep(1, times = bad)) # custom vector of observations

    #zero out trial vectors and counters
  a<-vector(mode="numeric", length=0)
      positive<-0
      negative<-0

    for (i in 1:5000) { # loop through trials sampling strategy on the same point estimate  
    
      a<-sample(data, (sampling), replace=TRUE) #sampling, make sure to set the "replacement"
      
      if (sum(a)==1) { #how many positive samples constitute a positive finding? Is this < or = ?
        positive<-positive+1 # count positive
      }
      
      else {
        negative<-negative+1 # count negative
      } 
      
  }
  result<-append(result, (positive/(positive+negative))) # vector of probabilities of positive 
  pointest<-append(pointest, portion) # vector of point estimates   

}

result_sum<-sum(result)
result_norm<-result/result_sum

plot(pointest,result_norm, type="b")

#add second series to the plot

lines(pointest, result_norm, col="red")



# no simulation approach - beta distribution
# alpha = hits, beta = misses, n = # of units(??)

plot(density(rbeta(3617, 27, 36)), xlim=c(0,1))
lines(density(rbeta(200000, 2, 10)), xlim=c(0,1), col="red")

#boxplots
a<-rbeta(200000, 1, 11)
b<-rbeta(200000, 2, 10)

data<-data.frame(a, b)

boxplot(data, ylab="% adulteration")

#confidence interval of beta distribution

x<-rbeta(3617, 27, 36)

hist(x)
quantile(x,probs=c(.025,.975)) #calculate 95% confidence interval
quantile(x,probs=c(.005,.995)) #calculate 99% confidence interval



