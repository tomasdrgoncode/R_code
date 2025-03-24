#data

total<-16000000
portion<-0.000037445

bad<-total*portion
good<-total-bad

good
bad

a<-c(rep(0, times = good), rep(1, times = bad))# custom vector of observations



a<-c(rep(0, times = 9990), rep(1, times=10)) # vector of observations, 0.1%
a<-c(rep(0, times = 9950), rep(1, times=50)) # vector of observations, 0.5%
a<-c(rep(0, times = 9900), rep(1, times=100)) # vector of observations, 1%
a<-c(rep(0, times = 9500), rep(1, times=500)) # vector of observations, 5%
a<-c(rep(0, times = 9000), rep(1, times=1000)) # vector of observations, 10%
a<-c(rep(0, times = 8500), rep(1, times=1500)) # vector of observations, 15%
a<-c(rep(0, times = 8000), rep(1, times=2000)) # vector of observations, 20%
a<-c(rep(0, times = 7500), rep(1, times=2500)) # vector of observations, 25%
a<-c(rep(0, times = 7000), rep(1, times=3000)) # vector of observations, 30%

a<-rnorm(10000, mean=10, sd=1) #normal distribution

a<-rnorm(40000, mean=41, sd=11.3) #normal distribution


# code for plotting power curves for different sampling strategies
# for bunomial distributions 
# zero out vectors
result<-vector(mode="numeric", length=0)
b<-vector(mode="numeric", length=0)
sampling<-vector(mode="numeric", length=0)



for (j in seq(from=100, to=1000, by=100)) { # loop through a sequence of sampling strategies

#or
  
for (j in c(18,108)) { #loop through discrete sampling strategies
  
  #zero out positive/negative counters
  negative<-0 
  positive<-0
  print (j)
  sampling<-append(sampling, j)

  for (i in 1:1000) { # loop through trials of each sampling strategy  
    
    b<-sample(a, (j), replace=TRUE) #sampling, make sure to set the "replacement"
    
    if (sum(b)<1) { #what is the minimum of hits for a positive?
      negative<-negative+1
    }
    
    else {
      positive<-positive+1
    } 
    
  }
  
  result<-append(result, (negative/(positive+negative))) 
}
print(sampling)
print(result) 
plot(sampling, result, ylim=c(0,1), type="l")

par(new=TRUE)
plot(sampling, result, ylim=c(0,1), type="l", col="black", xaxp  = c(0, 100000, 10))



# code for plotting power curves for different sampling strategies
# for normal distributions 
# zero out vectors
result<-vector(mode="numeric", length=0)
b<-vector(mode="numeric", length=0)
sampling<-vector(mode="numeric", length=0)




# Code for box plots of sampling strategy results


# zero out vectors for sampling strategies (10, 50, 100, 500, 1000, 5000 samples)
m3<-numeric()
m10<-numeric()
m50<-numeric()
m100<-numeric()
m500<-numeric()
m1000<-numeric()
m5000<-numeric()

# loop through sampling strategies (Monte Carlo trials)
# in a binary dataset with certain % of "1", how many "1" will we find with each sampling strategy?
for (i in 1:1000) {  
  m3<-append(m3, sum(sample(a, 3)))
  m10<-append(m10, sum(sample(a, 10)))
  m50<-append(m50, sum(sample(a, 50)))
  m100<-append(m100, sum(sample(a, 100)))
  m500<-append(m500, sum(sample(a, 500)))
  m1000<-append(m1000, sum(sample(a, 1000)))
  m5000<-append(m5000, sum(sample(a, 5000)))

}

# loop through sampling strategies (Monte Carlo trials)
# in a binary dataset with certain % of "1", how well do we represent the whole (mean) with each sampling strategy?
for (i in 1:1000) {  
  m3<-append(m3, mean(sample(a, 3)))
  m10<-append(m10, mean(sample(a, 10)))
  m50<-append(m50, mean(sample(a, 50)))
  m100<-append(m100, mean(sample(a, 100)))
  m500<-append(m500, mean(sample(a, 500)))
  m1000<-append(m1000, mean(sample(a, 1000)))
  m5000<-append(m5000, mean(sample(a, 5000)))
  
}



data<-data.frame(m3, m10, m50, m100, m500, m1000, m5000)

bx.p<-boxplot(data)
bxp(bx.p,pars=list(ylim=c(0,max(m3))))#lets me use the ylim on a boxplot








# Code for finding a confidence interval for a particular set of values (sampling strategy and limits)

# zero out vectors

sampleset<-vector(mode="numeric", length=0)

  
for (i in 1:1000) { # loop through trials of each sampling strategy  
    
    b<-sample(a, (1000), replace=TRUE) #sampling, make sure to set the "replacement"
    sampleset<-append(sampleset, sum(b))
    
}
  
hist(sampleset)
quantile(sampleset,probs=c(.025,.975)) #calculate 95% confidence interval
quantile(sampleset,probs=c(.005,.995)) #calculate 99% confidence interval




#equation for binomial sampling
#gives probability of missing a single hit given frequency of outcome

c<-0.98 #confidence
p<-c(0.01,0.05,0.1,0.2,0.3) #prevalence
x<-log(1-c)/log(1-p) #of samples
x

# with correction for finite population
N<-100
xf<-(x*N)/(x+(N-1))
xf<-round(xf, digits=0)

result<-data.frame(p,xf)
names(result)[1]<-paste("Anticipated portion of bad recalls")
names(result)[2]<-paste("Number of samples")

View(result, paste(N," cosignees, ", "Conf = ", c))




#equation for binomial sampling
#gives probability of missing a single hit given constant # of undetected outcomes
#WORK ON THIS
c<-0.95 #confidence
t<-10 #number of hits that is tollerable to miss
p<-c(0.01,0.05,0.1,0.2,0.3) #prevalence
N<-500
pp<-

x<-log(1-c)/log(1-p) #of samples
x

# with correction for finite population?
xf<-(x*N)/(x+(N-1))
xf


