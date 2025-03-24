a<-c(rep(0, times = 9990), rep(1, times=10)) # vector of observations, 0.1%
a<-c(rep(0, times = 9950), rep(1, times=50)) # vector of observations, 0.5%
a<-c(rep(0, times = 9900), rep(1, times=100)) # vector of observations, 1%
a<-c(rep(0, times = 9500), rep(1, times=500)) # vector of observations, 5%
a<-c(rep(0, times = 9000), rep(1, times=1000)) # vector of observations, 10%
a<-c(rep(0, times = 8000), rep(1, times=2000)) # vector of observations, 20%

#for small filers
nlines<-1000 #filer size
er<-0.01 #error rate

a<-c(rep(0, times = nlines-nlines*er), rep(1, times=nlines*er)) # vector of observations, filer w 100


# zero out vectors
result<-vector(mode="numeric", length=0)
b<-vector(mode="numeric", length=0)
sampling<-vector(mode="numeric", length=0)



for (j in seq(from=0, to=100, by=5)) { # loop through the sampling strategies
  #zero out positive/negative counters
  negative<-0 
  positive<-0
  print (j)
  sampling<-append(sampling, j)

  for (i in 1:10000) { # loop through trials of each sampling strategy  
    
    b<-sample(a, (j), replace=FALSE) #sampling, make sure to set the "replacement"
    
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
plot(sampling, result, xlim=c(0,100), ylim=c(0,1), type="l",xaxp  = c(0, 100, 10))

par(new=TRUE)
plot(sampling, result, xlim=c(0,100), ylim=c(0,1), type="l", col="black", xaxp  = c(0, 100, 10))



# to find a confidence interval for a particular set of values (sampling strategy and limits)

# zero out vectors

sampleset<-vector(mode="numeric", length=0)

  
for (i in 1:1000) { # loop through trials of each sampling strategy  
    
    b<-sample(a, (500), replace=TRUE) #sampling, make sure to set the "replacement"
    sampleset<-append(sampleset, sum(b))
    
}
  
hist(sampleset)
quantile(sampleset,probs=c(.025,.975)) #calculate 95% confidence interval
quantile(sampleset,probs=c(.005,.995)) #calculate 99% confidence interval




#Plotting 5 charts with 7 lines

a<-c(rep(0, times = 9990), rep(1, times=10)) # vector of observations, 0.1%
a<-c(rep(0, times = 9950), rep(1, times=50)) # vector of observations, 0.5%
a<-c(rep(0, times = 9900), rep(1, times=100)) # vector of observations, 1%
a<-c(rep(0, times = 9500), rep(1, times=500)) # vector of observations, 5%
a<-c(rep(0, times = 9000), rep(1, times=1000)) # vector of observations, 10%
a<-c(rep(0, times = 8500), rep(1, times=1500)) # vector of observations, 15%
a<-c(rep(0, times = 8000), rep(1, times=2000)) # vector of observations, 20%
a<-c(rep(0, times = 7500), rep(1, times=2500)) # vector of observations, 25%
a<-c(rep(0, times = 7000), rep(1, times=3000)) # vector of observations, 30%




# 1 error
# zero out vectors
result<-vector(mode="numeric", length=0)
b<-vector(mode="numeric", length=0)
sampling<-vector(mode="numeric", length=0)



for (j in seq(from=1, to=500, by=25)) { # loop through the sampling strategies
  #zero out positive/negative counters
  negative<-0 
  positive<-0
  print (j)
  sampling<-append(sampling, j)
  
  for (i in 1:10000) { # loop through trials of each sampling strategy  
    
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
plot(sampling, result, xlim=c(0,500), ylim=c(0,1), type="l",xaxp  = c(0, 500, 20))

par(new=TRUE)
plot(sampling, result, xlim=c(0,500), ylim=c(0,1), type="l", col="black", xaxp  = c(0, 500, 20))


# 5 errors
# zero out vectors
result<-vector(mode="numeric", length=0)
b<-vector(mode="numeric", length=0)
sampling<-vector(mode="numeric", length=0)



for (j in seq(from=1, to=500, by=25)) { # loop through the sampling strategies
  #zero out positive/negative counters
  negative<-0 
  positive<-0
  print (j)
  sampling<-append(sampling, j)
  
  for (i in 1:10000) { # loop through trials of each sampling strategy  
    
    b<-sample(a, (j), replace=TRUE) #sampling, make sure to set the "replacement"
    
    if (sum(b)<5) { #what is the minimum of hits for a positive?
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

par(new=TRUE)
plot(sampling, result, xlim=c(0,500), ylim=c(0,1), type="l", col="red", xaxp  = c(0, 500, 20))

# 10 errors
# zero out vectors
result<-vector(mode="numeric", length=0)
b<-vector(mode="numeric", length=0)
sampling<-vector(mode="numeric", length=0)



for (j in seq(from=1, to=500, by=25)) { # loop through the sampling strategies
  #zero out positive/negative counters
  negative<-0 
  positive<-0
  print (j)
  sampling<-append(sampling, j)
  
  for (i in 1:10000) { # loop through trials of each sampling strategy  
    
    b<-sample(a, (j), replace=TRUE) #sampling, make sure to set the "replacement"
    
    if (sum(b)<10) { #what is the minimum of hits for a positive?
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

par(new=TRUE)
plot(sampling, result, xlim=c(0,500), ylim=c(0,1), type="l", col="green", xaxp  = c(0, 500, 20))

# 15 errors
# zero out vectors
result<-vector(mode="numeric", length=0)
b<-vector(mode="numeric", length=0)
sampling<-vector(mode="numeric", length=0)



for (j in seq(from=1, to=500, by=25)) { # loop through the sampling strategies
  #zero out positive/negative counters
  negative<-0 
  positive<-0
  print (j)
  sampling<-append(sampling, j)
  
  for (i in 1:10000) { # loop through trials of each sampling strategy  
    
    b<-sample(a, (j), replace=TRUE) #sampling, make sure to set the "replacement"
    
    if (sum(b)<15) { #what is the minimum of hits for a positive?
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

par(new=TRUE)
plot(sampling, result, xlim=c(0,500), ylim=c(0,1), type="l", col="blue", xaxp  = c(0, 500, 20))

# 20 errors
# zero out vectors
result<-vector(mode="numeric", length=0)
b<-vector(mode="numeric", length=0)
sampling<-vector(mode="numeric", length=0)



for (j in seq(from=1, to=500, by=25)) { # loop through the sampling strategies
  #zero out positive/negative counters
  negative<-0 
  positive<-0
  print (j)
  sampling<-append(sampling, j)
  
  for (i in 1:10000) { # loop through trials of each sampling strategy  
    
    b<-sample(a, (j), replace=TRUE) #sampling, make sure to set the "replacement"
    
    if (sum(b)<20) { #what is the minimum of hits for a positive?
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

par(new=TRUE)
plot(sampling, result, xlim=c(0,500), ylim=c(0,1), type="l", col="magenta", xaxp  = c(0, 500, 20))


# 25 errors
# zero out vectors
result<-vector(mode="numeric", length=0)
b<-vector(mode="numeric", length=0)
sampling<-vector(mode="numeric", length=0)



for (j in seq(from=1, to=500, by=25)) { # loop through the sampling strategies
  #zero out positive/negative counters
  negative<-0 
  positive<-0
  print (j)
  sampling<-append(sampling, j)
  
  for (i in 1:10000) { # loop through trials of each sampling strategy  
    
    b<-sample(a, (j), replace=TRUE) #sampling, make sure to set the "replacement"
    
    if (sum(b)<25) { #what is the minimum of hits for a positive?
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

par(new=TRUE)
plot(sampling, result, xlim=c(0,500), ylim=c(0,1), type="l", col="yellow", xaxp  = c(0, 500, 20))

# 30 errors
# zero out vectors
result<-vector(mode="numeric", length=0)
b<-vector(mode="numeric", length=0)
sampling<-vector(mode="numeric", length=0)



for (j in seq(from=1, to=500, by=25)) { # loop through the sampling strategies
  #zero out positive/negative counters
  negative<-0 
  positive<-0
  print (j)
  sampling<-append(sampling, j)
  
  for (i in 1:10000) { # loop through trials of each sampling strategy  
    
    b<-sample(a, (j), replace=TRUE) #sampling, make sure to set the "replacement"
    
    if (sum(b)<30) { #what is the minimum of hits for a positive?
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

par(new=TRUE)
plot(sampling, result, xlim=c(0,500), ylim=c(0,1), type="l", col="black", xaxp  = c(0, 500, 20))


#plotting 5 boxplots with 6 boxes each
#Plotting 5 charts with 7 lines

a<-c(rep(0, times = 9990), rep(1, times=10)) # vector of observations, 0.1%
a<-c(rep(0, times = 9950), rep(1, times=50)) # vector of observations, 0.5%
a<-c(rep(0, times = 9900), rep(1, times=100)) # vector of observations, 1%
a<-c(rep(0, times = 9500), rep(1, times=500)) # vector of observations, 5%
a<-c(rep(0, times = 9000), rep(1, times=1000)) # vector of observations, 10%
a<-c(rep(0, times = 8500), rep(1, times=1500)) # vector of observations, 15%
a<-c(rep(0, times = 8000), rep(1, times=2000)) # vector of observations, 20%
a<-c(rep(0, times = 7500), rep(1, times=2500)) # vector of observations, 25%
a<-c(rep(0, times = 7000), rep(1, times=3000)) # vector of observations, 30%


# zero out vectors for sampling strategies (10, 50, 100, 500, 1000, 5000 samples)
m25<-numeric()
m50<-numeric()
m75<-numeric()
m100<-numeric()
m125<-numeric()
m150<-numeric()

# loop through sampling strategies (Monte Carlo trials)
# in a binary dataset with certain % of "1", how well do we represent the whole (mean) with each sampling strategy?
for (i in 1:10000) {  
  m25<-append(m25, 100*mean(sample(a, 25)))
  m50<-append(m50, 100*mean(sample(a, 50)))
  m75<-append(m75, 100*mean(sample(a, 75)))
  m100<-append(m100, 100*mean(sample(a, 100)))
  m125<-append(m125, 100*mean(sample(a, 125)))
  m150<-append(m150, 100*mean(sample(a, 150)))
  
}


labelsx<-c("25","50","75","100","125","150")
data<-data.frame(m25, m50, m75, m100, m125, m150)
colnames(data)<-labelsx

boxplot(data)

