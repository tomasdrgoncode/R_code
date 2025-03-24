
#population of 16 million lines with a proportion of errors
s<-rnorm(16000000, 2.975) #to generate normal dist, mean = error rate
s<-round(s) #round to get integers
mean (s) #check to make sure the mean is correct
length (s) # check to make sure the length is correct

#ACS 120 lines with proportion of errors
s<-c(rep(0, times=95900), rep(1, times=4100))
samp<-vector(mode="numeric", length=0)

mean (s) #check to make sure the mean is correct
length (s) # check to make sure the length is correct

# sample distribution and calculate mean of sample
for (i in 1:1000){
  sa<-sample(s, 120)
  sam<-mean(sa)
  samp<-append(samp,sam)
}

mean(samp)
sd(samp)

quantile(samp,probs=c(.025,.975)) #calculate 95% confidence interval
quantile(samp,probs=c(.005,.995)) #calculate 99% confidence interval

boxplot(samp, ylim=c(0, max(samp)))
