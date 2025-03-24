#other packages:
library(PracTools)
library(powerAnalysis)


#this below is from the base stats package:
# calculates whichever parameter is left at NULL or omitted


#ttest
power.t.test(sd=1, sig.level = 0.05, power=0.8, delta=0.1, alternative="two.sided", type="two.sample")

#ANOVA
groupmeans <- c(10, 11, 12, 13)
power.anova.test(groups = length(groupmeans), 
                 between.var = var(groupmeans), within.var = 10, 
                 power=0.8,sig.level=0.05,n=NULL)


#power plot (t-test)
#access elements of the power.t.test object through $

x<-c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
result<-vector(mode = "numeric")


for (i in x) {

  print(i)
  
  powerx<-power.t.test(sd=1, sig.level = 0.1, power=(i), delta=0.1, alternative="two.sided", type="two.sample")
  result<-append(result, powerx$n)



}

plot(result,x, type="b")


library(powerAnalysis)







