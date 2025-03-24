#describe distribution by skewness and kurtosis
library("moments", lib.loc="~/R/win-library/3.3")

a<-rnorm (100000, 0, 1)
a<-rnorm (1000, 100, 2)

a<-rgamma(1000, 0.4656, 0.03)
mean(a)

hist(a, breaks=20)
kurtosis(a)
skewness(a)



#which distribution fits best?
library("fitdistrplus", lib.loc="~/R/win-library/3.3")
descdist(a, discrete = FALSE, boot=500) 

#fit a gamma distribution
fitg<-fitdist(a, "gamma", method="mme")
plot(fitg)


#negative binomial
rnbinom(n, size, prob, mu)

hist(rnbinom(1000, 2, 0.5))