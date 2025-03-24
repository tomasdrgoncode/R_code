x<-rbeta(10000, 5, 5)

plot(density(x), xlim=c(0,1))

hist(x)

quantile(x,probs=c(.025,.975)) #calculate 95% confidence interval
quantile(x,probs=c(.005,.995)) #calculate 99% confidence interval
