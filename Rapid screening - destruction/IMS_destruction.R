#IMS data for destruction
sib<-1-rbeta(10000, 107, 18)
mean(sib)
quantile(sib,probs=c(.025,.975)) #calculate 95% confidence interval
plot(density(sib), xlim=c(0,1))

phe<-1-rbeta(10000, 27, 18)
mean(phe)
quantile(phe,probs=c(.025,.975)) #calculate 95% confidence interval
plot(density(phe), xlim=c(0,1))

sil<-1-rbeta(10000, 262, 12)
mean(sil)
quantile(sil,probs=c(.025,.975)) #calculate 95% confidence interval
plot(density(sil), xlim=c(0,1))