#Whitaker lots 2
#replicated test results from 10 5.45 kg samples from 29 lots of shelled peanuts
lots<-read.table("whitaker_lots_2.txt", header=TRUE)
lots<-lots[,-1] #drop the row names column
lots<-as.matrix(lots)
lots<-as.data.frame(lots)

means<-lapply(lots, mean)
medians<-lapply(lots, median)

deviations<-lapply(lots, sd)
variance<-lapply(lots, var)


#fit the distribution with fitdist
library("fitdistrplus", lib.loc="~/R/win-library/3.3")


#decide which distribution with descdist

for (j in 1:29) {
  
descdist(lots[[j]], discrete = FALSE)
}

#fit gamma

for (k in 1:29) {

fitg<-fitdist(lots[[k]] ,"gamma", method="mme")

plot(fitg)

}


#fit gamma and collect parameters

shape<-vector(mode="numeric", length=0)
rate<-vector(mode="numeric", length=0)


for (i in 1:29) {

fitg<-fitdist(lots[[i]] ,"gamma", method="mme")

shapeg=fitg[1]$estimate[1] 
scaleg=1/fitg[1]$estimate[2]
rateg<-1/scaleg
model<-rgamma(10000, shapeg, rateg)
plot(density(model), xlim=c(0,200))


shape<-append(shape,shapeg)
rate<-append(rate, rateg)

}

  
#shape parameters (lots 2)
sh<-as.vector(shape)
rate<-as.vector(rate)

means<-unlist(means)
medians<-unlist(medians)

means<-as.vector(means)
medians<-as.vector(medians)

reg<-lm(means ~ sh) #lm
coeff=coefficients(reg)
corr<-cor(means, sh)
eq = paste0("Means vs Shape ", "     correlation = ", round(corr, 2))
plot(sh, means, main=eq)
abline(reg)

reg<-lm(medians ~ sh) #lm
coeff=coefficients(reg)
corr<-cor(medians, sh)
eq = paste0("Medians vs Shape ", "     correlation = ", round(corr, 2))
plot(sh, medians, main=eq)
abline(reg)

#correlation btw shape and rate

reg<-lm(rate ~ shape) #lm
coeff=coefficients(reg)
corr<-cor(rate, shape)
eq = paste0("rate vs Shape ", "     correlation = ", round(corr, 2))
plot(shape, rate, main=eq)
abline(reg)


boxplot(rate)
mean(rate)
median(rate)
sd(rate)

#how about the correlation of Rate w central tendency?

plot(rate, means)
plot(rate, medians)


#approximate negative binomial dist???

test<-rnbinom(10000, 1, 0.5)

descdist(test, discrete = FALSE)

k<-read.table("whitaker_k.txt", header=FALSE)
k<-unlist(k)
k<-as.vector(k)

reg<-lm(k ~ sh) #lm
coeff=coefficients(reg)
corr<-cor(k, sh)
eq = paste0("NegBin k vs gamma shape ", "     correlation = ", round(corr, 2))
plot(sh, k, main=eq, xlab="gamma shape parameter", ylab="NegBin k parameter")
abline(reg)

