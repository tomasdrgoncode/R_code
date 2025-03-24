x<-read.table("zwietering.txt")

#fit the distribution with fitdist
library("fitdistrplus", lib.loc="~/R/win-library/3.3")


#decide which distribution with descdist


descdist(x$V1, discrete = FALSE)

#fit gamma

fitg<-fitdist(x$V1 ,"gamma", method="mme")

plot(fitg)



#fit gamma and collect parameters


fitg<-fitdist(x$V1 ,"gamma", method="mme")

shapeg=fitg[1]$estimate[1] 
scaleg=1/fitg[1]$estimate[2]
rateg<-1/scaleg
model<-rgamma(10000, shapeg, rateg)
plot(density(model), xlim=c(0,200))




