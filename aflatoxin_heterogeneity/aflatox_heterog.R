# TOS model

#data

s<-c(rep(0, times=9000), rep(1, times=1000)) #sample with 10% nuggets (such as apergillus growth clumps)
s<-rnorm(10000, mean=20, sd=10)# for bulk material with mostly even distribution

#or data from real distributions
x<-c(830, 2655)
meanx<-mean(x)
sdx<-sd(x) #calculated sd from data

#Or from known mean and SD
meanx<-1772
sdx<-300 

s<-rnorm(26000, mean=(meanx), sd=(sdx))# for bulk material with mostly even distribution
s<-abs(s)

#see how many over threshold
s<-data.frame(s)
length(which(s$s < 1000))/length(s$s)*100

#calculation for one sampling strategy

samp<-vector(mode="numeric", length=0)

for (i in 1:1000){
  sa<-sample(s, 40) # number of increments taken
  sam<-mean(sa)
  samp<-append(samp,sam)
}

sd<-sd(samp) #this is a SD of the mean estimates, not a SD of the whole distribution...

bx.p<-boxplot(samp)
bxp(bx.p,pars=list(ylim=c(0,max(samp))))#lets me use the ylim on a boxplot

hist(samp)
quantile(samp,probs=c(.025,.975)) #calculate 95% confidence interval
quantile(samp,probs=c(.005,.995)) #calculate 99% confidence interval

mean(s)
sdx
sdx/mean(s)*100


#charts with multiple sampling strategies

# TOS model

volume<-c(4, 10, 50, 100, 500, 1000, 5000)
FSE<-vector(mode="numeric", length=0)

#hist(s)

samp<-vector(mode="numeric", length=0)

for (i in 1:1000){
  sa<-sample(s, 4)
  sam<-mean(sa)
  samp<-append(samp,sam)
}

samp4<-samp

samp<-vector(mode="numeric", length=0)

for (i in 1:1000){
  sa<-sample(s, 10)
  sam<-mean(sa)
  samp<-append(samp,sam)
}

samp10<-samp

samp<-vector(mode="numeric", length=0)

for (i in 1:1000){
  sa<-sample(s, 50)
  sam<-mean(sa)
  samp<-append(samp,sam)
}

samp50<-samp

samp<-vector(mode="numeric", length=0)

for (i in 1:1000){
  sa<-sample(s, 100)
  sam<-mean(sa)
  samp<-append(samp,sam)
}

samp100<-samp

samp<-vector(mode="numeric", length=0)

for (i in 1:1000){
  sa<-sample(s, 500)
  sam<-mean(sa)
  samp<-append(samp,sam)
}

samp500<-samp

samp<-vector(mode="numeric", length=0)

for (i in 1:1000){
  sa<-sample(s, 1000)
  sam<-mean(sa)
  samp<-append(samp,sam)
}

samp1000<-samp

samp<-vector(mode="numeric", length=0)

for (i in 1:1000){
  sa<-sample(s, 5000)
  sam<-mean(sa)
  samp<-append(samp,sam)
}

samp5000<-samp

sd4<-sd(samp4)
sd10<-sd(samp10)
sd50<-sd(samp50)
sd100<-sd(samp100)
sd500<-sd(samp500)
sd1000<-sd(samp1000)
sd5000<-sd(samp5000)

sd<-c(sd4,sd10,sd50,sd100,sd500,sd1000,sd5000)
FSE<-(sd/(mean(s)))*100

plot(volume, sd, type="b")
plot(volume, FSE, type="b")

data<-data.frame(samp4, samp10, samp50, samp100, samp500, samp1000, samp5000)

bx.p<-boxplot(data, names=volume)
bxp(bx.p,pars=list(ylim=c(0,max(samp4))))#lets me use the ylim on a boxplot




boxplot(samp10, samp50, samp100, samp500, samp1000, samp5000, names=volume)
result<-vector(mode="numeric", length=0)

