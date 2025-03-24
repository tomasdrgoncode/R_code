# TOS model


s<-rnorm(10000, mean=20, sd=10)# for bulk material with mostly even distribution

#or data from real distributions
x<-c(309.01, 320.83, 276.48, 300.42, 226.24, 269.42, 304.89, 264.81, 311.24, 279.30, 227.92, 306.10, 296.29, 202.88, 265.34, 305.27, 332.71, 216.61, 285.97, 294.24, 249.68, 311.64, 292.13, 235.74, 345.57, 269.37, 237.44, 248.15, 243.71, 210.34, 223.61, 244.54, 330.26, 289.37, 270.20, 272.15, 338.92, 262.30, 223.28, 236.82, 274.25, 255.85, 268.72, 279.97, 191.14, 307.23, 275.48, 230.70)
meanx<-mean(x)
sdx<-sd(x) #calculated sd from data

hist(x)

#Or from known mean and SD
meanx<-271.14
sdx<-37.3 

s<-rnorm(10000, mean=(meanx), sd=(sdx))# for bulk material with mostly even distribution
s<-abs(s)

#see how what percent under/over threshold
s<-data.frame(s)
length(which(s$s >300))/length(s$s)*100

s<-s$s


#calculation for one sampling strategy

samp<-vector(mode="numeric", length=0)

for (i in 1:1000){
  sa<-sample(s, 48) # number of increments taken
  sam<-mean(sa)
  samp<-append(samp,sam)
}

sd<-sd(samp) #this is a SD of the mean estimates, not a SD of the whole distribution...

bx.p<-boxplot(samp)
bxp(bx.p,pars=list(ylim=c(0,max(samp))))#lets me use the ylim on a boxplot

hist(samp)
quantile(samp,probs=c(.025,.975)) #calculate 95% confidence interval
quantile(samp,probs=c(.005,.995)) #calculate 99% confidence interval

#for sdx if we are working from a distribution defined with meanx and sdx
mean(s)
sdx
sdx/mean(s)*100


#charts with multiple sampling strategies

# TOS model

volume<-c(6, 12, 24, 36, 48, 60, 72, 84, 96)
FSE<-vector(mode="numeric", length=0)

#hist(s)

samp<-vector(mode="numeric", length=0)

for (i in 1:1000){
  sa<-sample(s, 6)
  sam<-mean(sa)
  samp<-append(samp,sam)
}

samp6<-samp

samp<-vector(mode="numeric", length=0)

for (i in 1:1000){
  sa<-sample(s, 12)
  sam<-mean(sa)
  samp<-append(samp,sam)
}

samp12<-samp

samp<-vector(mode="numeric", length=0)

for (i in 1:1000){
  sa<-sample(s, 24)
  sam<-mean(sa)
  samp<-append(samp,sam)
}

samp24<-samp

samp<-vector(mode="numeric", length=0)

for (i in 1:1000){
  sa<-sample(s, 36)
  sam<-mean(sa)
  samp<-append(samp,sam)
}

samp36<-samp

samp<-vector(mode="numeric", length=0)

for (i in 1:1000){
  sa<-sample(s, 48)
  sam<-mean(sa)
  samp<-append(samp,sam)
}

samp48<-samp

samp<-vector(mode="numeric", length=0)

for (i in 1:1000){
  sa<-sample(s, 60)
  sam<-mean(sa)
  samp<-append(samp,sam)
}

samp60<-samp

samp<-vector(mode="numeric", length=0)

for (i in 1:1000){
  sa<-sample(s, 72)
  sam<-mean(sa)
  samp<-append(samp,sam)
}

samp72<-samp

samp<-vector(mode="numeric", length=0)

for (i in 1:1000){
  sa<-sample(s, 84)
  sam<-mean(sa)
  samp<-append(samp,sam)
}

samp84<-samp

samp<-vector(mode="numeric", length=0)

for (i in 1:1000){
  sa<-sample(s, 96)
  sam<-mean(sa)
  samp<-append(samp,sam)
}

samp96<-samp


sd6<-sd(samp6)
sd12<-sd(samp12)
sd24<-sd(samp24)
sd36<-sd(samp36)
sd48<-sd(samp48)
sd60<-sd(samp60)
sd72<-sd(samp72)
sd84<-sd(samp84)
sd96<-sd(samp96)


sd
sd<-c(sd6,sd12,sd24,sd36,sd48,sd60,sd72,sd84, sd96)
FSE<-(sd/(mean(s)))*100

plot(volume, sd, type="b")
plot(volume, FSE, type="b")

data<-data.frame(samp6, samp12, samp24, samp36, samp48, samp60, samp72, samp84, samp96)

bx.p<-boxplot(data, names=volume)
bxp(bx.p,pars=list(ylim=c(0,max(samp6))))#lets me use the ylim on a boxplot




boxplot(samp6, samp12, samp24, samp36, samp48, samp60, samp72, samp84, samp96, names=volume)
result<-vector(mode="numeric", length=0)

