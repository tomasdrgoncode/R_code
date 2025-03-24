# TOS model

volume<-c(10, 50, 100, 500, 1000, 5000)
FSE<-vector(mode="numeric", length=0)


s<-c(rep(0, times=9000), rep(1, times=1000)) #sample with 10% nuggets (such as apergillus growth clumps)
s<-rnorm(10000, mean=20, sd=10)# for bulk material with mostly even distribution
s<-sample(s)
#afla<-c(rep(0, times=2040), rep(0, times=368), rep(0, times=375), 0.9009, 1.6943, 3.2938, 7.19, 11.7412, 21.362, 40.0692, 83.57, 1067, 2, 3.8, 6.52, 10.84, 211, 2.6, 10.1, 62.3, 920)
afla<-c(0.9009, 1.6943, 3.2938, 7.19, 11.7412, 21.362, 40.0692, 83.57, 1067, 2, 3.8, 6.52, 10.84, 211, 2.6, 10.1, 62.3, 920)
hist(afla)

#or data from real distributions
x<-c(4.54, 5.09)
meanx<-mean(x)
#sdx<-sd(x)
sdx<-0.5

s<-rnorm(50000, mean=(meanx), sd=(sdx))# for bulk material with mostly even distribution
#s<-abs(s)

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

sd10<-sd(samp10)
sd50<-sd(samp50)
sd100<-sd(samp100)
sd500<-sd(samp500)
sd1000<-sd(samp1000)
sd5000<-sd(samp5000)

sd<-c(sd10,sd50,sd100,sd500,sd1000,sd5000)
FSE<-(sd/(mean(s)))*100

plot(volume, sd, type="b")
plot(volume, FSE, type="b")

data<-data.frame(samp10, samp50, samp100, samp500, samp1000, samp5000)

bx.p<-boxplot(data)
bxp(bx.p,pars=list(ylim=c(0,max(samp10))))#lets me use the ylim on a boxplot




boxplot(samp10, samp50, samp100, samp500, samp1000, samp5000, names=volume)
result<-vector(mode="numeric", length=0)


#probability of catching stuff in bulk material with heterogeneity = SD

#Aflatoxins in cracked corn feed
x<-c(29.03, 51.72, 42.39)
meanx<-mean(x)
scx<-sd(x)

s<-rnorm(40000, mean=(mean(x)), sd=(sd(x)))# for bulk material with mostly even distribution
s<-s*0.9
result<-vector(mode="numeric", length=0)
sampling<-vector(mode="numeric", length=0)



#for (j in seq(from=1, to=1000, by=100)) { # loop through the sampling strategies, j = sampling volume in units
for (j in c(1, 3, 5, 10, 20, 30, 40, 50, 100)) { 
  print (j)
  sampling<-append(sampling, j)
  negative<-0
  positive<-0
  for (i in 1:10000) { # loop through trials of each sampling strategy  
    
    sa<-sample(s, (j)) #take a j-sized sample from s
    
    if (mean(sa)<20) { #this is the regulatory threshold, mean over this is a hit, mean under this is a miss
      negative<-negative+1
    }
    
    else {
      positive<-positive+1
    } 
    
  }
  
  result<-append(result, (negative/(positive+negative))) 
}

print(result)

#hist(s)
#par(new=TRUE) 
plot(sampling, result, type="b", ylim=c(0,0.05))
#plot(sampling, result, type="b", ylim=c(0,1), col="black")


#A mix of clean and dirty spots (say the fungus grows and is minimally disturbed)

x<-c(29.03, 51.72, 42.39)
meanx<-mean(x)
scx<-sd(x)

s<-c((rnorm(30000, mean=(mean(x)), sd=(sd(x)))), rep(0, times=10000))


