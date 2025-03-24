#question: how many employees to survey to get accurate leave estimates
#assumption: 212 hrs on average (what SD? 10%?)


#or data from real distributions
x<-c(x,y,z) #sample of known leave hrs from employees)
meanx<-mean(x)
sdx<-sd(x)




#or use best estimate
meanx<-268
#sdx<-meanx*0.01 #sd=x% of mean
sdx<-108 #actual SD from data
trials<-500 #number of monte carlo trials

sampling<-c(5, 10, 50, 100, 200) #number of samples taken
FSE<-vector(mode="numeric", length=0)
result<-vector(mode="numeric", length=0)

s<-rnorm(1000, mean=(meanx), sd=(sdx))# for a set of employees with mostly even distribution of leave taken
s<-abs(s)

hist(s, xlim=c(0, max(s)))

samp<-vector(mode="numeric", length=0)

for (i in 1:trials){
  sa<-sample(s, 5)
  sam<-mean(sa)
  samp<-append(samp,sam)
}

samp5<-samp

samp<-vector(mode="numeric", length=0)

for (i in 1:trials){
  sa<-sample(s, 10)
  sam<-mean(sa)
  samp<-append(samp,sam)
}

samp10<-samp

samp<-vector(mode="numeric", length=0)

for (i in 1:trials){
  sa<-sample(s, 50)
  sam<-mean(sa)
  samp<-append(samp,sam)
}

samp50<-samp

samp<-vector(mode="numeric", length=0)

for (i in 1:trials){
  sa<-sample(s, 100)
  sam<-mean(sa)
  samp<-append(samp,sam)
}

samp100<-samp

samp<-vector(mode="numeric", length=0)

for (i in 1:trials){
  sa<-sample(s, 500)
  sam<-mean(sa)
  samp<-append(samp,sam)
}

samp200<-samp

samp<-vector(mode="numeric", length=0)


sd5<-sd(samp5)
sd10<-sd(samp10)
sd50<-sd(samp50)
sd100<-sd(samp100)
sd200<-sd(samp200)


sd<-c(sd5,sd10,sd50,sd100,sd200)
FSE<-(sd/(mean(s)))*100

hist(s, xlim=c(0,max(s)))

plot(sampling, sd, type="b")
plot(sampling, FSE, type="b")
lines(sampling, FSE, type="b",col="green")



data<-data.frame(samp5, samp10, samp50, samp100, samp200)

bx.p<-boxplot(data)
bxp(bx.p,pars=list(ylim=c(0,max(samp5))))#lets me use the ylim on a boxplot

