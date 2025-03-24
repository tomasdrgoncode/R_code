data<-read.csv("Done_vs_plan.csv", header=TRUE)


sdata<-subset(data, PlanFY=="2015") 
  
  
plan<-sdata$Plan_Hours
accomplished<-sdata$Sum_of_Hours
diff<-plan-accomplished

plot(plan,accomplished)#, xlim=c(0,10), ylim=c(0,10))

fit <- glm(accomplished~plan)
co <- coef(fit)
abline(fit)

hist(diff, breaks = 1000)#, xlim=c(-2000,2000))
cor(plan, accomplished)

sumplan<-sum(plan)
sumaccomplished<-sum(accomplished)
FTE<-(sumplan-sumaccomplished)/(40*52)
FTE

#by year

data<-read.csv("Done_vs_plan_by_year.csv", header=TRUE)

#how well does past ACC predict future ACC

x<-data$ACC2014
y<-data$ACC2015
z<-data$PLAN2015

data1<-data.frame(x,y,z)
data1<-na.omit(data1)

par(mfrow=c(1,2))

plot(data1$x,data1$y, xlim=c(0, 10000), ylim=c(0, 10000))
fit <- glm(data1$y~data1$x)
co <- coef(fit)
abline(fit)
cor(data1$x,data1$y)

plot(data1$z,data1$y, xlim=c(0, 10000), ylim=c(0, 10000))
fit <- glm(data1$y~data1$z)
co <- coef(fit)
abline(fit)
cor(data1$z, data1$y)



#general workload growth, except for 2011 that is weird
acc<-c(sum(data$ACC2012, na.rm=TRUE),
       sum(data$ACC2013, na.rm=TRUE),
       sum(data$ACC2014, na.rm=TRUE),
       sum(data$ACC2015, na.rm=TRUE))
FY<-c("2012", "2013", "2014", "2015")

barplot(acc, names.arg=FY, ylim=c(0, 1000000))



#difference between total volume of work wityh mean and median

data_n<-rnorm(1000, mean=10, sd=1) #normal dist
data_nn<-(data_n^8)/1000000000 #non-normal dist

hist(data_n, xlim=c(0, max(data_n)))
hist(data_nn, xlim=c(0, max(data_nn)))


mean_n<-mean(data_n)
median_n<-median(data_n)
mean_nn<-mean(data_nn)
median_nn<-median(data_nn)
load_n<-sum(data_n)
load_nn<-sum(data_nn)
plan_n<-median_n*1000
plan_nn<-median_nn*1000
pplan_n<-plan_n/load_n
pplan_nn<-plan_nn/load_nn
module_load_nn<-load_nn/median_nn
module_load_nn
module_plan_nn<-median_nn*module_load_nn
mpplan_nn<-module_plan_nn/load_nn

data_all<-c(pplan_n, pplan_nn, mpplan_nn)
barplot(data_all)

load_n
plan_n
load_nn
plan_nn
module_plan_nn


