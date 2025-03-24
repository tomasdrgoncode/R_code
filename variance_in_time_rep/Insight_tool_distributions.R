data<-read.csv("Module_Time_Insight_V1.csv", header=TRUE)
data<-data$Sum.of.Hours #extracts the sum of hours column
data<-data[-1] #removes first value which is the sum in some of the downloads
hist(data, breaks=50)
mean(data)
median(data)
sd(data)

boxplot(data)
quantile(data) #all 4 quartiles

#specific quartiles
Q1 <- quantile(data, 0.025) 
Q2 <- quantile(data, 0.975)  
Q1
Q2

plot(density(data))

data1<-rnorm(1000,40,4)
hist(data1, xlim=c(0,max(data1)))