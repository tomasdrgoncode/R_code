data<-read.table("production.txt", header=TRUE)
plot(data$year, data$all, type="b", ylim=c(0, 22000))

par(new=T)
plot(data$year, data$clams, type="b", ylim=c(0, 22000))

par(new=T)
plot(data$year, data$mussels, type="b", ylim=c(0, 22000))

par(new=T)
plot(data$year, data$oyster, type="b", ylim=c(0, 22000))

