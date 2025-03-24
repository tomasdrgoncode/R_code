data<-read.table("shilpa07032017peraccomplishment.txt")

hist(data$time, breaks =10)
mean(data$time)
sd(data$time)
median(data$time)