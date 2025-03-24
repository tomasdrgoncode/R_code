data<-read.csv("total_lines_per_FEI_2016.csv", header=TRUE)

plot(data$Lines,data$Value)

qqplot(data$Lines,data$Value)

plot(data$Lines,data$Value, xlim = c(0,1000), ylim = c(0,20))


