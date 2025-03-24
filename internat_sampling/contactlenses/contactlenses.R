x<-seq(from = 1, to = 9, by = 0.1)
y<-1/x + 1/x^2

plot (x,y, type="l", ylim=c(0,1))

par(new=TRUE)

y<-(1/x+ 1/x^2)*2 

plot (x,y, type="l", col="red", ylim=c(0,1))

par(new=TRUE)

y<-(1/x+ 1/x^2)*3 

plot (x,y, type="l", col="green", ylim=c(0,1))