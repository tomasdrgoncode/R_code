x<-c(1:1000)

xsqrt<-sqrt(x)
oneover_xsqrt<-(1/xsqrt)

plot(x, type="l")

plot(xsqrt,type="l")
lines(x)

plot(oneover_xsqrt, type="l")