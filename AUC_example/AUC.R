
#random assignment
response<-sample(c(0,1), 1000, replace = TRUE)
predictor<-sample(c(0,1), 1000, replace = TRUE)

plot(cumsum(response),cumsum(predictor))

#classifier result

response1<-c(rep (0, times=500),rep(1, times=500))
response1<-c(rep(0, times=200),rep(c(1,0), times=300), rep(1, times=200))
predictor1<-c(sample(c(0,0,0,1), 500, replace = TRUE), sample(c(0,1,1,1), 500, replace = TRUE))

plot(cumsum(response1),cumsum(predictor1))
barplot(response1)
barplot(predictor1)

library(pROC)

roc(response1,predictor1)
plot(roc(response1,predictor1, AUC=TRUE, ci=TRUE))

