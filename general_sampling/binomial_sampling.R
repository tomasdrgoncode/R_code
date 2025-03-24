#sampling from 0 and 1

result<-vector(mode="numeric", length=0)

for (j in 0:100) {   
  a<-sum(sample(c(0,1), 10, replace = TRUE))
  result<-append(result,a)
  
}

hist(result)

resultn<-rnorm(1000)

shapiro.test(result)

qqplot(result, resultn)
