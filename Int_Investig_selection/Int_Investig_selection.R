# establish the # of available inspectors in region

pa<-(paste(seq(1:10), "pa", sep="")) #pacific pool
ce<-(paste(seq(1:40), "ce", sep="")) #central pool
ne<-(paste(seq(1:60), "ne", sep="")) #northeast pool
sw<-(paste(seq(1:80), "sw", sep="")) #southwest pool
se<-(paste(seq(1:100), "se", sep="")) #southeast pool

#selection by region

k<-vector(mode="numeric", length=0)

for (i in seq(1:100)) {
  print (i)
    k<-append(k,sample(pa, 1))
    k<-append(k, sample(ce, 1))
    k<-append(k, sample(ne, 1))
    k<-append(k, sample(sw, 1))
    k<-append(k, sample(se, 1))
} 

data<-table(k) #tabulate frequency of selection
plot(data)

data<-as.data.frame(data) #extract frequency of selection
hist(data$Freq)




#random selection
all<-c(pa,ce,ne,sw,se)

l<-vector(mode="numeric", length=0)

for (i in seq(1:500)) {
  print (i)
  l<-append(l,sample(all, 1))
  
}

data<-table(l)
plot(data)

data<-as.data.frame(data)
hist(data$Freq)

