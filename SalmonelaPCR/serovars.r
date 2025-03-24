data<-read.table("serovars.txt")
data1<-data$V1
data2<-unique(data1)
write(data2, "serovars1.txt")