#variance analysis

data<-read.table("d5.txt", header=TRUE, row.names=1) #data with "total"

data<-data[c(1,2,3,4,5,6,7)] #data without "rank"
data<-data[c(1,2,3,4,5,6)] #data without "total" and "rank"
data<-data[c(2,3,4,5,6,7,8)] #data without document name

d<-dist(as.matrix(data))
plot(hclust(d), cex=0.5)


#overall sums of squares, not sure if correct...
(sum(data$BCI-mean(data$BCI))^2)
names<-c("PHR","SA", "BI","BPI","PRR","BCI")
ssq<-c(1.687423e-29, 3.993608e-30, 2.300318e-27, 9.984021e-31, 1.919772e-26, 8.28797e-29)
ssq<-ssq/sum(ssq)*100
ssq
df<-data.frame(names,ssq)
df<-df[order(-df$ssq),]

#PCA approach to variance

biplot(princomp(as.matrix(data)))
loadings(princomp(as.matrix(data)))
plot(princomp(as.matrix(data)))

components<-(prcomp(as.matrix(data)))
print(prcomp(as.matrix(data)))
plot(prcomp(as.matrix(data)))

components<-read.table("components.txt", header=TRUE, row.names=1)

components<-components[order(-abs(components$PC1)),]
names<-c("SA", "BCI", "BPI", "BI", "PRR", "PHR")
percent<-abs(components$PC1)/sum(abs(components$PC1))*100
barplot(cumsum(percent), names.arg=names)

components<-components[order(-abs(components$PC2)),]
names<-c("BI", "SA", "PRR", "BPI", "BCI", "PHR")
percent<-abs(components$PC2)/sum(abs(components$PC2))*100
barplot(cumsum(percent), names.arg=names)



#Anova approach to variance

fit<-aov(total~PHR+SA+BI+BPI+PRR+BCI,data )

names<-c("PHR","SA", "BI","BPI", "PRR", "BCI", "Rest")
ssquares<-c(3591.983, 10614.890, 3049.559, 1022.514, 946.547, 1079.038, 0.00)
percent<-ssquares/sum(ssquares)*100

df<-data.frame(names,percent)
df<-df[order(-df$percent),]
barplot(df$percent, names.arg=df$names)
barplot(cumsum(df$percent), names.arg=df$names)


#ranking totals

data<-read.table("D5.txt", header=TRUE)
hist(data$total)

Document_type<-data$DocumentType
Document_type<-gsub('[0-9]+', '', Document_type) #remove numbers
data<-data.frame(Document_type, data$total, data$rank)


#subset data for plotting

CPG<-data[which(data$Document_type=='CPG'),]
CPGM<-data[which(data$Document_type=='CPGM'),]
IOM<-data[which(data$Document_type=='IOM'),]
RPM<-data[which(data$Document_type=='RPM'),]

plot(data$data.total, data$data.rank)

plot(CPG$data.total, CPG$data.rank, xlim=c(0,100), ylim=c(0,110))
par(new = TRUE)
plot(CPGM$data.total,CPGM$data.rank, xlim=c(0,100), ylim=c(0,110), col="red")
par(new = TRUE)
plot(RPM$data.total, RPM$data.rank, xlim=c(0,100), ylim=c(0,110), col="green")
par(new = TRUE)
plot(IOM$data.total, IOM$data.rank, xlim=c(0,100), ylim=c(0,110), col="blue")


#bayesian

library(bnlearn)
data(learning.test)

res = gs(learning.test)



