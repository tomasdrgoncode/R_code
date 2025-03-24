data<-read.table("lc43hours.txt")

sums<-c(sum(data$LC1), sum(data$LC2), sum(data$LC3), sum(data$LC4), sum(data$LC5), sum(data$LCX))

barplot(sums, names.arg=colnames(data))

#data$nonLC1<-data$LC2 + data$LC3 + data$LC4 + data$LC5 + data$LCX

data<-mat.sort(data,3, decreasing=TRUE)

barplot(data$LC3, names.arg=rownames(data), cex.names=0.7, las=3)

distances<-dist(data)
plot(hclust(distances))


fit <- princomp(data)
summary(fit) # print variance accounted for
loadings(fit) # pc loadings
plot(fit,type="lines") # scree plot
fit$scores # the principal components
biplot(fit)



data<-read.table("lc41hours.txt")

sums<-c(sum(data$LC1), sum(data$LC2), sum(data$LC3), sum(data$LC4), sum(data$LC5))

barplot(sums, names.arg=colnames(data), ylim=c(1,150000))

data<-mat.sort(data,4, decreasing=TRUE)
data<-subset(data, data$LC4>100)

barplot(data$LC4, names.arg=rownames(data), cex.names=0.7, las=3)

distances<-dist(data)
plot(hclust(distances))