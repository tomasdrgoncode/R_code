
n<-100

subject1<-sample(c(0,1),n, replace=TRUE)
subject2<-sample(c(0,1),n, replace=TRUE)
occ1<-sample(c(0,1),n, replace=TRUE)
occ2<-sample(c(0,1),n, replace=TRUE)
occ3<-sample(c(0,1),n, replace=TRUE)
occ4<-sample(c(0,1),n, replace=TRUE)
occ5<-sample(c(0,1),n, replace=TRUE)
occ6<-sample(c(0,1),n, replace=TRUE)
occ7<-sample(c(0,1),n, replace=TRUE)
occ8<-sample(c(0,1),n, replace=TRUE)
occ9<-sample(c(0,1),n, replace=TRUE)
occ10<-sample(c(0,1),n, replace=TRUE)

data<-data.frame(subject1,subject2, occ1, occ2, occ3, occ4, occ5, occ6, occ7, occ8, occ9, occ10)

subject1<-rnorm(n,10,1)
subject2<-rnorm(n,10,1)
occ1<-rnorm(n,10,1)
occ2<-rnorm(n,10,1)
occ3<-rnorm(n,10,1)
occ4<-rnorm(n,10,1)
occ5<-rnorm(n,10,1)
occ6<-rnorm(n,10,1)
occ7<-rnorm(n,10,1)
occ8<-rnorm(n,10,1)
occ9<-rnorm(n,10,1)
occ10<-rnorm(n,10,1)

data<-data.frame(subject1,subject2, occ1, occ2, occ3, occ4, occ5, occ6, occ7, occ8, occ9, occ10)


heatmap(as.matrix(data))

plot(hclust(dist(t(as.matrix(data)))))

biplot(prcomp(t(data)),var.axes=FALSE)
