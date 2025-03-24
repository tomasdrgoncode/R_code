

#exposure models
x<-c(1:100)
y<-x/(1+x)
yy<-x/(5+x)
yyy<-x/(10+x)
z<- vector(mode="numeric", length=0)

#to plot the different shapes of the exposure function
plot(x,y,ylim=c(0,1), type="l")
par(new=T)
lines(x,yy, col=2)
par(new=T)
lines(x,yyy, col=3)
par(new=F)




#very general HAF risk model

# generate and plot a risk surface
x<-c(1:100)
y<-c(1:100)
z<- vector(mode="numeric", length=0)

for (i in x){
  for (j in y){
    z<-append(z,((x[i]) + (y[j]))) #risk function goes here 
 }
}

#normalize z to 0-100
z<-z/max(z)
z<-100-(z*100)

z<-matrix(data=(z),nrow=100, ncol=100)

#persp(x,y,z, col="red", theta=30, phi=15)
#persp3D(z = z, theta=20, phi=15, col = jet.col(10))

#plot surface with rgl 

col <- rainbow(length(z))[rank(z)]
persp3d(x, y, z, color = col, xlab = "severity", ylab = "probability", zlab = "exposure")
surface3d(x ,y, z, back="lines")
surface3d(x, y, z, front = "lines")

#generate points (for now random, later informed)
severity<-sample(1:100)
exposure<-sample(1:100)
probability<-sample(1:100)

#plot the points into the surface graph
points3d(severity, probability, exposure, size=5, col = rainbow(10))

#plot only points
plot3d(severity, probability, exposure, size=5, col=rainbow(10))
