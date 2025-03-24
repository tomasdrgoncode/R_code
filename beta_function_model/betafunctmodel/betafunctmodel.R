ex<-c(0,1,2,3,4,5,6,7,8,9) #exposure
sev<-c(0,1,2,3,4,5,6,7,8,9) #severity
mod<-vector(mode="numeric", length=0) #prepare empty vector
min_beta = 0.5

for (i in 1:10){
  for (j in 1:10){
    x<-ex[i]*sev[j] # product of exposure and severity
    y<-0.5 + (0.5/81)*x #coerce between 0.5 and 1
    mod<-append(mod, y) #put into vector
  }
}

for (i in 1:10){
  for (j in 1:10){
    x<-ex[i] + sev[j] # sum of exposure and severity
    y<-0.5 + (0.5/18)*x #coerce between 0.5 and 1
    mod<-append(mod, y) #put into vector
  }
}

for (i in 1:10){
  for (j in 1:10){
    x<-ex[i]^2 + sev[j]^2 # sum of exposure^2 and severity^2
    y<-0.5 + (0.5/162)*x #coerce between 0.5 and 1
    mod<-append(mod, y) #put into vector
  }
}

for (i in 1:10){
  for (j in 1:10){
    x<-(0.4*ex[i]*sev[j] + 0.4*(ex[i] + sev[j]) + 0.2*(ex[i]^2 + sev[j]^2))# more complex function
    y<-0.5 + (0.5/72)*x #coerce between 0.5 and 1
    mod<-append(mod, y) #put into vector
  }
}

#for varying minimal betas

ex<-c(0,1,2,3,4,5,6,7,8,9) #exposure
sev<-c(0,1,2,3,4,5,6,7,8,9) #severity
mod<-vector(mode="numeric", length=0) #prepare empty vector

for (i in 1:10){
  for (j in 1:10){
    x<-ex[i] + sev[j] # sum of exposure and severity
    y<-0.22 + (0.78/18)*x #coerce between 0.5 and 1
    mod<-append(mod, y) #put into vector
  }
}

beta<-matrix(mod, nrow=10, ncol=10) #matrix from vector

persp(ex, sev, beta, theta=10, phi=10, zlim = range(0,1), shade = 0.3)


# SeaFrost surveilance data, sampling unit = 1 lb
# 1% - 0.95
# 5% - 0.78
# 10% - 0.55
# 20% -  0.3

col <- rainbow(length(beta))[rank(beta)]
col2 <- matrix(rep(col,length(sev)), length(ex))
plot(persp3d(ex, sev, beta, color=col2, theta=10, phi=10, zlim = range(0,1), shade = 0.3))

