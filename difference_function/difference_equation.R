#difference function


#the equation: y[i+1]<-r*y[i]*(1-y[i])

rset<-seq(4, 2, by = -0.02)

for (r in rset) { 

  result<-vector(mode="numeric", length=0)
  y0<-0.01 #initial condition
#  r<-2.9 #rate

  result<-append(result, y0)

  for (i in 1:100) {

    y<-r*y0*(1-y0)
    result<-append(result,y)
    y0<-y

#    print(y0)
#    print(result)
  }
  
  print (r)
  plot(result, type="l", ylim = c(0,1), main = r)

}