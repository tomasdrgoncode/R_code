y<-25000 #principal
r<-0.01 #inflation or index
n<-4 # number of compounds
t<-1 # number of years

#simple
xs<-y+y*t*(r)
 
#compound
xc<-y*(1+r/n)^(n*t)

#continuous compounding
xcc = y*exp(1)^(r*t)

xs
xc
xcc