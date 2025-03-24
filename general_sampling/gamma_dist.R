#gamma distribution
#For large k the gamma distribution converges to Gaussian distribution 
#with mean μ = kθ and variance σ^2 = kθ^2.
# k=scale θ=shape
#rate = 1/scale
#rgamma(n, shape, scale)

n<-10000
sh<-100 #shape parameter, =mean if k=1
k<-1
r<-1/k #rate

data<-rgamma(n,sh,scale=(k))
hist(data)

meang<-k*sh #mean on gamma dist
var<-sqrt(k*sh^2) #variance of gamma dist
sdg<-sqrt(var) #standard deviation of gamma dist

mean(data)
sd(data)
meang
sdg

x<-rnorm(10, mean =100, sd=10)
data<-dgamma(x, 1000)
hist(data)