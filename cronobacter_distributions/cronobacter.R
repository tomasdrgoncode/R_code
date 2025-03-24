
library(fitdistrplus)
library(logspline)

initialcfu<-rep(c(1,0), times = c(10,990)) #initial distribution of cfu per exposure unit
initialcfu<-sample(initialcfu)
initial<-initialcfu
nn<-length(initialcfu) #number of exposure units

barplot(initialcfu)
#hist(initialcfu)
#sum(initialcfu)
#descdist(initialcfu)
#descdist(initialcfu, discrete = TRUE)

# enrichment, n = number of doublings of the cfu
for (n in 1:10) {
enriched<-initialcfu*2
initialcfu<-enriched
}

barplot(enriched)
#hist(enriched)
#sum(enriched)
#descdist(enriched, discrete = FALSE)
#descdist(enriched, discrete = TRUE)

#generic levels of mixing


mixed<-vector(mode = "numeric", length = 0)

for (i in 1:nn) {
  mix<-mean(sample(enriched, 5))
  mixed<-append(mixed,mix)
}

barplot(mixed)
#hist(mixed)
#sum(mixed)
#descdist(mixed)
#descdist(mixed, discrete = TRUE)
mixed5<-mixed



mixed<-vector(mode = "numeric", length = 0)

for (i in 1:nn) {
  mix<-mean(sample(enriched, 10))
  mixed<-append(mixed,mix)
}

barplot(mixed)
#hist(mixed)
#sum(mixed)
#descdist(mixed)
#descdist(mixed, discrete = TRUE)
mixed10<-mixed


mixed<-vector(mode = "numeric", length = 0)

for (i in 1:nn) {
  mix<-mean(sample(enriched, 20))
  mixed<-append(mixed,mix)
}

barplot(mixed)
#hist(mixed)
#sum(mixed)
#descdist(mixed)
#descdist(mixed, discrete = TRUE)
mixed20<-mixed



mixed<-vector(mode = "numeric", length = 0)

for (i in 1:nn) {
  mix<-mean(sample(enriched, 50))
  mixed<-append(mixed,mix)
}

barplot(mixed)
#hist(mixed)
#sum(mixed)
#descdist(mixed)
#descdist(mixed, discrete = TRUE)
mixed50<-mixed

mixed<-vector(mode = "numeric", length = 0)

for (i in 1:nn) {
  mix<-mean(sample(enriched, 100))
  mixed<-append(mixed,mix)
}

barplot(mixed)
#hist(mixed)
#sum(mixed)
#descdist(mixed)
#decdist(mixed, discrete = TRUE )
mixed100<-mixed


mixed<-vector(mode = "numeric", length = 0)

for (i in 1:nn) {
  mix<-mean(sample(enriched, 200))
  mixed<-append(mixed,mix)
}

barplot(mixed)
#hist(mixed)
#sum(mixed)
#descdist(mixed)
#descdist(mixed, discrete = TRUE)
mixed200<-mixed


mixed<-vector(mode = "numeric", length = 0)

for (i in 1:nn) {
  mix<-mean(sample(enriched, 300))
  mixed<-append(mixed,mix)
}

barplot(mixed)
#hist(mixed)
#sum(mixed)
#descdist(mixed)
#descdist(mixed, discrete = TRUE)
mixed300<-mixed

mixed<-vector(mode = "numeric", length = 0)

for (i in 1:nn) {
  mix<-mean(sample(enriched, 400))
  mixed<-append(mixed,mix)
}

barplot(mixed)
#hist(mixed)
#sum(mixed)
#descdist(mixed)
#descdist(mixed, discrete = TRUE)
mixed400<-mixed

mixed<-vector(mode = "numeric", length = 0)

for (i in 1:nn) {
  mix<-mean(sample(enriched, 500))
  mixed<-append(mixed,mix)
}

barplot(mixed)
#hist(mixed)
#sum(mixed)
#descdist(mixed)
#descdist(mixed, discrete = TRUE)
mixed500<-mixed

sampleid<-c(1:1000)


randmix<-data.frame(sampleid, log(mixed500), log(mixed400), log(mixed300), log(mixed200), log(mixed100), log(mixed50), log(mixed20), log(mixed10), log(mixed5), log(enriched), log(initial))

#or in reverse
randmix<-data.frame(sampleid, log(initial), log(enriched), log(mixed5), log(mixed10), log(mixed20), log(mixed50), log(mixed100), log(mixed200), log(mixed300), log(mixed400), log(mixed500))


# Heatmap 
library(reshape)
mdata <- melt(randmix, id="sampleid")

library(ggplot2)
library(hrbrthemes)
ggplot(mdata, aes(sampleid,variable, fill= value)) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="blue") +
  theme_ipsum()

ggplot(mdata, aes(variable, sampleid, fill= value)) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="blue") +
  theme_ipsum()





library(viridis)
ggplot(mdata, aes(sampleid,variable, fill= value)) + 
  geom_tile() +
  scale_fill_viridis(discrete=FALSE) +
  theme_ipsum()



#specific mixing protocols

enriched<-sample(enriched)

mixed<-vector(mode = "numeric", length = 0)

for (i in 1:nn) {
  mix<-mean(sample(enriched, 50))
  mixed<-append(mixed,mix)
}

barplot(mixed)
hist(mixed)
descdist(mixed)


#conveyor belt mixing, mix 2 adjacent units (CURRENTLY CONFLICT WITH SAMPLING STRATEGY EVAL BELOW)

enriched<-sample(enriched)

for (k in 1:10) { 
 
  for (j in 1:100) { 

    mixed<-vector(mode = "numeric", length = 0)
  
    for(i in seq(1, (length(enriched)))) {

      if (i < length(enriched)) { #for all before the last element or enriched
      
        mix<-mean(c(enriched[i], enriched[i+1])) #mix = average consecutive elements and append the average
        mixed<-append(mixed,mix)
      
        } else {
      
        last<-c(enriched[1], enriched[(length(enriched))]) #add last element that is the average of last and first element of the vector to the front of the vector so that I don't shorten the mixed
        mlast<-mean(last)
          if (sample(c(0, 1), 1) == 1) {   #this is here to even out the peak drift
          mixed<-append(mixed, mlast, after = 0)
          } else {
          mixed<-append(mixed, mlast)  
          }
        }
      }

  #plot(mixed, type="b")
  #hist(mixed)
  enriched<-mixed

  }

assign(paste0("mixed", k), mixed )
  
} #end of big loop

sampleid<-c(1:1000)
conveyormix<-data.frame(sampleid, log(initialcfu), log(enriched), log(mixed1), log(mixed2), log(mixed3), log(mixed4), log(mixed5), log(mixed6), log(mixed7), log(mixed8), log(mixed9), log(mixed10))


#MODIFY FOR CONVEYOR BELT MIXING QUITE NOT WORKING YET
# Heatmap 
library(reshape)
mdata <- melt(conveyormix, id="sampleid")

library(ggplot2)
library(hrbrthemes)
ggplot(mdata, aes(sampleid,variable, fill= value)) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="blue") +
  theme_ipsum()

library(viridis)
ggplot(mdata, aes(sampleid,variable, fill= value)) + 
  geom_tile() +
  scale_fill_viridis(discrete=FALSE) +
  theme_ipsum()




#Sampling strategy analysis

#see how many sub-samples over threshold
s<-data.frame(mixed)

length(which(s$mixed > 0))
length(which(s$mixed > 0))/length(s$mixed)*100



#calculation for one sampling strategy

samp<-vector(mode="numeric", length=0)

for (i in 1:1000){
  sa<-sample(mixed, 4) # number of increments taken
  sam<-mean(sa)
  samp<-append(samp,sam)
}

sd<-sd(samp) #this is a SD of the mean estimates, not a SD of the whole distribution...

bx.p<-boxplot(samp)
bxp(bx.p,pars=list(ylim=c(0,max(samp))))#lets me use the ylim on a boxplot

hist(samp)
quantile(samp,probs=c(.025,.975)) #calculate 95% confidence interval
quantile(samp,probs=c(.005,.995)) #calculate 99% confidence interval

mean(s)
sdx
sdx/mean(s)*100



#charts with multiple sampling strategies

# TOS model

volume<-c(4, 10, 50, 100, 500, 1000)
FSE<-vector(mode="numeric", length=0)

#hist(s)

samp<-vector(mode="numeric", length=0)

for (i in 1:1000){
  sa<-sample(mixed, 4)
  sam<-mean(sa)
  samp<-append(samp,sam)
}

samp4<-samp

samp<-vector(mode="numeric", length=0)

for (i in 1:1000){
  sa<-sample(mixed, 10)
  sam<-mean(sa)
  samp<-append(samp,sam)
}

samp10<-samp

samp<-vector(mode="numeric", length=0)

for (i in 1:1000){
  sa<-sample(mixed, 50)
  sam<-mean(sa)
  samp<-append(samp,sam)
}

samp50<-samp

samp<-vector(mode="numeric", length=0)

for (i in 1:1000){
  sa<-sample(mixed, 100)
  sam<-mean(sa)
  samp<-append(samp,sam)
}

samp100<-samp

samp<-vector(mode="numeric", length=0)

for (i in 1:1000){
  sa<-sample(mixed, 500)
  sam<-mean(sa)
  samp<-append(samp,sam)
}

samp500<-samp

samp<-vector(mode="numeric", length=0)

for (i in 1:1000){
  sa<-sample(mixed, 1000)
  sam<-mean(sa)
  samp<-append(samp,sam)
}

samp1000<-samp


sd4<-sd(samp4)
sd10<-sd(samp10)
sd50<-sd(samp50)
sd100<-sd(samp100)
sd500<-sd(samp500)
sd1000<-sd(samp1000)

sd<-c(sd4,sd10,sd50,sd100,sd500,sd1000)
FSE<-(sd/(mean(mixed)))*100

plot(volume, sd, type="b")
plot(volume, FSE, type="b")

data<-data.frame(samp4, samp10, samp50, samp100, samp500, samp1000)

bx.p<-boxplot(data, names=volume)
bxp(bx.p,pars=list(ylim=c(0,max(samp4))))#lets me use the ylim on a boxplot




boxplot(samp4, samp10, samp50, samp100, samp500, samp1000, names=volume)
result<-vector(mode="numeric", length=0)

#see how many point estimates under regulatory limit
negative<-vector(mode="numeric", length=0)

p<-data.frame(samp4, samp10, samp50, samp100, samp500, samp1000) 

negative<-append(negative, length(which(p$samp4 < 1)))
negative<-append(negative, length(which(p$samp10 < 1)))
negative<-append(negative, length(which(p$samp50 < 1)))
negative<-append(negative, length(which(p$samp100 < 1)))
negative<-append(negative, length(which(p$samp500 < 1)))
negative<-append(negative, length(which(p$samp1000 < 1)))

plot(negative/1000, type="b") #percent false negative per 1000 trials 






#Cronobacter example

cfu<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3.3,10,40,560,10,13.3,123.3,0,20,0,0,0,0,0,0,0,0,0
)

cfu<-c(rep(0, 1134), 3.3, 10, 40, rep(0, 25), 560, rep(0,66), 10, 13, 123.3, rep(0, 66), 20, rep(0, 67), rep(0, 864))

barplot(cfu)
hist(cfu)
descdist(cfu)





for(i in 1:length(c(1:10))) {                    # assign function within loop
  assign(paste0("x", i), i)
}


