# SeaFrost real import alert sampling scheme
# sampling unit = 1lb

# bad ship 1% contamination
A1<-c(rep(1, times=42), rep(0, times=4116))
A2<-c(rep(1, times=41), rep(0, times=4075))
A3<-c(rep(1, times=41), rep(0, times=4075))
A4<-c(rep(1, times=37), rep(0, times=3660))
A5<-c(rep(1, times=36), rep(0, times=3534))
A6<-c(rep(1, times=30), rep(0, times=2994))
A7<-c(rep(1, times=16), rep(0, times=1580))
A8<-c(rep(1, times=34), rep(0, times=3368))
A9<-c(rep(1, times=30), rep(0, times=2994))
A10<-c(rep(1, times=28), rep(0, times=2744))

# Bad day 1% contamination
A1<-c(rep(1, times=334), rep(0, times=3824))
A2<-c(rep(1, times=0), rep(0, times=4116))
A3<-c(rep(1, times=0), rep(0, times=4116))
A4<-c(rep(1, times=0), rep(0, times=3696))
A5<-c(rep(1, times=0), rep(0, times=3570))
A6<-c(rep(1, times=0), rep(0, times=3024))
A7<-c(rep(1, times=0), rep(0, times=1596))
A8<-c(rep(1, times=0), rep(0, times=3402))
A9<-c(rep(1, times=0), rep(0, times=3024))
A10<-c(rep(1, times=0), rep(0, times=2772))

# bad ship 5% contamination
A1<-c(rep(1, times=208), rep(0, times=3950))
A2<-c(rep(1, times=206), rep(0, times=3910))
A3<-c(rep(1, times=206), rep(0, times=3910))
A4<-c(rep(1, times=185), rep(0, times=3511))
A5<-c(rep(1, times=179), rep(0, times=3391))
A6<-c(rep(1, times=151), rep(0, times=2873))
A7<-c(rep(1, times=80), rep(0, times=1516))
A8<-c(rep(1, times=170), rep(0, times=3232))
A9<-c(rep(1, times=151), rep(0, times=2872))
A10<-c(rep(1, times=139), rep(0, times=2633))

# Bad day 5% contamination
A1<-c(rep(1, times=1674), rep(0, times=2484))
A2<-c(rep(1, times=0), rep(0, times=4116))
A3<-c(rep(1, times=0), rep(0, times=4116))
A4<-c(rep(1, times=0), rep(0, times=3696))
A5<-c(rep(1, times=0), rep(0, times=3570))
A6<-c(rep(1, times=0), rep(0, times=3024))
A7<-c(rep(1, times=0), rep(0, times=1596))
A8<-c(rep(1, times=0), rep(0, times=3402))
A9<-c(rep(1, times=0), rep(0, times=3024))
A10<-c(rep(1, times=0), rep(0, times=2772))

# bad ship 10% contamination
A1<-c(rep(1, times=416), rep(0, times=3742))
A2<-c(rep(1, times=412), rep(0, times=3704))
A3<-c(rep(1, times=412), rep(0, times=3704))
A4<-c(rep(1, times=370), rep(0, times=3326))
A5<-c(rep(1, times=357), rep(0, times=3213))
A6<-c(rep(1, times=302), rep(0, times=2722))
A7<-c(rep(1, times=160), rep(0, times=1436))
A8<-c(rep(1, times=340), rep(0, times=3062))
A9<-c(rep(1, times=302), rep(0, times=2722))
A10<-c(rep(1, times=277), rep(0, times=2495))

# Bad day 10% contamination
A1<-c(rep(1, times=3347), rep(0, times=811))
A2<-c(rep(1, times=0), rep(0, times=4116))
A3<-c(rep(1, times=0), rep(0, times=4116))
A4<-c(rep(1, times=0), rep(0, times=3696))
A5<-c(rep(1, times=0), rep(0, times=3570))
A6<-c(rep(1, times=0), rep(0, times=3024))
A7<-c(rep(1, times=0), rep(0, times=1596))
A8<-c(rep(1, times=0), rep(0, times=3402))
A9<-c(rep(1, times=0), rep(0, times=3024))
A10<-c(rep(1, times=0), rep(0, times=2772))


a<-c(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)
par(mfrow=c(1,2))

#Set up vectors
b<-vector(mode="numeric", length=0)
catch<-vector(mode="numeric", length=0)
count<-0
catch<-vector(mode="numeric", length=0)

#a<-c(1,0,0,0,0,0,0,0,0,0)

for (i in 1:1000) { #trial starts with trying to detect contamination and ends with detecting one
  b<-vector(mode="numeric", length=0)
  count<-0

  while (sum(b) < 1) {#the loop keeps going until we hit a "1"
    a<-sample(a)
    b<-sample(a,18)#this is the number of samples drawn
    count<-count+1#this is how many times we did not catch anything in this trial
    print (b)
  }
count
catch<-append(catch,count)#vector of numbers indicating how long it took to catch a "1" in each trial
catch
}
#par(new=N)
plot(density(catch))
plot(table(catch))
sum(catch)#this is the total number of sampling attempts

#plotting multiple histograms per %of contamination in the dataset

p1<-hist(catch1,)
p5<-hist(catch5)
p10<-hist(catch10)

plot(p10,col=rgb(0,1,0,1/4), xlim=c(0,50), ylim=c(0,1000))
plot(p5,col=rgb(1,0,0,1/4), xlim=c(0,50), ylim=c(0,1000), add=T)
plot(p1,col=rgb(0,0,1,1/4), xlim=c(0,50), ylim=c(0,1000), add=T)