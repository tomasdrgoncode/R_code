#can you go worse than random when you create a misinformed sampling scheme?

# bins of sampling units, 1% contamination in all bins
good<-99
bad<-1
# bins of sampling units, 3% contamination in all bins
good<-97
bad<-3
# bins of sampling units, 5% contamination in all bins
good<-95
bad<-5
# bins of sampling units, 8% contamination in all bins
good<-92
bad<-8

A1<-c(rep(1, times=(bad)), rep(0, times=(good)))
A2<-c(rep(1, times=(bad)), rep(0, times=(good)))
A3<-c(rep(1, times=(bad)), rep(0, times=(good)))
A4<-c(rep(1, times=(bad)), rep(0, times=(good)))
A5<-c(rep(1, times=(bad)), rep(0, times=(good)))
A6<-c(rep(1, times=(bad)), rep(0, times=(good)))
A7<-c(rep(1, times=(bad)), rep(0, times=(good)))
A8<-c(rep(1, times=(bad)), rep(0, times=(good)))
A9<-c(rep(1, times=(bad)), rep(0, times=(good)))
A10<-c(rep(1, times=(bad)), rep(0, times=(good)))



# bins of sampling units, 1% contamination, all in bin #10
good<-100
bad<-0
good10<-90
bad10<-10

# bins of sampling units, 3% contamination, all in bin #10
good<-100
bad<-0
good10<-70
bad10<-30

# bins of sampling units, 5% contamination, all in bin #10 
good<-100
bad<-0
good10<-50
bad10<-50

# bins of sampling units, 8% contamination, all in bin #10 
good<-100
bad<-0
good10<-20
bad10<-80


A1<-c(rep(1, times=(bad)), rep(0, times=(good)))
A2<-c(rep(1, times=(bad)), rep(0, times=(good)))
A3<-c(rep(1, times=(bad)), rep(0, times=(good)))
A4<-c(rep(1, times=(bad)), rep(0, times=(good)))
A5<-c(rep(1, times=(bad)), rep(0, times=(good)))
A6<-c(rep(1, times=(bad)), rep(0, times=(good)))
A7<-c(rep(1, times=(bad)), rep(0, times=(good)))
A8<-c(rep(1, times=(bad)), rep(0, times=(good)))
A9<-c(rep(1, times=(bad)), rep(0, times=(good)))
A10<-c(rep(1, times=(bad10)), rep(0, times=(good10)))


#zero out variables

resultbins<-0
negative<-0
positive<-0
j<-5 #samples per bin, this is the sampling plan

for (i in 1:1000) { # loop through trials of each sampling strategy  
    b<-vector(mode="numeric", length=0)
    b<-append(b, sample(A1,(j)))
    b<-append(b, sample(A2,(j)))
    b<-append(b, sample(A3,(j)))
    b<-append(b, sample(A4,(j)))
    b<-append(b, sample(A5,(j)))
    b<-append(b, sample(A6,(j)))
    b<-append(b, sample(A7,(j)))
    b<-append(b, sample(A8,(j)))
    b<-append(b, sample(A9,(8))) #in bad decision oversample this one
    b<-append(b, sample(A10,(2))) #in bad decision undersample this one
    
if (sum(b)==0) {
      negative<-negative+1
    }
    
    else {
      positive<-positive+1
    } 
    
  }
  
resultbins<-(negative/(positive+negative))


# or for random sampling
a<-c(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)
j<-50 #10 times the non-random "j", or any other sampling strategy

resultrandom<-0
negative<-0
positive<-0

for (i in 1:10000) { # loop through trials of each sampling strategy  
  b<-vector(mode="numeric", length=0)
  b<-sample(a,(j))

  if (sum(b)==0) {
    negative<-negative+1
  }

  else {
    positive<-positive+1
  } 

}

resultrandom<-(negative/(positive+negative)) 

print(resultbins)
print(resultrandom) 

data<-c(resultrandom,resultbins)
names<-c("random","stratified" )
barplot(data, ylim=c(0,1), names.arg=(names))


#data summary

gbe<-c(0.6, 0.218, 0.065, 0.013) #good decision, bins, even
gba<-c(0.57, 0.184, 0.026, 0) #good decision, bins, all in 10
gbr<-c(0.6, 0.213, 0.07, 0.012) #good decision, bins, random
bbe<-c(0.58, 0.206, 0.08, 0.017) #bad decision, bins even
bba<-c(0.819, 0.534, 0.25, 0.033) #bad decision, bins, all in 10
contam<-c(1, 3, 5, 8) #x axis

plot(contam,gbe, type="b", ylim=c(0,0.9))
par(new=TRUE)
plot(contam,gba, type="b", ylim=c(0,0.9), col="blue")
par(new=TRUE)
plot(contam,gbr, type="b", ylim=c(0,0.9), col="red")
par(new=TRUE)
plot(contam,bbe, type="b", ylim=c(0,0.9), col="green")
par(new=TRUE)
plot(contam,bba, type="b", ylim=c(0,0.9), col="magenta")


#now to the SeaFrost example
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

# overall 20% contamination.....now all A1 would be contaminated so there is no point doing scenarios

a<-c(rep(1, times=6695), rep(0, times=26779))

# sampling scheme for DWPE and surveillance

resultrandom<-0
resultbins<-0

b<-vector(mode="numeric", length=0)
sampling<-vector(mode="numeric", length=0)

#make sure the counters are zeroed in the right place....
negative<-0
positive<-0

for (i in 1:1000) { # loop through trials of sampling strategy  
  
#stratified DWPE sampling  
#  b<-append(b, sample(A1,(18)))
#  b<-append(b, sample(A2,(18)))
#  b<-append(b, sample(A3,(18)))
#  b<-append(b, sample(A4,(18)))
#  b<-append(b, sample(A5,(8)))
#  b<-append(b, sample(A6,(7)))
#  b<-append(b, sample(A7,(3)))
#  b<-append(b, sample(A8,(7)))
#  b<-append(b, sample(A9,(6)))
#  b<-append(b, sample(A10,(5)))
 
#stratified but not weighed by the size of the product code  
#  b<-append(b, sample(A1,(10)))
#  b<-append(b, sample(A2,(10)))
#  b<-append(b, sample(A3,(10)))
#  b<-append(b, sample(A4,(10)))
#  b<-append(b, sample(A5,(10)))
#  b<-append(b, sample(A6,(10)))
#  b<-append(b, sample(A7,(10)))
#  b<-append(b, sample(A8,(10)))
#  b<-append(b, sample(A9,(10)))
#  b<-append(b, sample(A10,(10)))

#stratified surveillance sampling
  b<-append(b, sample(A1,(2)))
  b<-append(b, sample(A2,(2)))
  b<-append(b, sample(A3,(2)))
  b<-append(b, sample(A4,(2)))
  b<-append(b, sample(A5,(2)))
  b<-append(b, sample(A6,(2)))
  b<-append(b, sample(A7,(1)))
  b<-append(b, sample(A8,(2)))
  b<-append(b, sample(A9,(2)))
  b<-append(b, sample(A10,(1)))

  
  if (sum(b)==0) {
    negative<-negative+1
  }
  
  else {
    positive<-positive+1
  } 
  
}

resultbins<-negative/(positive+negative) 

# or for random sampling
a<-c(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)

negative<-0
positive<-0

for (i in 1:10000) { # loop through trials of surveillance sampling strategy  
  b<-vector(mode="numeric", length=0)
  b<-sample(a,(18)) #this is the sampling strategy
  
  if (sum(b)==0) {
    negative<-negative+1
  }
  
  else {
    positive<-positive+1
  } 
  
}

resultrandom<-(negative/(positive+negative)) 

print(resultbins)
print(resultrandom) 

data<-c(resultrandom,resultbins)
names<-c("random","stratified" )
barplot(data, ylim=c(0,1), names.arg=(names))

