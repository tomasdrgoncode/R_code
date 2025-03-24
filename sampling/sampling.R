

#SeaFrost example

#Seafrost frozen Mahi cartons, 797 cartons, sampling unit = 1 carton
a<-c(rep(0, times=789), rep(1, times=8)) #1% cont
a<-c(rep(0, times=757), rep(1, times=40)) #5% cont
a<-c(rep(0, times=717), rep(1, times=80)) #10% cont
a<-c(rep(0, times=637), rep(1, times=160)) #20% cont
      
# same thing with pounds
#33474 pounds, sampling unit = 1 pound

a<-c(rep(0, times=33139), rep(1, times=335)) #1% cont
a<-c(rep(0, times=31800), rep(1, times=1674)) #5% cont
a<-c(rep(0, times=30127), rep(1, times=3347)) #10% cont
a<-c(rep(0, times=26779), rep(1, times=6695)) #20% cont

#Seafrost frozen Mahi cartons, 797 cartons, sampling unit = 1 carton, but by production code
#a<-c(rep(0, times=789), rep(1, times=8)) #1% cont
#a<-c(rep(0, times=757), rep(1, times=40)) #5% cont
#a<-c(rep(0, times=717), rep(1, times=80)) #10% cont
#a<-c(rep(0, times=637), rep(1, times=160)) #20% cont

#generic example, 50000 sampling units
#a<-c(rep(0, times=49500), rep(1, times=500)) #1%
#a<-c(rep(0, times=47500), rep(1, times=2500)) #5%
a<-c(rep(0, times=450), rep(1, times=50)) #10%
#a<-c(rep(0, times=40000), rep(1, times=10000)) #20%



result<-vector(mode="numeric", length=0)

b<-vector(mode="numeric", length=0)
sampling<-vector(mode="numeric", length=0)



for (j in seq(from=0, to=500, by=10)) { # loop through the sampling strategies
#zero out positive/negative counters
  negative<-0 
  positive<-0
  print (j)
  sampling<-append(sampling, j)
  for (i in 1:10000) { # loop through trials of each sampling strategy  

    b<-sample(a, (j))
    
    if (sum(b)==0) {
      negative<-negative+1
    }

    else {
      positive<-positive+1
    } 

  }

  result<-append(result, (negative/(positive+negative))) 
}

print(result) 
#plot(sampling, result, xlim=c(0,500), ylim=c(0,1), type="l")
par(new=TRUE)
plot(sampling, result, xlim=c(0,500), ylim=c(0,1), type="l", col="blue")



# SeaFrost, bad ship scenario, coerced to sampling per production code, 1% contamination
A1<-c(rep(1, times=1), rep(0, times=98))
A2<-c(rep(1, times=1), rep(0, times=97))
A3<-c(rep(1, times=1), rep(0, times=97))
A4<-c(rep(1, times=1), rep(0, times=87))
A5<-c(rep(1, times=1), rep(0, times=80))
A6<-c(rep(1, times=1), rep(0, times=71))
A7<-c(rep(1, times=1), rep(0, times=71))
A8<-c(rep(1, times=1), rep(0, times=84))
A9<-c(rep(1, times=1), rep(0, times=65))
A10<-c(rep(1, times=1), rep(0, times=38))

# SeaFrost, bad ship scenario, coerced to sampling per production code, 5% contamination
A1<-c(rep(1, times=4), rep(0, times=95))
A2<-c(rep(1, times=4), rep(0, times=94))
A3<-c(rep(1, times=4), rep(0, times=94))
A4<-c(rep(1, times=4), rep(0, times=84))
A5<-c(rep(1, times=4), rep(0, times=77))
A6<-c(rep(1, times=4), rep(0, times=68))
A7<-c(rep(1, times=4), rep(0, times=68))
A8<-c(rep(1, times=4), rep(0, times=81))
A9<-c(rep(1, times=4), rep(0, times=62))
A10<-c(rep(1, times=4), rep(0, times=35))

# SeaFrost, bad day scenario, coerced to sampling per production code, 1% contamination
A1<-c(rep(1, times=10), rep(0, times=89))
A2<-c(rep(1, times=0), rep(0, times=98))
A3<-c(rep(1, times=0), rep(0, times=98))
A4<-c(rep(1, times=0), rep(0, times=88))
A5<-c(rep(1, times=0), rep(0, times=81))
A6<-c(rep(1, times=0), rep(0, times=72))
A7<-c(rep(1, times=0), rep(0, times=72))
A8<-c(rep(1, times=0), rep(0, times=85))
A9<-c(rep(1, times=0), rep(0, times=66))
A10<-c(rep(1, times=0), rep(0, times=39))

# SeaFrost, bad day scenario, coerced to sampling per production code, 5% contamination
A1<-c(rep(1, times=40), rep(0, times=59))
A2<-c(rep(1, times=0), rep(0, times=98))
A3<-c(rep(1, times=0), rep(0, times=98))
A4<-c(rep(1, times=0), rep(0, times=88))
A5<-c(rep(1, times=0), rep(0, times=81))
A6<-c(rep(1, times=0), rep(0, times=72))
A7<-c(rep(1, times=0), rep(0, times=72))
A8<-c(rep(1, times=0), rep(0, times=85))
A9<-c(rep(1, times=0), rep(0, times=66))
A10<-c(rep(1, times=0), rep(0, times=39))



result<-vector(mode="numeric", length=0)

b<-vector(mode="numeric", length=0)
sampling<-vector(mode="numeric", length=0)



for (j in seq(from=1, to=10, by=1)) { # loop through the sampling strategies
  negative<-0
  positive<-0
  
  print (j)
  sampling<-append(sampling, j)
  for (i in 1:1000) { # loop through trials of each sampling strategy  
    
    b<-append(b, sample(A1,(j)))
    b<-append(b, sample(A2,(j)))
    b<-append(b, sample(A3,(j)))
    b<-append(b, sample(A4,(j)))
    b<-append(b, sample(A5,(j)))
    b<-append(b, sample(A6,(j)))
    b<-append(b, sample(A7,(j)))
    b<-append(b, sample(A8,(j)))
    b<-append(b, sample(A9,(j)))
    b<-append(b, sample(A10,(j)))
              
    if (sum(b)==0) {
      negative<-negative+1
    }
    
    else {
      positive<-positive+1
    } 
    
  }
  
  result<-append(result, (negative/(positive+negative))) 
}

print(result) 
plot(sampling, result)


# SeaFrost, bad day scenario, random sampling, 1% contamination
A1<-c(rep(1, times=10), rep(0, times=89))
A2<-c(rep(1, times=0), rep(0, times=98))
A3<-c(rep(1, times=0), rep(0, times=98))
A4<-c(rep(1, times=0), rep(0, times=88))
A5<-c(rep(1, times=0), rep(0, times=81))
A6<-c(rep(1, times=0), rep(0, times=72))
A7<-c(rep(1, times=0), rep(0, times=72))
A8<-c(rep(1, times=0), rep(0, times=85))
A9<-c(rep(1, times=0), rep(0, times=66))
A10<-c(rep(1, times=0), rep(0, times=39))

# SeaFrost, bad day scenario, random sampling, 5% contamination
A1<-c(rep(1, times=40), rep(0, times=59))
A2<-c(rep(1, times=0), rep(0, times=98))
A3<-c(rep(1, times=0), rep(0, times=98))
A4<-c(rep(1, times=0), rep(0, times=88))
A5<-c(rep(1, times=0), rep(0, times=81))
A6<-c(rep(1, times=0), rep(0, times=72))
A7<-c(rep(1, times=0), rep(0, times=72))
A8<-c(rep(1, times=0), rep(0, times=85))
A9<-c(rep(1, times=0), rep(0, times=66))
A10<-c(rep(1, times=0), rep(0, times=39))

a<-c(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)

result<-vector(mode="numeric", length=0)

b<-vector(mode="numeric", length=0)
sampling<-vector(mode="numeric", length=0)




for (j in seq(from=1, to=100, by=10)) { # loop through the sampling strategies
  negative<-0
  positive<-0
  
  print (j)
  sampling<-append(sampling, j)
  for (i in 1:100) { # loop through trials of each sampling strategy  
    
    b<-sample(a, (j))
    
    if (sum(b)==0) {
      negative<-negative+1
    }
    
    else {
      positive<-positive+1
    } 
    
  }
  
  result<-append(result, (negative/(positive+negative))) 
}

print(result) 
plot(sampling, result)


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

# overall 20% contamination.....now all A1 would be contaminated so there is no point doing scenarios

a<-c(rep(1, times=6695), rep(0, times=26779))

# sampling scheme for DWPE

result<-vector(mode="numeric", length=0)

b<-vector(mode="numeric", length=0)
sampling<-vector(mode="numeric", length=0)

#make sure the counters are zeroed in the right place....
negative<-0
positive<-0

for (i in 1:1000) { # loop through trials of sampling strategy  
    
    b<-append(b, sample(A1,(18)))
    b<-append(b, sample(A2,(18)))
    b<-append(b, sample(A3,(18)))
    b<-append(b, sample(A4,(18)))
    b<-append(b, sample(A5,(8)))
    b<-append(b, sample(A6,(7)))
    b<-append(b, sample(A7,(3)))
    b<-append(b, sample(A8,(7)))
    b<-append(b, sample(A9,(6)))
    b<-append(b, sample(A10,(5)))
    
    if (sum(b)==0) {
      negative<-negative+1
    }
    
    else {
      positive<-positive+1
    } 
    
}
  
result<-negative/(positive+negative) 

print(result) 

# same for surveilance sampling now sampling production codes

result<-vector(mode="numeric", length=0)

b<-vector(mode="numeric", length=0)

negative<-0
positive<-0

for (i in 1:10000) { # loop through trials of sampling strategy  
  
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
  
  #b<-append(b, sample(A1,(1)))
  #b<-append(b, sample(A2,(1)))
  #b<-append(b, sample(A3,(1)))
  #b<-append(b, sample(A4,(1)))
  #b<-append(b, sample(A5,(1)))
  #b<-append(b, sample(A6,(1)))
  #b<-append(b, sample(A7,(1)))
  #b<-append(b, sample(A8,(1)))
  #b<-append(b, sample(A9,(1)))
  #b<-append(b, sample(A10,(1)))
  
  
  if (sum(b)==0) {
    negative<-negative+1
  }
  
  else {
    positive<-positive+1
  } 
  
}

result<-negative/(positive+negative) 

print(result) 



# SeaFrost real import alert sampling scheme




a<-c(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)
a<-sample(a,33474) #randomize vector a (not necessary)
#or
a<-c(rep(1, times=335), rep(0, times=33139)) #overall 1%
a<-c(rep(1, times=1638), rep(0, times=31800)) #overall 5%
a<-c(rep(1, times=3347), rep(0, times=30127)) #overall 10%
a<-c(rep(1, times=6695), rep(0, times=26779)) #overall 20%

result<-vector(mode="numeric", length=0)

b<-vector(mode="numeric", length=0)
sampling<-vector(mode="numeric", length=0)

negative<-0
positive<-0


for (i in 1:100000) { # loop through trials of each sampling strategy  
    
    b<-sample(a, (18))
    
    if (sum(b)==0) {
      negative<-negative+1
    }
    
    else {
      positive<-positive+1
    } 
    
  }
  
result<-append(result, (negative/(positive+negative))) 

print(result) 



beta1<-c(0.002, 0.001, 0.334, 0.002, 0.0005, 0.832)
beta5<-c(0,0,0.004,0,0,0.4)
beta10<-c(0,0,0,0,0,0.15)
beta20<-c(0,0,0,0,0,0.018)
beta<-matrix(c(beta1, beta5, beta10, beta20), nrow=length(beta1))

colnames(beta)<-c("1%", "5%", "10%", "20%")
rownames(beta)<-c("DWPE_BS", "DWPE_BD", "DWPE_random", "Surv_BS", "Surv_BD", "Surv_random")
beta<-t(beta)
barplot(beta, beside=TRUE, legend.text=TRUE, main="sampling unit = 1lb, contamination = 1%, 5%, 10%, 20%")

