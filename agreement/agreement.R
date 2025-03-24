

# simulation, how likely agreement by chance

field<-c(rep(1, times=94), rep(0, times=4)) #real data
lab<-c(rep(1, times=90), rep(0, times=8)) #real data

score<-vector(mode="numeric", length=0)
ag<-vector(mode="numeric", length=0)

for (i in 1:10000) { # loop through trials 
  print (i)
  a<-sample(field, (length(field))) #randomize field
  b<-sample(lab, (length(lab))) #randomize lab
  
  for (j in 1:(length(field))) { #loop through randomized set

    if (a[j]==b[j]) {
    ag<-append(ag, 1) #if agreement (1-1 or 0-0) then add 1 to ag vector
    }
  
  else {
    ag<-append(ag, 0) #if disagreement (1-0 or 0-1) then add 0 to ag vector
    } 
  
  }
score<-append(score, sum(ag)) #total score is a sum of the ag vector
ag<-vector(mode="numeric", length=0) # xero out ag for next trial

}

hist(score, freq=FALSE) #histogram of score with "probability" in Y axis

score_s<-sort(score, decreasing=TRUE)
table(score)



