negative<-c(0,0,0,0,0,0.67, 0.38, 0.04, 0.75, 0.05, 0.014, 2.9, 2.2, 1.4)
positive<-c(31.3,21,26,6.7,1.7,4.2,3.1,2.1,2,2,1.9,1.6,1.7,3.5,1.6,1290.0,53.9)

shapiro.test(positive) #is "positive" normal? (no)

shapiro.test(negative) #is "negative" normal? (no)
#can't do ttest on non-normal

ks.test(positive,negative)#ks difficult to interprete in this situation





# simulation, how likely agreement by chance

field<-c(rep(1, times=96), rep(0, times=4)) #real data
lab<-c(rep(1, times=90), rep(0, times=10)) #real data

score<-vector(mode="numeric", length=0)
ag<-vector(mode="numeric", length=0)

for (i in 1:10000) { # loop through trials 
  
  a<-sample(field) #randomize field
  b<-sample(lab) #randomize lab
  
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
print(score_s)
