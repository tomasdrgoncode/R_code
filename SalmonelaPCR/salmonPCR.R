vidas<-c(0,0,0,0,1,1,1,1,0,0,1,1,1,1,1,1,1,1,0)
PCR<-c(1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1)
cult<-c(1,1,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0)

library("pROC", lib.loc="~/R/win-library/3.3")

roc(cult,vidas)
plot(roc(cult,vidas, AUC=TRUE, ci=TRUE))

roc(cult, PCR)
plot(roc(cult, PCR, AUC=TRUE, ci=TRUE))


#empirical model of agreement

field<-VIDAS #real data
lab<-cult #real data

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

table(score)







#table 2

dnac<-c(10^5, 10^4, 10^3, 10^2, 10, 2, 1)
dnact<-c(23.02, 26.76, 29.65, 32.93, 35.83, 36.72, 38.48)
ldna<-log10(dnac)

cor(dnac, dnact)
plot(dnac,dnact)

cor(ldna, dnact)
plot(ldna, dnact)
abline(lm(dnact~ldna))
