
bank<-0
throw<-0
bet<-0
s<-c(rep(0, times=18), rep(1, times=18))
throws<-vector(mode="numeric", length=0)


for (i in 1:100){
  bet<-1
  bank<-(bank-bet)

  throw<-sample(s,1)
  throws<-append(throws, throw)

 if (throw==1){ 
  
  bet<-bet*2 
  bank<-(bank + bet)
 }

 if (throw==!0) { 
   
  bank<-bank-bet
 }
}

bank
