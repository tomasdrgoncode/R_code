x<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
result<-vector(mode="numeric", length=0)

for (n in 1:10000) {

  xx<-sample(x, 16)
  sxx<-sum(xx)
  result<-append(result, sxx)

}

hist(result)




xresult<-c(1,1,1,0,1,1,1,0,1,1,1,0,1,1,1,0,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0)


library("pROC", lib.loc="~/R/win-library/3.3")

plot(roc(x,xresult, AUC=TRUE, ci=TRUE)) #to plot AUC

#monte Carlo trials random AUC distribution

#for the suicidality model, test portion (20% of data)
x<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

#for the actual experiment it's 130 cases and 65 controls, 20% of that is 26 cases and 13 controls

x<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0)

x<-c(x,x,x,x,x,x,x,x,x,x,x)

result<-vector(mode="numeric", length=0)

for (n in 1:1000) {

  xarea<-roc(x,sample(x)) #to extract AUC from a random trial
  xauc<-auc(xarea)
  result<-append(result, xauc)

}

hist(result)

plot(density(result))