mydata<-Updated_ITR_Employee_Validation_02082023

no<-mydata$Hours [which (mydata$Operational == 'Operational Support')] #this is "indirect" and "support"
op<-mydata$Hours [which (mydata$Operational == 'Operational Direct')]

#no<-mydata$Hours [which (mydata$Category == 'Non-Operational')]
#op<-mydata$Hours [which (mydata$Category == 'Operational')]

#tobacco etc
mydata<-ITR_CF_Tobacco_OpvsNonOp
mydata<-OPQO_NatExp
mydata<-OPQO_GenCSO
mydata<-OPQO_Cadre
mydata<-ITR_GenCSO_Cadre
mydata<-OIO_FY23_ITR_Data_673412_hours
mydata<-OIO_FY23_ITR_Data_699040_hours

colnames(mydata)<- c("ID","Operational","Hours")

no<-mydata$Hours [which (mydata$Operational == 'Non-Operational')] 
op<-mydata$Hours [which (mydata$Operational == 'Operational')]
length(no)
length(op)

meanno<-mean(no)
meanop<-mean(op)

percentop<-meanop/(meanop+meanno)

boxplot(no, op)

hist(no)
hist(op)

conversion<-2087*percentop

print("Means")

meanop
meanno
percentop
conversion

#or from sums of op and nonop

sop<-sum(op)
sno<-sum(no)

spercent<-sop/(sno+sop)

sconversion<-2087*spercent

print("Sums")

sop
sno
spercent
sconversion

#or medians

mop<-median(op)
mno<-median(no)

mpercent<-mop/(mno+mop)

mconversion<-2087*mpercent

print("Median")

mop
mno
mpercent
mconversion

#geometric mean
gmop<-exp(mean(log(op)))
gmno<-exp(mean(log(no))) 

gpercent<-gmop/(gmno+gmop)
gmconv<-2087*gpercent

#or psych package
library("psych")          

gmop<-geometric.mean(op)
gmno<-geometric.mean(no) 

gpercent<-gmop/(gmno+gmop)
gmconv<-2087*gpercent

print("Geometric Mean")

gmop
gmno
gpercent
gmconv






#distribution of FTE totals

mydata_wide<-reshape(mydata, idvar = "FDA User ID1", timevar = "Operational", direction = "wide")

mydata_wide[3]<- NULL
colnames(mydata_wide)<- c("ID","OP", "NO") 
mydata_wide<-na.omit(mydata_wide)
mydata_wide$sum<-(mydata_wide$OP + mydata_wide$NO)

hist(mydata_wide$sum,breaks=40)
boxplot (mydata_wide$sum, ylim=c(0,2500))

print ("Mean FTE")
mean(mydata_wide$sum)
print ("Standard deviation")
sd(mydata_wide$sum)

