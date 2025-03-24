FSMAHR <- read.table("FSMAHR.txt", sep = "\t", header = TRUE) #read data
FSMAHR <- FSMAHR[!duplicated(FSMAHR$Work_Accomplishment_ID),] #remove acc ID duplicates
FSMAHR <- subset(FSMAHR, as.Date(FSMAHR$Operation_Date,"%m/%d/%Y") > as.Date("01/01/2011", "%m/%d/%Y")) #subset by date


FSMANHR<- read.table("FSMANHR.txt", sep = "\t", header = TRUE) #read data
FSMANHR <- FSMANHR[!duplicated(FSMANHR$Work_Accomplishment_ID),] #remove acc ID duplicates
FSMANHR <- subset(FSMANHR, as.Date(FSMANHR$Operation_Date, "%m/%d/%Y") > as.Date("01/01/2011", "%m/%d/%Y")) #subset by date


#plot date vs hours
plot(as.Date(FSMAHR$Operation_Date, "%m/%d/%Y"), FSMAHR$Total_Hours_Spent) 
plot(as.Date(FSMANHR$Operation_Date, "%m/%d/%Y"), FSMANHR$Total_Hours_Spent)


#analyze High Risk
mean(FSMAHR$Total_Hours_Spent)
median(FSMAHR$Total_Hours_Spent)
sd(FSMAHR$Total_Hours_Spent)
max(FSMAHR$Total_Hours_Spent)
min(FSMAHR$Total_Hours_Spent)
boxplot(FSMAHR$Total_Hours_Spent)
hist(FSMAHR$Total_Hours_Spent, breaks=200)

#analyze non-High Risk
mean(FSMANHR$Total_Hours_Spent)
median(FSMANHR$Total_Hours_Spent)
sd(FSMANHR$Total_Hours_Spent)
max(FSMANHR$Total_Hours_Spent)
min(FSMANHR$Total_Hours_Spent)
boxplot(FSMANHR$Total_Hours_Spent)
hist(FSMANHR$Total_Hours_Spent, xlim=c(0,1200), breaks=200)

#outlier removal


remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.001, .999), na.rm = na.rm, ...) #what portion of outliers to remove?
  H <- 1.5 * IQR(x, na.rm = na.rm) #inter quantile range
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

#make sure to name the correct dataset
data<-as.numeric(remove_outliers(FSMAHR$Total_Hours_Spent)) #data with removed outliers
boxplot(FSMAHR$Total_Hours_Spent, data)
mean(FSMAHR$Total_Hours_Spent) #with outliers
mean(data, na.rm=TRUE) #without outliers
median(FSMAHR$Total_Hours_Spent) #with outliers 
median(data, na.rm=TRUE) #without outliers





hours <- read.table("HR_NHR.txt", sep = "\t", header = TRUE)
hist(hours$HR2, xlim=c(0,1000), breaks = 200)
hist(hours$HR3, xlim=c(0,1000))
hist(hours$NHR2, breaks=200)
hist(hours$NHR3, xlim=c(0,600))

boxplot(hours$HR2, hours$HR3 )
boxplot(hours$NHR2, hours$NHR3 )

mean(hours$HR2, na.rm=TRUE)
sd(hours$HR2, na.rm=TRUE)
median(hours$HR2, na.rm=TRUE)

mean(hours$HR3, na.rm=TRUE)
sd(hours$HR3, na.rm=TRUE)
median(hours$HR3, na.rm=TRUE)

mean(hours$NHR2, na.rm=TRUE)
sd(hours$NHR2, na.rm=TRUE)
median(hours$NHR2, na.rm=TRUE)

mean(hours$NHR3, na.rm=TRUE)
sd(hours$NHR3, na.rm=TRUE)
median(hours$NHR3, na.rm=TRUE)
