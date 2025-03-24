data<-read.table("Entry_Review_Time_Survey.txt", header=TRUE)


plot(data$quarter, data$Hours, ylim=c(0, max(data$Hours+2000)), xlim=c(0,32), axes=FALSE)
axis(side = 1, at = c(0,4,8,12,16,20,24,28,32))
axis(side = 2, at = c(seq(from = 0, to = 16000, by=1000)))


Hours<-data$Hours
Quarter<-data$quarter

abline(lm(Hours~Quarter))
fit<-lm(Hours~Quarter)


prd<-predict(lm(Hours~Quarter),interval=c("confidence"), level = 0.95, type="response")
prd<-as.data.frame(prd)

lines(prd$lwr)
lines(prd$upr)

prd<-predict(fit, data.frame(Quarter=c(0:32)), interval=c("predict"), level = 0.95)
prd<-as.data.frame(prd)
lines(prd$lwr)
lines(prd$upr)

# prediction by year and prediction of number of lines
data<-read.table("CDRH_ER_lines_hours.txt", header=TRUE)
data$hperline<-data$hours/data$lines #calculate hours per line

plot(data$year,data$hperline, ylim=c(0,0.005), type="b")
plot(data$year,data$hours, ylim=c(0,65000), type="b")
plot(data$year,data$lines, ylim=c(0, max(data$lines)), type="b")

#prepare data for fit
year<-data$year
pyear<-c(year, 2017, 2018) #predicted years
lines<-data$lines
hours<-data$hours
hpline<-data$hperline

#plot hours with space for prediction
plot(year, hours, ylim=c(0,65000), xlim=c(2012,2018))
abline(lm(hours~year))
fit<-lm(hours~year)

#plot hours plus prediction and 95% confidence interval
prd<-predict(fit, data.frame(year=c(2012,2013,2014,2015,2016, 2017,2018)), interval=c("predict"), level = 0.95)
prd<-as.data.frame(prd)

lines(pyear, prd$lwr)
lines(pyear, prd$upr)

#predict # of lines
plot(year, lines, ylim=c(0,24000000), xlim=c(2012,2018))
abline(lm(lines~year))
fit<-lm(lines~year)

#plot hours plus prediction and 95% confidence interval
prd<-predict(fit, data.frame(year=c(2012,2013,2014,2015,2016, 2017,2018)), interval=c("predict"), level = 0.95)
prd<-as.data.frame(prd)

lines(pyear, prd$lwr)
lines(pyear, prd$upr)



