#Data on OIIPCODE 11 PAC 82845G
data<-read.table("Module_Time_Insight_V1.txt", header=TRUE)
data$OperationDate<-as.Date(data$OperationDate, "%m/%d/%Y")

plot(data$OperationDate, data$TotalHoursSpent, type="p")


fit <- glm(data$TotalHoursSpent~data$OperationDate)
co <- coef(fit)
abline(fit)

