data<-read.table("outbreaks.txt", header=TRUE)

par(mfrow=c(3,1))

Ecoli<-subset(data, Etiology=="Escherichia")
plot(Ecoli$Year, Ecoli$Outbreaks, ylim=c(0, max(Ecoli$Outbreaks)), type="b")

Listeria<-subset(data, Etiology=="Listeria")
plot(Listeria$Year, Listeria$Outbreaks, ylim=c(0, max(Listeria$Outbreaks)), type="b")

Salmonella<-subset(data, Etiology=="Salmonella")
plot(Salmonella$Year, Salmonella$Outbreaks, ylim=c(0, max(Salmonella$Outbreaks)), type="b")