data<-read.table("IndustryCodeVarianceInTime.txt", header=TRUE)

data1<-data.frame(data$Total_Hours, data$Industry_Code) #isolate IC and hours

IC<-unique(data1$data.Industry_Code) #unique IC for subsetting

 
for (i in IC) {
  
datas<-subset(data1, data.Industry_Code==i)
datas<-datas$data.Total_Hours
assign(paste("IC",i,sep=""), datas)
}


IC<-paste("IC", IC, sep="")





boxplot(IC2,IC3,IC4,IC5,IC7,IC9,IC12,IC13,IC14,IC15,IC16,IC17,IC18,IC20,IC21,IC22,IC23,IC24,IC25,IC26,IC27,IC28,IC29,IC30,IC31,IC32,IC33,IC34,IC35,IC36,IC37,IC38,IC39,IC40,IC41,IC45,IC46,IC50,IC52,IC54,IC71,IC72, names=IC, cex.axis=0.7)