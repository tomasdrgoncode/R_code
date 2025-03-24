
#count unique institutions worldwide (clinicaltrials.org)

data<-read.csv("clinicaltrials_2006.csv")

institutions<-data$Sponsor.Collaborators

write.csv(institutions,"institutions.csv")

data1<-read.csv("institutions.csv", header=FALSE)

length(unique(data1$V1))



#count unique institutions US (clinicaltrials.org)

data<-read.csv("USclinicaltrials_2006.csv")

institutions<-data$Sponsor.Collaborators

write.csv(institutions,"USinstitutions.csv")

data1<-read.csv("USinstitutions.csv", header=FALSE)

length(unique(data1$V1))


#count operations on ALL BIMO firms (ORADSS)
data<-read.csv("BIMO_all.csv")
hist(data$Count_of_Operations, breaks = 10000, xlim=c(0, 500))
data1<-subset(data, data$Count_of_Operations==1)
data0<-subset(data, data$Count_of_Operations==0)

