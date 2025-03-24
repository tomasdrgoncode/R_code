#assuming element weightequal
data<-read.table("st_weights.txt")

data$percent<-data$V2/sum(data$V2)*100
barplot(data$percent, names.arg=data$V1, ylim=c(0, 20))

#assuming standards weight equal

data<-read.table("el_weights.txt")
barplot(data$V2, names.arg=data$V1)

#model based on KS report

elements<-scan("elements.txt", what="", sep="\n")
states<-scan("states.txt", what="", sep="\n")

a<-c(rep(1, times=3500), rep(0, times=7100))

perfect<-c(rep(0, times=106))

AL<-sample(a, 106)
AK<-sample(a, 106)
AZ<-sample(a, 106)
AR<-sample(a, 106)
CA<-sample(a, 106)
CO<-sample(a, 106)
CT<-sample(a, 106)
DE<-sample(a, 106)
FL<-sample(a, 106)
GA<-sample(a, 106)
HI<-sample(a, 106)
ID<-sample(a, 106)
IL<-sample(a, 106)
IN<-sample(a, 106)
IA<-sample(a, 106)
KS<-sample(a, 106)
KY<-sample(a, 106)
LA<-sample(a, 106)
ME<-sample(a, 106)
MD<-sample(a, 106)
MA<-sample(a, 106)
MI<-sample(a, 106)
MN<-sample(a, 106)
MS<-sample(a, 106)
MO<-sample(a, 106)
MT<-sample(a, 106)
NE<-sample(a, 106)
NV<-sample(a, 106)
NH<-sample(a, 106)
NJ<-sample(a, 106)
NM<-sample(a, 106)
NY<-sample(a, 106)
NC<-sample(a, 106)
ND<-sample(a, 106)
OH<-sample(a, 106)
OK<-sample(a, 106)
OR<-sample(a, 106)
PA<-sample(a, 106)
RI<-sample(a, 106)
SC<-sample(a, 106)
SD<-sample(a, 106)
TN<-sample(a, 106)
TX<-sample(a, 106)
UT<-sample(a, 106)
VT<-sample(a, 106)
VA<-sample(a, 106)
WA<-sample(a, 106)
WV<-sample(a, 106)
WI<-sample(a, 106)
WY<-sample(a, 106)

data<-data.frame(AL,AK,AZ,AR,CA,CO,CT,DE,FL,GA,HI,ID,IL,IN,IA,KS,KY,LA,ME,MD,MA,MI,MN,MS,MO,MT,NE,NV,NH,NJ,NM,NY,NC,ND,OH,OK,OR,PA,RI,SC,SD,TN,TX,UT,VT,VA,WA,WV,WI,WY,perfect, row.names=elements)

plot(hclust(dist(t(data))))

states1<-states[-51]


distances<-dist(t(data))
distances<-as.matrix(distances)
distances<-distances[51,]
distances<-data.frame(distances)
fix(distances)

distances<-distances[order(distances$col1)]

