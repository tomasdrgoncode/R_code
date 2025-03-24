library("stringdist", lib.loc="~/R/win-library/3.3")


a<-c("Old Dominon rd Ontario Canada")
b<-("olddominionrd Ontario Canada")
c<-("Old Dominion rd ON CA")
d<-("Main St, Athens, Georgia")
ab<-stringdist(a, b)
ac<-stringdist(a,c)
ad<-stringdist(a,d)
bc<-stringdist(b,c)
bd<-stringdist(b,d)
cd<-stringdist(c,d)
data1<-c(ab,ac,ad,bc,bd,cd)
print(data1)



library("fuzzywuzzyR", lib.loc="~/R/win-library/3.3") #this is a python library ported to R....


word = "new york jets"

choices = c("Atlanta Falcons", "New York Jets", "New York Giants", "Dallas Cowboys")

GetCloseMatches(word, choices)
Ratio(word, choices)
