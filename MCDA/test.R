

# ----------------------------------------
# ranking some cars (from original article on UTA by Siskos and Lagreze, 1982)
# the separation threshold

# the performance table
data <- rbind(
  c(173, 11.4, 10.01, 10, 7.88, 49500),
  c(176, 12.3, 10.48, 11, 7.96, 46700),
  c(142, 8.2, 7.30, 5, 5.65, 32100),
  c(148, 10.5, 9.61, 7, 6.15, 39150),
  c(178, 14.5, 11.05, 13, 8.06, 64700),
  c(180, 13.6, 10.40, 13, 8.47, 75700),
  c(182, 12.7, 12.26, 11, 7.81, 68593),
  c(145, 14.3, 12.95, 11, 8.38, 55000),
  c(161, 8.6, 8.42, 7, 5.11, 35200),
  c(117, 7.2, 6.75, 3, 5.81, 24800),
  c(150, 10.0, 8.0, 5, 8, 35000), #this is the ideal car (low)
  c(170, 12.0, 10.0, 8, 11, 39000) #this is the ideal car (high)
)
rownames(data) <- c(
  "Peugeot 505 GR",
  "Opel Record 2000 LS",
  "Citroen Visa Super E",
  "VW Golf 1300 GLS",
  "Citroen CX 2400 Pallas",
  "Mercedes 230",
  "BMW 520",
  "Volvo 244 DL",
  "Peugeot 104 ZS",
  "Citroen Dyane", 
  "IDEAL L", #added ideal car (low)
  "IDEAL H") #added ideal car (high)
colnames(data) <- c(
  "MaximalSpeed",
  "ConsumptionTown",
  "Consumption120kmh",
  "HP",
  "Space",
  "Price")
# ranks of the alternatives



#plotting data to see where the ideal car is, distances approach
plot(hclust(dist(data)))
PCA(data) #


pc<-princomp(data)

library(rgl) #not sure where this package is.....
plot3d(pc$scores[,1:3])


#MCDA from http://www.decision-deck.org/r/tutorial.html#tutorial-r-mcda

f <- system.file("datasets","performanceTable2.csv",package="MCDA")

pT <- read.csv(file = f, header=TRUE, row.names=1)

# filter out cars which do not
# respect Thierry's initial rules

fPT <- pT[(pT$g4>=2 & pT$g5>=2 & pT$g2 < 30), ]

# drop car a14 from the table

fPT <- fPT[!(rownames(fPT) %in% "a14"), ]
fPT #print table



criteriaMinMax <- c("min","min","min","max","max")

names(criteriaMinMax) <- colnames(pT)

plotRadarPerformanceTable(fPT, criteriaMinMax, overlay=FALSE, bw=TRUE, lwd =5)


par(mfrow=c(2,3))
for (i in 1:dim(pT)[2]){
  yaxis <- range(pT[,i])*c(0.99,1.05)
  if (criteriaMinMax[i] =="min")
    oPT <- pT[order(pT[,i],decreasing=FALSE),]
  else
    oPT <- pT[order(pT[,i],decreasing=TRUE),]
  name <-paste(colnames(pT)[i]," (",criteriaMinMax[i],")", sep="")
  barplot(oPT[,i], main=name, names.arg = rownames(oPT),
          density = i*10, ylim = yaxis, xpd=FALSE)
}

# normalization of the data from the performance table

normalizationTypes <- c("percentageOfMax","percentageOfMax","percentageOfMax",
                        "percentageOfMax","percentageOfMax")

names(normalizationTypes) <- c("g1","g2","g3","g4","g5")

nPT <- normalizePerformanceTable(pT,normalizationTypes)

w <- c(-1,-2,-1,0.5,0.5)
names(w) <- colnames(pT)
ws<-weightedSum(nPT,w)

# rank the scores of the alternatives
rank(-ws)

# add supplementary car to pT

missing <- c(16966,30,37.7,2.33,3.25)
pT<-rbind(pT,missing)
rownames(pT)[14] <- "a10"

nPT <- normalizePerformanceTable(pT,normalizationTypes)
ws<-weightedSum(nPT,w)

# rank the scores of the alternatives
rank(-ws)

# ranks of the alternatives

alternativesRanks <- c(1,2,3,4,5)
names(alternativesRanks) <- c("a11","a03","a13","a09","a14")

# number of break points for each criterion : 1 segment = 2 breakpoints

criteriaNumberOfBreakPoints <- c(2,2,2,2,2)
names(criteriaNumberOfBreakPoints) <- colnames(pT)

# lower and upper bounds of the criteria for the determination of value functions

criteriaLBs=apply(pT,2,min)
names(criteriaLBs) <- colnames(pT)

criteriaUBs=apply(pT,2,max)
names(criteriaUBs) <- colnames(pT)

# the separation threshold

epsilon <-0.01

# UTA elicitation

x<-UTA(pT, criteriaMinMax,
       criteriaNumberOfBreakPoints, epsilon,
       alternativesRanks = alternativesRanks,
       criteriaLBs = criteriaLBs, criteriaUBs = criteriaUBs)

x

# plot the piecewise linear value functions

plotPiecewiseLinearValueFunctions(x$valueFunctions)

# ranks of the alternatives for the second try

alternativesRanks <- c(1,2,3,4,5,6,7)
names(alternativesRanks) <- c("a11","a03","a08","a04","a13","a09","a14")

x2<-UTA(pT, criteriaMinMax,
        criteriaNumberOfBreakPoints, epsilon,
        alternativesRanks = alternativesRanks,
        criteriaLBs = criteriaLBs, criteriaUBs = criteriaUBs)


# plot the piecewise linear value functions

plotPiecewiseLinearValueFunctions(x2$valueFunctions)

# apply the value functions on the original performance table

tPT <- applyPiecewiseLinearValueFunctionsOnPerformanceTable(
  x2$valueFunctions,
  pT
)
# calculate the overall score of each alternative

mavt <- weightedSum(tPT,rep(1,5))

plotAlternativesValuesPreorder(mavt, decreasing=TRUE)

