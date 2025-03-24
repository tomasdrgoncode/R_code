
#data:
x <- c("no", "no", "yes", "yes", "yes", "no", "yes", "no", "yes", "yes", "yes", "yes", "yes", "no")
x<-c(1,1,1,1,1,1,1,0)

freqs <- table(x)/length(x)

# calculate shannon-entropy
-sum(freqs * log2(freqs))

#or
library("entropy", lib.loc="~/R/win-library/3.3")
entropy.empirical(freqs, unit="log2")