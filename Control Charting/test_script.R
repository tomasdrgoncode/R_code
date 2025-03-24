library(Rspc)
# generate some data
data <- c(rnorm(100)) 
data<-data^2
# evaluate all the Nelson rules, 
# calculate control limits from data (lcl, cl, ucl are not provided) using formulas for i-chart, 
# don't modify any rule parameters (parRules = NULL)
result<-EvaluateRules(x = data, type = 'i', whichRules = 1:8, lcl = NA, cl = NA, ucl = NA)
#The vector 'x' is the only compulsory parameter, default values for the rest is as above. 

plot(result$x)
grid(nx= NA, ny = NULL, col = "black", lty = "solid",
     lwd = par("lwd"), equilogs = TRUE)
