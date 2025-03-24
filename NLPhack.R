testdf<-data.frame(test1)

library(stringr)

# Split test1rm column into 6 variables 
testdf[c('var1', 'var2', 'var3', 'var4', 'var5', 'var6')] <- str_split_fixed(testdf$test1, ';', 6)