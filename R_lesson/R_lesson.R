library(tidyverse)

#objects

a<-1

x<-c(1,2,3,4,5)

xsq<-x^2

intro<-"Hello World"
intro

plot(x,xsq, type = "b")
plot(x,xsq)

my_data<-data.frame(x,xsq)

my_data$x

my_data$x[2]

x<-c("a", "C", "b")
x<-c("c", 3, 1, "a")
x_sorted<-sort(x)


#sorts


x<-c(1:20) #vector of numbers for the list
l<-sample(x,replace = FALSE) #randomized order of x
print(l)




#sort through linear switching

switch=TRUE #set the switch to true
print(l)
for (i in 1:(length(x))) {
  if (switch==FALSE) break #if no switch in the last round then break
  switch=FALSE #set switch to false to start
  
      for (j in 1:(length(x)-1)) {
    
      if (l[j]>l[j+1]) { #test pair and switch if needed
        big<-l[j]
        small<-l[j+1]
        l[j]<-small
        l[j+1]<-big
        switch=TRUE #record the switch
      } 
    }
    print(l)
}

print(l)
print(i)

#as a function


my_function <-function(l) {
  switch=TRUE #set the switch to true
  print(l)
  for (i in 1:(length(l))) {
    if (switch==FALSE) break #if no switch in the last round then break
    switch=FALSE #set switch to false to start
  
    for (j in 1:(length(l)-1)) {
    
      if (l[j]>l[j+1]) { #test pair and switch if needed
        big<-l[j]
        small<-l[j+1]
        l[j]<-small
        l[j+1]<-big
        switch=TRUE #record the switch
      } 
    }
    print(l)
  }

  print(l)
#  print(i)
}

result<-my_function(l)




#positive - negative function
check <- function(x) {
  if (x > 0) {
    result <- "Positive"
  }
  else if (x < 0) {
    result <- "Negative"
  }
  else {
    result <- "Zero"
  }
return(result)
#print(result)
  }

check(1)
check(-10)
check(0)

output<-check(1)