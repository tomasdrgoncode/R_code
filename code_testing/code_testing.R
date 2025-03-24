
#objects

a<-1

x<-c(1,2,3,4,5)

xsq<-x^2

plot(x,xsq, type = "b")

my_data<-data.frame(x,xsq)

my_data$x


x<-c("a", "C", "b")
x_sorted<-sort(x)

x<-c("c", 3, 1, "a")



#sorts


x<-c(1:20) #vector of numbers for the list
l<-sample(x,replace = FALSE) #randomized order of x
print(l)




#linear switching

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
}








#Monte Carlo
y<-c(1:(length(x)-1)) #one shortet than x to prevent out of bounds call...

for (i in 1:10) {
  
  j<-sample(y,1) #this is the Monte Carlo part...
  
  if (l[j]>l[j+1]) {
    big<-l[j]
    small<-l[j+1]
    l[j]<-small
    l[j+1]<-big
  }
  print(l)
}

print (i)
print(l)

