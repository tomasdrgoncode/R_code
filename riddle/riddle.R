#raw monte carlo

for (i in c(1:100000000)) {
  
# print(i)  
  
C <- sample(0:9, 1)
A <- sample(0:9, 1)
E <- sample(0:9, 1)
S <- sample(0:9, 1)
R <- sample(0:9, 1)

I <- sample(0:9, 1)
D <- sample(0:9, 1)

caesars <- as.numeric(paste0(C,A,E,S,A,R,S))
ides <- as.numeric(paste0(I,D,E,S))


 if (caesars == ides^R) {
   print ("trial #:")
   print(i)
   print ("CAESARS")
   print (caesars)
   print ("IDES")
   print (ides)
   print ("R")
   print (R)
   break
 }
}


#with additional info (R = 2)

for (i in c(1:100000000)) {
   
   # print(i)  
   
   C <- sample(c(0,1,3,4,5,6,7,8,9), 1)
   A <- sample(c(0,1,3,4,5,6,7,8,9), 1)
   E <- sample(c(0,1,3,4,5,6,7,8,9), 1)
   S <- sample(c(0,1,3,4,5,6,7,8,9), 1)
   R <- 2
   
   I <- sample(c(0,1,3,4,5,6,7,8,9), 1)
   D <- sample(c(0,1,3,4,5,6,7,8,9), 1)
   
   caesars <- as.numeric(paste0(C,A,E,S,A,R,S))
   ides <- as.numeric(paste0(I,D,E,S))
   
   
   if (caesars == ides^R) {
      print ("trial #:")
      print(i)
      print ("CAESARS")
      print (caesars)
      print ("IDES")
      print (ides)
      print ("R")
      print (R)
      break
   }
}



