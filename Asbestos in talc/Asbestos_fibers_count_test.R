#asbestos in talc
#rules for regulatory decision: 1 blank has to have 0 fibers, 3 replicates have to have at least 4 fibers, with at least 2 replicates having at least 1 fiber
#this loop generates all possible distributions of 4 fibers across "blank + 3 replicates"



x<-c(0,1,2,3) #choices for # of fibers per replicate


s1<-c(1,1,1,1) #dummy file seed object for results

for (i in 1:10000) { #10000 is arbitrary to make sure the choices are saturated

s<-sample(x, 4, replace = TRUE) 

  if ((sum(s)) == 4) {print(s) #check if total is 4
    s1<-rbind(s, s1) #add to seed
    } 

}



s1<-unique(s1) #remove duplicates
s1<-data.frame(s1) #make into data frame for sorting


result <-s1[order(s1$X1, s1$X2, s1$X3, s1$X4),] #sort by columns (to get 0 in first column (blank) on top)
row.names(result)<-NULL #remove row names


#---------
#---------
#generate all possible outcomes


result<-c(0,0,0,0)

for (i in 0:3) {
  for (j in 0:3) {
    for (k in 0:3) {
      for (l in 0:3) {
        
       if (sum(c(i,j,k,l)) == 4) {result<-rbind(result, c(i,j,k,l))} 
        
        
      }
    }
  }
}

row.names(result)<-NULL #remove row names


