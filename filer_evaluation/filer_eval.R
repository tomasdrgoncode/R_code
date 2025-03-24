
# generate sample (lines with no mistakes = 0, lines with mistakes = 1 (2, 3?) etc"

a<-c(rep (0, times=90000), rep (1, times=10000))

# zero out vectors
result<-vector(mode="numeric", length=0)

b<-vector(mode="numeric", length=0)
sampling<-vector(mode="numeric", length=0)



# loop through the sampling strategies

for (j in seq(from=0, to=100, by=10)) { #loop through sampling strategies from 0 samples to all samples in a
  print (j)
  sampling<-append(sampling, j)
  negative<-0
  positive<-0
  
  for (i in 1:10000) { # loop through trials of each sampling strategy  

    b<-sample(a, (j))
    
    if (sum(b)==0) {
      negative<-negative+1
    }

    else {
      positive<-positive+1
    } 

  }
  print(negative)
  result<-append(result, (negative/(positive+negative))) 
}

print(result) 
print(sampling)

#plot(sampling, result, type="l",xlim=c(0,200), ylim=c(0,1))

par(new=TRUE)
plot(sampling, result, type="l", col="red", xlim=c(0,200), ylim=c(0,1))


filer<-read.table("filer_eval.csv", header = TRUE) # read csv output from ORADSS

filer1<-subset(filer, row.numbers=!0)

me<-mean(filer$Entries)
mee<-sd(filer$Entries)
le<-mean(filer$Lines)
dle<-mean(filer$DLines)
lee<-sd(filer$Lines)
dlee<-sd(filer$DLines)
x<-c(me,le,dle)
barplot(x)
