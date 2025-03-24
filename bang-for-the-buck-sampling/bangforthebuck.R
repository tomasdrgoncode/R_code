#empirical - monte carlo
#sampling from 0 and 1

result<-vector(mode="numeric", length=0)

for (j in 0:100) {   
  a<-sum(sample(c(0,1), 10, replace = TRUE))
  result<-append(result,a)
  
}

hist(result)

resultn<-rnorm(1000)




#equation for binomial sampling
#gives probability of missing a single hit given frequency of outcome

c<-0.95 #confidence
p<-c(0.01,0.05,0.1,0.2,0.3) #prevalence
x<-log(1-c)/log(1-p) #of samples
x

# with correction for finite population
N<-100
xf<-(x*N)/(x+(N-1))
xf<-round(xf, digits=0)

result<-data.frame(p,xf)
names(result)[1]<-paste("Anticipated portion of bad recalls")
names(result)[2]<-paste("Number of samples")

View(result, paste(N," cosignees, ", "Conf = ", c))

plot (x,p, type = "b")



prevalence<-0.5
sampling<-10
probability<-(1-(1-(prevalence))^(sampling))
probability




#--------------------------------------------------------------------------
#Monte Carlo model

#the question: what is the likelyhood that I will get at least one adulterated unit from the set of boxes
#strategy 1: pick a set of boxes, pick random samples from boxes
#strategy 2: pick random samples from the whole set of all samples from all boxes

#proposition: all sampling strategies yield the same probability of catching an adulterated unit in the long run
#caveat: random sampling from boxes may need to be corrected for finite population

#Make boxes
box1<-c(rep(0, times=90), rep(1, times=10))
box2<-c(rep(0, times=90), rep(1, times=10))
box3<-c(rep(0, times=99), rep(1, times=1))
box4<-c(rep(0, times=99), rep(1, times=1))
box5<-c(rep(0, times=99), rep(1, times=1))
box6<-c(rep(0, times=99), rep(1, times=1))
box7<-c(rep(0, times=99), rep(1, times=1))
box8<-c(rep(0, times=99), rep(1, times=1))
box9<-c(rep(0, times=99), rep(1, times=1))
box10<-c(rep(0, times=99), rep(1, times=1))

#make list of box names
boxes<-c("box1", "box2", "box3", "box4", "box5", "box6", "box7", "box8", "box9", "box10")

#make global object placeholders
strategy1<-0
strategy2<-0
result_strategy1<-vector(mode="numeric", length=0)
result_strategy2<-vector(mode="numeric", length=0)

#set the number of samples per box
nofboxes<-6
nofsamples<-1
totsamples<-nofboxes*nofsamples

#number of trials
ntrials<-1000

#replacement
#repl<-"FALSE"  does not work

#---------------------------------------------------------------------------
#big loop starts here

for (k in 1:10) { # number of super-trials

  #make local object placeholders
  strategy1<-0
  strategy2<-0

#----------------------------------------------------------------------------
# first sampling strategy starts here
  
  for (j in 1:ntrials){

    #randomly pull a subset of box names
    selection<-sample(boxes, nofboxes, replace=FALSE)

    print(k)
    print(j) #print a counter
  
    for(i in 1:nofboxes){

          x<-(sample(get(selection[i]), nofsamples,replace=FALSE)) #sample selected box
  
      
      if (sum(x) > 0) {               #if sample contains 1, add 1 to strategy and break loop
          strategy1<-strategy1 + 1
          break
          }
    }

  }  
  #print (strategy1)

  #add outcome of super-trial to results
  result_strategy1<-append(result_strategy1, strategy1)

  #------------------------------------------------------------------
  #strategy 2 starts here - random sampling

  # put all units from all boxes into one
  boxes2<-c(box1, box2, box3, box4, box5 , box6, box7, box8, box9, box10) 

  for (j in 1:ntrials){
  
      x<-(sample(boxes2, totsamples, replace=FALSE)) #sample the combined set of units from all boxes

            if (sum(x) > 0) {               #if sample contains 1, add 1 to strategy2
        strategy2<-strategy2 + 1
    }
  
  }  
  #print (strategy2)
  
  #add outcome of super-trial to results 
  result_strategy2<-append(result_strategy2, strategy2)

}

#create max for scaling of the plot
scaling<-c(result_strategy2, result_strategy1)
scaling<-scaling/ntrials
maximum<-max(scaling)

#boxplot the two strategies (this overwrites the original strategy1 and 2 objects)
strategy1<-result_strategy1/ntrials
strategy2<-result_strategy2/ntrials

boxplot(strategy1, strategy2, ylim=c(0,maximum), names=c("strategy1","random"))

print("strategy1")
print(strategy1)
print("random")
print(strategy2)

#test difference
t.test(result_strategy1, result_strategy2)





