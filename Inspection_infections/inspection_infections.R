x<-read.delim("investigators.txt", header=TRUE, sep="\t")
hist(x$Count_of_Inspections)


#Infections in 2 week increments (11 infectious investigators per 2 weeks)



r<-3
rl<-1.05

#Approach, no lockdown vs full lockdown
#for each incubation period assume R or Rl transmissions

infections_R3<-(11*r^12)+(11*r^11)+(11*r^10)+(11*r^9)+(11*r^8)+(11*r^7)+(11*r^6)+(11*r^5)+(11*r^4)+(11*r^3)+(11*r^2)+(11*r)
infections_l<-(11*rl^12)+(11*rl^11)+(11*rl^10)+(11*rl^9)+(11*rl^8)+(11*rl^7)+(11*rl^6)+(11*rl^5)+(11*rl^4)+(11*rl^3)+(11*rl^2)+(11*rl)

effect1<-(infections_R3 - infections_l)



#table for r and seed maps in case distribution not uniform

ip<-c(1,2,3,4,5,6,7,8,9,10,11,12,13) #incubation periods
seed<-c(11,11,11,11,11,11,11,11,11,11,11,11,11) #12 incubation periods
r<-c(3,3,3,3,3,3,1,1,1,1,1,1,1) #R during uncontrolled growth (adjusted for hypothetical herd immunity)
rl<-c(1.05,1.05,1.05,1.05,1.05,1.05,1.05,1.05,1.05,1.05,1.05,1.05,1.05) #R during lockdown
ipl<-c(13,12,11,10,9,8,7,6,5,4,3,2,1) #incubation periods left

mydf<-data.frame(ip,seed,r,rl,ipl)

mydf$scen1r<-seed*r^ipl #scenario 1.... number of infected - uncontrolled growth
mydf$scen1rl<-seed*rl^ipl #scenario 1 .... number of infected - all lockdown

effect1<-sum(mydf$scen1r)-sum(mydf$scen1rl) # number of infections prevented
effect1deaths<-effect1/100 #number of deaths prevented
effect1dollars<-effect1deaths*10000000 # death times $10m for statistical life



plot(mydf$ip, mydf$scen1r, type="b")
plot(mydf$ip, log(mydf$scen1r), type="b")


#plot(mydf$ip, cumsum(mydf$scen1r), type="b", ylim=c(0,max(cumsum(mydf$scen1r))))
#lines(mydf$ip, cumsum(mydf$scen1rl), type="b")





# better accounting for herd immunity with % population infected
# herd immunity not implemented yet


n<-14 #initial number of infected
USpop <- 328200000 #US population

incubp <- 50 # number of incubation periods February through November
r <- 3 # R number of the virus.... number of people that one infected individual infects during illness
rm <- r #set initial rm to r
ninfected <- vector(mode="numeric", length=0)
rtracking <- vector(mode="numeric", length=0)

for (i in seq(1:incubp)) {
 n <- n*rm #R modified by % non-infected population
 percentpop <- (1-n/32820000)
 rm <- r*percentpop
 ninfected <- append(ninfected,n)
 rtracking <- append(rtracking,rm)
}

plot(rtracking, type="b")
plot(ninfected, type="b")
