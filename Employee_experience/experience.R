#experience as # of hours per year

Ex<-read.table("Experience_hours.csv", sep=",", header=TRUE)

Ex<-as.data.frame(Ex)
Ex[is.na(Ex)]<-0 #replace NA with 0

#dataframe with cummulative sums per year
E2001<-Ex$X2000+Ex$X2001
E2002<-E2001+Ex$X2002
E2003<-E2002+Ex$X2003
E2004<-E2003+Ex$X2004
E2005<-E2004+Ex$X2005
E2006<-E2005+Ex$X2006
E2007<-E2006+Ex$X2007
E2008<-E2007+Ex$X2008
E2009<-E2008+Ex$X2009
E2010<-E2009+Ex$X2010
E2011<-E2010+Ex$X2011
E2012<-E2011+Ex$X2012
E2013<-E2012+Ex$X2013
E2014<-E2013+Ex$X2014
E2015<-E2014+Ex$X2015
E2016<-E2015+Ex$X2016

ExperienceHrs<-data.frame(Ex$name, Ex$ID, Ex$X2000, E2001, E2002, E2003, E2004, E2005, E2006, E2007, E2008, E2009, E2010, E2011, E2012, E2013, E2014, E2015, E2016)
ExperienceHrs<-ExperienceHrs[order(ID),]

# experience as # of operations
Ex<-read.table("Experience_operations.csv", sep=",", header=TRUE)

Ex<-as.data.frame(Ex)
Ex[is.na(Ex)]<-0 #replace NA with 0

#dataframe with cummulative sums per year
E2001<-Ex$X2000+Ex$X2001
E2002<-E2001+Ex$X2002
E2003<-E2002+Ex$X2003
E2004<-E2003+Ex$X2004
E2005<-E2004+Ex$X2005
E2006<-E2005+Ex$X2006
E2007<-E2006+Ex$X2007
E2008<-E2007+Ex$X2008
E2009<-E2008+Ex$X2009
E2010<-E2009+Ex$X2010
E2011<-E2010+Ex$X2011
E2012<-E2011+Ex$X2012
E2013<-E2012+Ex$X2013
E2014<-E2013+Ex$X2014
E2015<-E2014+Ex$X2015
E2016<-E2015+Ex$X2016

ExperienceOps<-data.frame(Ex$name, Ex$ID, Ex$X2000, E2001, E2002, E2003, E2004, E2005, E2006, E2007, E2008, E2009, E2010, E2011, E2012, E2013, E2014, E2015, E2016)


#experience as years with entered data

Ex<-read.table("Experience_operations.csv", sep=",", header=TRUE)

Ex<-as.data.frame(Ex)
Ex[is.na(Ex)]<-0 #replace NA with 0

ID<-Ex$ID #extract ID before replacing
name<-Ex$name #extract name before replacing

Ex<-Ex[ -c(1,2)] #remove name and ID before replacing

Ex <- as.data.frame(lapply(Ex, function(x){replace(x, x >0,1)})) #replace all non-zero numbers with "1"
Ex$name<-name #add back name
Ex$ID<-ID #add back ID
Ex<-Ex[c(18,19,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)] #get name and ID to the front
#Ex is now 0 or 1 depending if person entered data or not


#dataframe with cummulative sums per year
E2001<-Ex$X2000+Ex$X2001
E2002<-E2001+Ex$X2002
E2003<-E2002+Ex$X2003
E2004<-E2003+Ex$X2004
E2005<-E2004+Ex$X2005
E2006<-E2005+Ex$X2006
E2007<-E2006+Ex$X2007
E2008<-E2007+Ex$X2008
E2009<-E2008+Ex$X2009
E2010<-E2009+Ex$X2010
E2011<-E2010+Ex$X2011
E2012<-E2011+Ex$X2012
E2013<-E2012+Ex$X2013
E2014<-E2013+Ex$X2014
E2015<-E2014+Ex$X2015
E2016<-E2015+Ex$X2016

ExperienceYears<-data.frame(Ex$name, Ex$ID, Ex$X2000, E2001, E2002, E2003, E2004, E2005, E2006, E2007, E2008, E2009, E2010, E2011, E2012, E2013, E2014, E2015, E2016)


#remove data from years when person did not enter data
# 1. remove name and ID before replacing
Ex<-Ex[ -c(1,2)] 
ExperienceYears<-ExperienceYears[-c(1,2)]
ExperienceHrs<-ExperienceHrs[-c(1,2)]
ExperienceOps<-ExperienceOps[-c(1,2)]

# 2. multiply by Ex to null out years with no entry
ExperienceYears<-Ex*ExperienceYears
ExperienceOps<-Ex*ExperienceOps
ExperienceHrs<-Ex*ExperienceHrs


#histogram per year
x<-subset(ExperienceYears, ExperienceYears$X2005>0)

hist(x$X2002, breaks = 2, xlim = c(1,17))

#number of employees with 1 year of experience each year
x<-(ExperienceYears)

a<-table(x$X2001)
a[names(a)==1]
a<-table(x$X2002)
a[names(a)==1]
a<-table(x$X2003)
a[names(a)==1]
a<-table(x$X2004)
a[names(a)==1]
a<-table(x$X2005)
a[names(a)==1]
a<-table(x$X2006)
a[names(a)==1]
a<-table(x$X2007)
a[names(a)==1]
a<-table(x$X2008)
a[names(a)==1]
a<-table(x$X2009)
a[names(a)==1]
a<-table(x$X2010)
a[names(a)==1]
a<-table(x$X2011)
a[names(a)==1]
a<-table(x$X2012)
a[names(a)==1]
a<-table(x$X2013)
a[names(a)==1]
a<-table(x$X2014)
a[names(a)==1]
a<-table(x$X2015)
a[names(a)==1]
a<-table(x$X2016)
a[names(a)==1]

year<-c(2001:2016)
new_empl<-c(191,584,358,121,77,56, 73, 217, 588, 323, 208, 84, 123, 159, 293, 399 )

barplot(new_empl, names.arg=year)

