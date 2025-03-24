#Twitter mining
#http://www.rdatamining.com/examples/text-mining
#Consumer Key (API Key) 4yWGP5eGfNWnjFFz5b09aEfSs
#Consumer Secret (API Secret) T6Gybdi71IQJuxSKj7OCOyDDHTePIA0HwEtg5l9tqT5dQv4la9 
#Access token:  3154735013-EAOJDVXu7SpptWdssBdECbUNQLrPIERDZ0XJVxA
#Access token secret: 7o8YnNztbwb2rcO9JeLPRwrk2AoUWnlf8VKuXtI0qYG45

install.packages(c("devtools", "rjson", "bit64", "httr"))

#RESTART R session!

setwd("C:/Users/Tomas.Drgon/Desktop/r_projects/textmining/twitterCorpus") #working directory

library(devtools)
library("twitteR")
library(SnowballC)
library("tm")
library("wordcloud")

setup_twitter_oauth("4yWGP5eGfNWnjFFz5b09aEfSs","T6Gybdi71IQJuxSKj7OCOyDDHTePIA0HwEtg5l9tqT5dQv4la9","3154735013-EAOJDVXu7SpptWdssBdECbUNQLrPIERDZ0XJVxA","7o8YnNztbwb2rcO9JeLPRwrk2AoUWnlf8VKuXtI0qYG45")


text <-searchTwitter("from:realDonaldTrump", n=100)
text <-searchTwitter("kratom", n = 100)
#text <-searchTwitter("from:potus", n=100)


sources<-sapply(text,function(x)x$getStatusSource())
sources<-gsub("</a>","",sources)
sources<-strsplit(sources,">")
sources<-sapply(sources,function(x)ifelse(length(x)>1,x[2],x[1]))
source_table=table(sources)
pie(source_table[source_table>10])

#create a data frame from tweets
df <- do.call("rbind", lapply(text, as.data.frame))
dim(df)

write.csv(df, file="text.csv")
write.csv(df$text, file="textonly.csv")


text<-toString(df$text)
text <- Corpus(VectorSource(text))
text <- tm_map(text, PlainTextDocument)
text <- tm_map(text, removePunctuation)
text <- tm_map(text, removeWords, c(stopwords('english'), 'murder'))
text <- tm_map(text, stemDocument)

wordcloud(text, min.freq=100)
wordcloud(text, max.words=400)


#put tweets on the map......not working yet

long<-df$longitude
lat<-df$latitude
points<-data.frame(long,lat) #this makes a data frame from the geo info from the tweets

