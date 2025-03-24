
library(SnowballC)
library("tm")
library("wordcloud")
library("readxl")

mydata<-read_excel("Docket_Comments_FMQ.xlsx")


text <- data.frame(text=mydata$Comment, stringsAsFactors=FALSE)

text <- Corpus(VectorSource(mydata$Comment))
#text <- tm_map(text, PlainTextDocument)
text <- tm_map(text, removeWords, c(stopwords('english')))
text <- tm_map(text, removePunctuation)
text <- tm_map(text, stemDocument)

wordcloud(text, min.freq=5)
wordcloud(text, max.words=400)


#generate DTM 
dtm <- DocumentTermMatrix(text, control=list(wordLengths=c(4, Inf)))

terms<-colnames(dtm)

#dtm with custom dictionary
dtm<-DocumentTermMatrix(text, list(dictionary=c('blood', 'diabetes', 'bleeding', 'fatigue', 'hypoglycemia', 'palpitations', 'uterine', 'coronary', 'embolism', 'graft', 'ischaemic', 'infarction', 'necrosis', 'kidney', 'injury', 'renal')))
#prepare for clustering
dtm <- removeSparseTerms(dtm, 0.4) # This makes a matrix that is only XX% empty space, maximum.   

dtm_mod<-dtm
dtm_mod$sum<-rowsum(dtm)


#????
#inspect DTM
freqs <- as.matrix(dtm)
colSums(freqs)


fix(freqs)



#visualize as dendrogram
library(cluster)   
d <- dist(dtm)  
plot(hclust(d), cex=0.5)

heatmap(as.matrix(dtm))

#visualize as PC
plot(prcomp(as.matrix(dtm)))
biplot(prcomp(as.matrix(dtm)))
biplot(prcomp(as.matrix(dtm)), ylim=c(-0.3,0.1))

#visualize topics
plot(princomp(as.matrix(t(dtm))))
biplot.princomp(as.matrix(t(dtm)))

library("FactoMineR")
PCA(as.matrix(dtm), axes=c(1,2))
PCA(as.matrix(dtm), axes=c(2,3))
PCA(as.matrix(dtm), axes=c(3,4))
PCA(as.matrix(dtm), axes=c(4,5))

#look at the values of the new dimensions for each observation
#and the contribution of each original variable to the new dimension
summary.PCA(PCA(as.matrix(dtm)), nbelements = Inf )


options(max.print=999999)
output<-capture.output(summary.PCA(PCA(as.matrix(dtm)), nbelements = Inf ))

fix(output)


