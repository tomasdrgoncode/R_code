library(readxl)
Summaries <- read_excel("reasons_short.xlsx")
Summaries$ID<-as.character(Summaries$ID)

doc_id<-Summaries$ID
text<-Summaries$Reason

mydataframe<-data.frame(doc_id, text)

fix(mydataframe)

#makes corpus from the Summaries vector
#or jump to tokenization

library(tm)
textCorpus<-SimpleCorpus(DataframeSource(mydataframe))

#textCorpus<-VCorpus(DataframeSource(mydataframe))

textCorpus <- tm_map(textCorpus, removeWords, stopwords("english"))
textCorpus <- tm_map(textCorpus, stemDocument, language = "english")  
textCorpus <- tm_map(textCorpus, removePunctuation)
textCorpus <- tm_map(textCorpus, removeNumbers)

inspect(textCorpus)


#generate DTM 
dtm <- DocumentTermMatrix(textCorpus, control=list(wordLengths=c(2, 10)))
dtm <- DocumentTermMatrix(textCorpus, control=list(global=c(200, 300)))


#or DTM with engrams...
library(tm); library(tau);
tokenize_ngrams <- function(x, n=5) return(rownames(as.data.frame(unclass(textcnt(x,method="string",n=n)))))

textCorpus <- Corpus(VectorSource(text))
dtm <- DocumentTermMatrix(textCorpus,control=list(tokenize=tokenize_ngrams))

#Warning messages:
#  1: In TermDocumentMatrix.SimpleCorpus(x, control) :
#  custom functions are ignored
#2: In TermDocumentMatrix.SimpleCorpus(x, control) :
#  custom tokenizer is ignored

#or DTM with custom dictionary
dtm <- DocumentTermMatrix(textCorpus, list(dictionary=c('term1', 'term2', 'etc')))
                                                        
                                                        
dtm<-removeSparseTerms(dtm, 0.95)

#inspect DTM
terms<-dtm$dimnames
summary(terms)
                                                        
#find frequent terms 
frequentterms<-findFreqTerms(dtm, lowfreq = 50, highfreq = Inf)
frequentterms
length(frequentterms)
       
#DTM with just the frequentterms
dtm <- DocumentTermMatrix(textCorpus, list(dictionary=frequentterms))
    
#frequency of terms in corpus
words<-as.data.frame(rowSums(as.matrix(t(dtm))))
wordfreq<-data.frame(row.names(words), words$`rowSums(as.matrix(t(dtm)))`)
colnames(wordfreq) <- c("word","frequency")
     
wordfreq<-wordfreq[order(wordfreq$frequency, decreasing = FALSE), ]
fix(wordfreq)

write.table(wordfreq, file = "term_frequency.txt")
    
library(wordcloud)
wordcloud(wordfreq$word, wordfreq$frequency, min.freq=50 ,max.words=300) 

                                                        
library(data.table) # write terms into file
fwrite(list(terms), file = "terms.txt", sep = ",")

#ANYTHING BELOW IS TOO SLOW FOR THE DATASET

#visualize documents as dendrogram
library(cluster)   
d <- dist(dtm)  
plot(hclust(d))
    
heatmap(as.matrix(dtm))

#visualize words as dendrogram

library(cluster)   
d <- dist(t(dtm))  
plot(hclust(d))

heatmap(as.matrix(dtm))

                                                        
                                                        
#PCA
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
                                                        
res.pca<-PCA(as.matrix(dtm), axes=c(1,2), scale.unit = TRUE)
dimensions<-res.pca$ind$coord
plot(dimensions)
                                                        
dimensions<-data.frame(dimensions)
                                                        
                                                        
plot(dimensions$Dim.1, dimensions$Dim.2)
plot(dimensions$Dim.2, dimensions$Dim.3)
plot(dimensions$Dim.3, dimensions$Dim.4)
plot(dimensions$Dim.4, dimensions$Dim.5)
                                                        
                                                        
                                                        
eigenvalues <- res.pca$eig
eigenvalues<-as.data.frame(eigenvalues)
barplot(eigenvalues$eigenvalue)
barplot(eigenvalues$`percentage of variance`)
                                                        
write.table(output, file="output")
                                                        

#tokenization of engrams

library(tm); library(tau);

tokenize_ngrams <- function(x, n=5) return(rownames(as.data.frame(unclass(textcnt(x,method="string",n=n)))))

text<-tokenize_ngrams(text)

library(data.table) # write terms into file
fwrite(list(text), file = "engrams5.txt", sep = ",")


library(wordcloud)
wordcloud(text, min.freq = 10)
wordcloud(textCorpus, min.freq = 100)
                                                        
#word frequencies
sampletext<-emergingtech$DetailedSummary
wordfreq<-termFreq(sampletext)
wordfreq<-data.frame(wordfreq)
                                                        
library(data.table) # write term freq into file
write.table(wordfreq, file="termfreq.txt")
                                                        
#need to do this with the corpus or cleaned text
                                                        
                                                        
                                                        