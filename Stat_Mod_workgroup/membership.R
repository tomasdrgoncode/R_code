membership<-read.csv("membership.csv")

mytable <- table(membership$center)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls,
    main="Membership") 

wordcloud(membership$modelin_expertise, max.words=30)
wordcloud(membership$regulatory_expertise, max.words=100)