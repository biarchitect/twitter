library("data.table")
library("dplyr")
library("ggplot2")



#tweets<-as.data.frame(maml.mapInputPort(1))
tweets<-read.csv(file=filename,header=TRUE)


### Aggregate and plot tweets ###
summaryOfTweets <- group_by(tweets, user) %>% summarise(numberOfTweets = n()) 
nrow(summaryOfTweets)
summaryOfTweets <- arrange(summaryOfTweets, desc(numberOfTweets))
top10<-head(summaryOfTweets, n = 10)

ggplot(top10, aes(x = reorder(user, numberOfTweets), y = numberOfTweets, width = 0.5)) +
  geom_bar(stat = "identity", fill = "grey70", colour = "black") + 
  scale_y_continuous(breaks = seq(0, 30, 2), limits = c(0, 30),
                     expand = c(0, 0)) +
  coord_flip() +
  ggtitle("Top users tweeting the keyword Azure") + 
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey60"),
        plot.title = element_text(face = "bold", size = 12),
        axis.title = element_blank())

## ======================================================================
## Text mining
## ======================================================================
# install.packages("SnowballC")
#install.packages("tm")
library(tm)
library(SnowballC)

myCorpus <- Corpus(VectorSource(tweets$text))
myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
myStopWords <- c(stopwords("english"))
myCorpus <- tm_map(myCorpus, removeWords, myStopWords)

myCorpusCopy <- myCorpus



###############################
tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths=c(1, Inf)))

freq.terms <- findFreqTerms(tdm, lowfreq = 10)
m  <- as.matrix(rowSums(as.matrix(tdm)))
m <- as.data.frame(m, stringsAsFactors = FALSE)
terms <- as.data.frame(row.names(tdm), stringsAsFactors=FALSE)
m <- cbind(terms, m)
m <- arrange(m,desc(V1) )

#maml.mapOutputPort("m")

library(wordcloud)
wordcloud(m[,1], m[,2], random.order = FALSE, random.color = FALSE, scale = c(10, .5), colors = c(colors(),"orange"))
