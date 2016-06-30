library("dplyr")
library("ggplot2")


#tweets<-as.data.frame(maml.mapInputPort(1))
filename<-"./data/tweets.csv"
tweets<-read.csv(file=filename,header=TRUE)

summaryOfTweets <- group_by(tweets, user) %>% summarise(numberOfTweets = n()) 
summaryOfTweets <- arrange(summaryOfTweets, desc(numberOfTweets))
top10<-head(summaryOfTweets, n = 10)
top10<-as.data.frame(top10)

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

#maml.mapOutputPort("top10")
