#Here I create a single document with the emails that I wanted to preprocess
library(ggplot2)

stats.edges <- read.csv(file="data/sentiment/graph_data_with_manual_sentiment.csv", head=TRUE, sep=",")
stats.sentiment <- read.csv(file="data/sentiment/sentiment_prediction_10733_emails.csv", head=TRUE, sep=",")
# 3: positive, 2:neutral, 1:negative

#1. BARPLOT FOR EDGES IN THE NETWORK - over 67 thousand
#source: http://www.cookbook-r.com/Graphs/Colors_%28ggplot2%29/
edges <- ggplot(stats.edges, aes(x = factor(sentiment),fill=factor(sentiment))) + geom_bar(stat = "count") 
edges.color <- edges + scale_fill_brewer(palette="Paired") + xlab("Sentiment") + ylab("Edges count") 
edges.color + scale_fill_discrete(name="Network\nSentiment", breaks=c("3", "2", "1"), labels=c("Positive", "Neutral", "Negative"))

#2. BARPLOT FOR SENTIMENT IN MESSAGES - only 10 thousand emails
emails <- ggplot(stats.sentiment, aes(x = factor(sentiment), fill=factor(sentiment))) + geom_bar(stat = "count") 
emails.color <- emails + scale_fill_brewer(palette="Paired") + xlab("Sentiment") + ylab("Emails") 
emails.color + scale_fill_discrete(name="Network\nSentiment", breaks=c("3", "2", "1"), labels=c("Positive", "Neutral", "Negative"))

#FINDINGS: Neutral messages are the predominant type in the data, but despite having a lot of negative
#emails and a few positive ones, negative emails were sent to less people than positive ones. 
