library(dplyr) 

#Here I create a single document with the emails that I wanted to preprocess

clean <- read.csv(file="data/clean_emails.csv", head=TRUE, sep=",")
graph <- read.csv(file="data/graph_with_id.csv", head=TRUE, sep=",")
colnames(graph) <- c('source', 'target', 'label', 'id')

unique_mails <- distinct(select(graph, label)) #10 thousand emails...uhmm a lot
colnames(unique_mails) <- c('Cod')

    #These are the emails that I will really need to preprocess and get the sentiment from
    emails.to.process <- filter(clean, Cod %in% unique_mails$Cod)
    write.table(emails.to.process, file="data/enron_emails_top_users.csv", row.names=FALSE, sep=",")

#NEED TO CREATE A SUBSET OF THESE EMAILS IN ORDER TO ANNOTATE THEM MANUALLY 
#AND TEST A MACHINE LEARNING CLASSIFIER
    subset.emails <- head(emails.to.process,1000)
    write.table(subset.emails, file="data/subset_enron_emails_top_1000_users.csv", row.names=FALSE, sep=",")
    
#FROM HERE I WILL ADD THE MANUAL SENTIMENT
data.textblob.sentiment <- read.csv(file="data/graph_data_with_sentiment.csv", head=TRUE, sep=",")
manual.sentiment <- read.csv(file="data/sentiment/sentiment_prediction_10733_emails.csv", head=TRUE, sep=",")

#Putting the pieces together... first add the sentiment for each email
data.with.sentiment <- cbind(emails.to.process, manual.sentiment$sentiment)

#now I need to put the clean data for the graph with its own sentiment
colnames(graph) <- c('source', 'target', 'Cod', 'id') #here I change the name of the columns so that the inner join works
uniendo <- inner_join(graph, data.with.sentiment, by ='Cod')
colnames(uniendo) <- c('source', 'target', 'label', 'id', 'email', 'sentiment')
enron.data <- select(uniendo, source, target, id, label, sentiment) #just choosing the data I need

write.table(enron.data, file="data/sentiment/graph_data_with_manual_sentiment.csv", row.names=FALSE, sep=",")