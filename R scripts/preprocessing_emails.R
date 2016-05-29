library(dplyr) 
library(tm) 
library(SnowballC) 
library(ggplot2)

data <- read.csv(file="data/emails.csv", head=TRUE, sep=",")

#Trying with 10 emails only
docs <-select(data, email) #I had to filter it again because there is some trash in the data
docs.subset <- head(docs,500) #in case I need to try things out
emails<-Corpus(VectorSource(docs$email))

#PREPROCESSING TEXT OPERATIONS
      #To lower case
      myCorpus <- tm_map(emails, content_transformer(tolower))
      
      # remove punctuation
      myCorpus <- tm_map(myCorpus, removePunctuation) 
      
      # remove numbers
      myCorpus <- tm_map(myCorpus, removeNumbers)
      
      # remove URLs
      removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
      myCorpus <- tm_map(myCorpus, content_transformer(removeURL))

      #removing stop words
      myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))

      myStopwords <- c('enron', 'subject', 'www', 'com', 'http', 'https', 'cc', 'message', 'sent', 're', 'ect', 'etc', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'll', 'am', 'pm')
      myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

      #To inspect the corpus anytime I want
for (i in 1:5) {
  cat(paste("[[", i, "]] ", sep = ""))
  #writeLines(myCorpus[[i]])
  writeLines(as.character(processed.corpus[[i]]))
}
      
      #stemming the emails
      myCorpus <- tm_map(myCorpus, stemDocument)
      
      #remove unnecessary white space
      myCorpus <- tm_map(myCorpus, stripWhitespace)
      
      #telling R we preprocessed text documents
      processed.corpus <- tm_map(myCorpus, PlainTextDocument) 
      
      #creating a new dataframe and then joing it with the original data
      texto <- data.frame(text=sapply(processed.corpus, `[[`, "content"), stringsAsFactors=FALSE)
      data$processed_text <- texto$text
      
      #the preprocessed text must be of at least 3 words length
      final.data <- filter(data, (sapply(gregexpr("[A-z]\\W+", data$processed_text), length) + 1L)>=3)
      
      final.data$id<-seq.int(nrow(final.data)) #This is used to add ids to the relationships for gephi

      emails.to.export <- select(final.data, Cod, processed_text)
      #EXPORTING THE DATA ONCE PROCESSED 

write.table(emails.to.export, file="data/clean_emails.csv", row.names=FALSE, sep=",")
